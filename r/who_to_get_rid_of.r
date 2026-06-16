# Load required packages
library(FactoMineR)  # PCA with missing data handling
library(factoextra)  # Nice visualizations
library(tidyverse)
library(readxl)
library(dplyr)
library(zoo)
library(purrr)
library(xts)
library(lubridate)
library(vars)
library(openxlsx)

df <- read_excel("data/clean/combined_monthly_panel_Q_refined.xlsx")
df_raw <- read_excel("data/raw/nowcasting_data_raw.xlsx")
str(df)
str(df_raw)
# Both already POSIXct, but ensure they're named "date" for convenience
df_raw <- df_raw %>% rename(date = Date)
df     <- df     %>% rename(date = Date)

# Extract year and month for grouping
df_raw <- df_raw %>% mutate(year = year(date), month = month(date))
df     <- df     %>% mutate(year = year(date), month = month(date))

# ============================================================
# INTERPRETABLE COLUMN FILTERING (white box)
# ============================================================

# Copy the full df (we'll keep df_raw as a clean reference)
df_full <- df

# 1. Remove obvious non‑numeric helpers (but keep date for later alignment)
df_work <- df_full %>%
  select(-year, -month)   # we can always re‑extract from date

# 2. Drop columns with >80% missing values
missing_frac <- df_work %>%
  summarise(across(-date, ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "frac_na")

high_na_vars <- missing_frac %>% filter(frac_na > 0.8) %>% pull(var)
cat("Dropping", length(high_na_vars), "columns with >80% NA:\n")
print(high_na_vars)

df_work <- df_work %>% select(-all_of(high_na_vars))

# 3. Drop columns with near‑zero variance (constant or almost constant)
# We need to handle NAs: temporarily impute with median to compute variance
library(caret)
numeric_vars <- df_work %>% select(-date) %>% names()

# Function to compute variance ignoring NAs
var_ignore_na <- function(x) var(x, na.rm = TRUE)

# Compute coefficient of variation (CV = sd/mean) for each numeric column
cv_values <- df_work %>%
  summarise(across(all_of(numeric_vars), 
                   list(cv = ~ sd(., na.rm = TRUE) / abs(mean(., na.rm = TRUE))))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "cv") %>%
  mutate(var = gsub("_cv$", "", var))

low_cv_vars <- cv_values %>% filter(cv < 0.01 | is.na(cv) | is.infinite(cv)) %>% pull(var)
cat("\nDropping", length(low_cv_vars), "columns with CV < 0.01 (almost constant):\n")
print(low_cv_vars)

df_work <- df_work %>% select(-all_of(low_cv_vars))

# 4. Drop highly correlated duplicates (cor > 0.99)
# Use only complete cases for correlation
df_cor <- df_work %>% select(-date) %>% na.omit()
if(ncol(df_cor) > 1 && nrow(df_cor) > 1) {
  cor_mat <- cor(df_cor, use = "pairwise.complete.obs")
  high_cor_pairs <- which(abs(cor_mat) > 0.99 & upper.tri(cor_mat), arr.ind = TRUE)
  
  to_drop <- c()
  for(i in 1:nrow(high_cor_pairs)) {
    var1 <- rownames(cor_mat)[high_cor_pairs[i, 1]]
    var2 <- colnames(cor_mat)[high_cor_pairs[i, 2]]
    # Drop the one with more missing values (or if tie, the one with lower variance)
    miss1 <- mean(is.na(df_work[[var1]]))
    miss2 <- mean(is.na(df_work[[var2]]))
    if(miss1 >= miss2) to_drop <- c(to_drop, var1) else to_drop <- c(to_drop, var2)
  }
  to_drop <- unique(to_drop)
  cat("\nDropping", length(to_drop), "columns because they are >99% correlated with another column:\n")
  print(to_drop)
  df_work <- df_work %>% select(-all_of(to_drop))
} else {
  cat("\nNot enough complete data to compute correlations.\n")
}

# 5. Final cleaned dataset
df_clean <- df_work
cat("\n=== FINAL REDUCTION ===\n")
cat("Original columns (excluding date):", ncol(df_full) - 1, "\n")
cat("Columns after filtering:", ncol(df_clean) - 1, "\n")
cat("Percentage reduction:", round(100 * (1 - (ncol(df_clean)-1)/(ncol(df_full)-1)), 1), "%\n")

# Show the remaining variables
cat("\nRemaining variables:\n")
print(names(df_clean)[-1])















































# ============================================================
# COLUMN REDUCTION – GDP ANCHOR, DROP LOW OVERLAP
# ============================================================

library(tidyverse)

df_work <- df %>% select(-year, -month)

anchor_var <- "GDP"
cv_threshold <- 0.05          # drop if CV < 0.05
min_overlap <- 30             # minimum overlapping months with GDP (GDP has 123 obs, so 30 is ~25%)
cor_threshold <- 0.15         # drop if |cor(GDP)| <= 0.15 AND overlap >= min_overlap

all_cols <- names(df_work)[!names(df_work) %in% c("date", anchor_var)]
keep_cols <- c(anchor_var)
drop_log <- data.frame(variable = character(), reason = character(), 
                       cv = numeric(), cor_with_GDP = numeric(), 
                       overlap = integer(), stringsAsFactors = FALSE)

for (col in all_cols) {
  col_data <- df_work[[col]]
  gdp_data <- df_work[[anchor_var]]
  
  # 1. CV (coefficient of variation)
  mean_val <- mean(col_data, na.rm = TRUE)
  sd_val <- sd(col_data, na.rm = TRUE)
  if (is.na(mean_val) || mean_val == 0 || is.na(sd_val)) {
    cv <- NA
  } else {
    cv <- sd_val / abs(mean_val)
  }
  
  # 2. Overlap with GDP
  overlap <- sum(!is.na(col_data) & !is.na(gdp_data))
  
  # 3. Correlation with GDP (only if overlap >= min_overlap)
  if (overlap >= min_overlap) {
    complete_idx <- !is.na(col_data) & !is.na(gdp_data)
    cor_val <- cor(col_data[complete_idx], gdp_data[complete_idx])
  } else {
    cor_val <- NA
  }
  
  # 4. Decision logic
  drop_reason <- NULL
  
  if (!is.na(cv) && cv < cv_threshold) {
    drop_reason <- sprintf("CV = %.4f (< %.2f)", cv, cv_threshold)
  } else if (overlap < min_overlap) {
    drop_reason <- sprintf("Overlap with GDP = %d (< %d)", overlap, min_overlap)
  } else if (!is.na(cor_val) && abs(cor_val) <= cor_threshold) {
    drop_reason <- sprintf("|cor(GDP)| = %.4f (<= %.2f), overlap = %d", 
                           abs(cor_val), cor_threshold, overlap)
  } else {
    # Kept: CV >= threshold AND overlap >= threshold AND |cor| > threshold
    keep_cols <- c(keep_cols, col)
    next
  }
  
  drop_log <- rbind(drop_log, data.frame(
    variable = col,
    reason = drop_reason,
    cv = ifelse(is.na(cv), NA, round(cv, 4)),
    cor_with_GDP = ifelse(is.na(cor_val), NA, round(cor_val, 4)),
    overlap = overlap,
    stringsAsFactors = FALSE
  ))
}

df_clean <- df_work %>% select(date, all_of(keep_cols))

cat("\n========== DROP REPORT ==========\n")
print(drop_log, row.names = FALSE)
cat("\n========== KEPT COLUMNS ==========\n")
cat("Kept", length(keep_cols), "columns:\n")
print(keep_cols)
cat("\n========== SUMMARY ==========\n")
cat("Original columns (excl date):", length(all_cols) + 1, "\n")
cat("Kept:", length(keep_cols), "\n")
cat("Dropped:", nrow(drop_log), "\n")




# Extract year‑month, then count overlaps
df_ym <- df %>%
  mutate(ym = floor_date(date, "month"))  # or use year(date) and month(date)

overlap_table_ym <- df_ym %>%
  summarise(across(-c(date, year, month, ym), ~ sum(!is.na(.) & !is.na(GDP)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "overlap_with_GDP") %>%
  arrange(desc(overlap_with_GDP))

print(overlap_table_ym, n = 60)


# Create overlap table: number of rows where both variable and GDP are non‑NA
overlap_table <- df %>%
  summarise(across(-c(date, year, month), ~ sum(!is.na(.) & !is.na(GDP)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "overlap_with_GDP") %>%
  arrange(desc(overlap_with_GDP))

print(overlap_table, n = 60)
