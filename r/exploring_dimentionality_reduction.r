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
# ------------------------------
# 1. Prepare the data
# ------------------------------
# Choose which dataset to reduce. I'll start with df (full panel).
# But PCA needs complete rows. Let's use only variables with >70% non-NA
# and keep rows with no NAs in those variables.

pca_data <- df %>%
  select(-date, -year, -month) %>%        # remove date columns
  select(where(~sum(!is.na(.)) > 0.7 * nrow(df)))  # keep well-covered vars

# How many variables left?
ncol(pca_data)  # hopefully 20-30

# Remove any row that still has NA (for PCA)
pca_data_complete <- pca_data[complete.cases(pca_data), ]
cat("Rows used:", nrow(pca_data_complete), "out of", nrow(df), "\n")

# Scale and center (essential for PCA)
pca_result <- prcomp(pca_data_complete, scale. = TRUE, center = TRUE)

# ------------------------------
# 2. How many components to keep?
# ------------------------------
# Scree plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
  labs(title = "Variance explained by each principal component")

# Cumulative variance
summary(pca_result)$importance[, 1:10]


# 3. Loadings: which variables define each component?
loadings <- as.data.frame(pca_result$rotation)
loadings$variable <- rownames(loadings)

# Top contributors to PC1 (largest absolute loading)
loadings %>%
  select(variable, PC1, PC2, PC3) %>%
  arrange(desc(abs(PC1))) %>%
  head(10)

# Visualise variable contributions
fviz_pca_var(pca_result, 
             col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE) +
  labs(title = "Variable contribution to PC1 and PC2")


pca_scores <- as.data.frame(pca_result$x)
pca_scores$date <- pca_data_complete$date  # you need to keep date from the complete rows

pca_scores %>%
  select(date, PC1, PC2, PC3) %>%
  pivot_longer(-date, names_to = "component", values_to = "score") %>%
  ggplot(aes(x = date, y = score, color = component)) +
  geom_line() +
  labs(title = "Principal components over time",
       y = "Standardized score") +
  theme_minimal()

####
# Re-run PCA while keeping dates
vars_to_use <- df %>%
  select(-date, -year, -month) %>%
  select(where(~sum(!is.na(.)) > 0.7 * nrow(df))) %>%
  names()

pca_data <- df %>% select(date, all_of(vars_to_use))
pca_data_complete <- pca_data[complete.cases(pca_data), ]

# Separate date for later
dates <- pca_data_complete$date
pca_matrix <- pca_data_complete %>% select(-date) %>% scale(center = TRUE, scale = TRUE)

pca_result <- prcomp(pca_matrix, center = FALSE, scale = FALSE)  # already scaled

# Now extract scores
pca_scores <- as.data.frame(pca_result$x)
pca_scores$date <- dates

# Plot PC1, PC2, PC3 over time
pca_scores %>%
  select(date, PC1, PC2, PC3) %>%
  pivot_longer(-date, names_to = "component", values_to = "score") %>%
  ggplot(aes(x = date, y = score, color = component)) +
  geom_line() +
  labs(title = "Principal components over time (aligned dates)",
       y = "Standardized score") +
  theme_minimal()

# Correlation-based distance
var_cor <- cor(pca_data_complete %>% select(-date), use = "pairwise")
dist_mat <- as.dist(1 - abs(var_cor))
hc <- hclust(dist_mat, method = "ward.D2")
plot(hc, main = "Variable clustering", cex = 0.6)


tax_pca <- prcomp(df_raw %>% select(-date, -year, -month), scale. = TRUE)
summary(tax_pca)$importance[,1:5]