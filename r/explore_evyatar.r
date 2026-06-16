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

# explore.R
# A comprehensive exploration script for the tax & macro dataset
# Run after the initial data loading (df and df_raw already in memory)

# ------------------------------
# 0. Additional libraries
# ------------------------------
library(ggplot2)
library(tidyr)
library(tibble)
library(corrplot)
library(plotly)
library(DT)
library(tseries)
library(forecast)
library(naniar)     # for missing data visualisation
library(skimr)      # compact summary tables

# ------------------------------
# 1. Quick look at both datasets
# ------------------------------
cat("\n===== RAW TAX DATA (df_raw) =====\n")
glimpse(df_raw)
skim(df_raw)

cat("\n===== FULL PANEL (df) =====\n")
glimpse(df)
skim(df)

# ------------------------------
# 2. Harmonise date columns
# ------------------------------
# Both already POSIXct, but ensure they're named "date" for convenience
df_raw <- df_raw %>% rename(date = Date)
df     <- df     %>% rename(date = Date)

# Extract year and month for grouping
df_raw <- df_raw %>% mutate(year = year(date), month = month(date))
df     <- df     %>% mutate(year = year(date), month = month(date))

# ------------------------------
# 3. Missing data analysis
# ------------------------------
# 3a. Overall missingness
cat("\nMissing values in df_raw: ", sum(is.na(df_raw)), "\n")
cat("Missing values in df: ", sum(is.na(df)), "\n")

# 3b. Missing per column in df
missing_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  arrange(desc(n_missing))

print(missing_summary, n = 60)

# Visualise missingness over time for df (naniar)
gg_miss_var(df, show_pct = TRUE) + labs(title = "Missing percentage per variable (df)")

# Plot which months are missing for the first few tax columns
df %>%
  select(date, `Total Gross Income Tax Division`:`Non-profit institution tax`) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  mutate(missing = is.na(value)) %>%
  ggplot(aes(x = date, y = series, fill = missing)) +
  geom_tile() +
  scale_fill_manual(values = c("grey80", "red")) +
  labs(title = "Missing data pattern for tax series in df", x = "", y = "")

# ------------------------------
# 4. Time series plots – tax components (df_raw)
# ------------------------------
tax_vars <- setdiff(names(df_raw), c("date", "year", "month"))

# Convert to long format for ggplot
# Replace the earlier creation of df_raw_long with this version (includes year)
df_raw_long <- df_raw %>%
  select(date, year, all_of(tax_vars)) %>%
  pivot_longer(-c(date, year), names_to = "series", values_to = "value")

# Faceted line plots (raw values)
ggplot(df_raw_long, aes(x = date, y = value)) +
  geom_line(color = "steelblue", linewidth = 0.3) +
  facet_wrap(~ series, scales = "free_y", ncol = 3) +
  labs(title = "Monthly tax revenue components (df_raw)", x = "", y = "Value (NIS ths.?)") +
  theme_minimal()

# Optional: interactive version with plotly
# ggplotly()

# ------------------------------
# 5. Histograms / densities for tax variables
# ------------------------------
df_raw_long %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "steelblue", bins = 30, alpha = 0.7) +
  facet_wrap(~ series, scales = "free") +
  labs(title = "Distributions of tax series (df_raw)") +
  theme_minimal()

# Or boxplots by year to see seasonality / trends
df_raw_long %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = value)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~ series, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Yearly distributions of tax components")

df_raw_long %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = year, y = value)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~ series, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Yearly distributions of tax components")

# ------------------------------
# 6. Summary statistics table (df_raw)
# ------------------------------
tax_summary <- df_raw %>%
  select(all_of(tax_vars)) %>%
  skim() %>%
  as_tibble() %>%
  select(skim_variable, numeric.mean, numeric.sd, numeric.p0, 
         numeric.p25, numeric.p50, numeric.p75, numeric.p100, n_missing)

datatable(tax_summary, caption = "Tax series summary statistics")


# ------------------------------
# 7. Correlation analysis – tax components
# ------------------------------
tax_cor <- cor(df_raw %>% select(all_of(tax_vars)), use = "complete.obs")

corrplot(tax_cor, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.6, 
         title = "Correlation matrix of tax components",
         mar = c(0,0,2,0))

# Interactive heatmap
plot_ly(z = tax_cor, x = colnames(tax_cor), y = rownames(tax_cor),
        type = "heatmap", colors = colorRamp(c("blue", "white", "red")))

# ------------------------------
# 8. Stationarity checks (ADF test)
# ------------------------------
adf_results <- map_dfr(tax_vars, function(var) {
  series <- df_raw[[var]]
  # remove any NA just in case
  series <- series[!is.na(series)]
  if(length(series) < 10) return(NULL)
  test <- adf.test(series, alternative = "stationary")
  tibble(variable = var, adf_stat = test$statistic, p_value = test$p.value)
})

datatable(adf_results, caption = "Augmented Dickey-Fuller test (levels)")

# ------------------------------
# 9. Seasonal decomposition for one key series (example: Total Income Tax Division Net)
# ------------------------------
main_series <- df_raw %>%
  select(date, `Total Income Tax Division Net`) %>%
  drop_na()

# Create a ts object (frequency = 12)
start_year <- year(min(main_series$date))
start_month <- month(min(main_series$date))
ts_net <- ts(main_series$`Total Income Tax Division Net`, 
             start = c(start_year, start_month), frequency = 12)

# STL decomposition
stl_decomp <- stl(ts_net, s.window = "periodic")
autoplot(stl_decomp) + labs(title = "STL decomposition – Total Income Tax Division Net")

# ------------------------------
# 10. Explore macro variables in df
# ------------------------------
# Identify columns that are not the 12 tax ones (those are fully covered by df_raw)
tax_col_names <- tax_vars
macro_vars <- setdiff(names(df), c("date", "year", "month", tax_col_names))

# Long format for macro series
df_macro_long <- df %>%
  select(date, all_of(macro_vars)) %>%
  pivot_longer(-date, names_to = "series", values_to = "value")

# Faceted time series (only series with at least some data)
ggplot(df_macro_long %>% filter(!is.na(value)), aes(x = date, y = value)) +
  geom_line(color = "darkorange", linewidth = 0.3) +
  facet_wrap(~ series, scales = "free_y", ncol = 4) +
  labs(title = "Macro-financial indicators (df)", x = "", y = "") +
  theme_minimal()

# Histograms of macro variables
df_macro_long %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "darkorange", bins = 30, alpha = 0.7) +
  facet_wrap(~ series, scales = "free") +
  labs(title = "Distributions of macro series") +
  theme_minimal()

# Summary table for macro variables
macro_summary <- df %>%
  select(all_of(macro_vars)) %>%
  skim() %>%
  as_tibble() %>%
  select(skim_variable, numeric.mean, numeric.sd, numeric.p0, 
         numeric.p25, numeric.p50, numeric.p75, numeric.p100, n_missing)

datatable(macro_summary, caption = "Macro series summary statistics")

# ------------------------------
# 11. Correlation between tax (df_raw) and macro (df) – align by date
# ------------------------------
# Merge tax data from df_raw with macro from df on date
combined <- df_raw %>%
  select(date, all_of(tax_vars)) %>%
  left_join(df %>% select(date, all_of(macro_vars)), by = "date")

# Correlations (tax vs macro)
tax_macro_cor <- cor(
  combined %>% select(all_of(tax_vars)),
  combined %>% select(all_of(macro_vars)),
  use = "pairwise.complete.obs"
)

# Heatmap
plot_ly(z = tax_macro_cor, 
        x = colnames(tax_macro_cor), 
        y = rownames(tax_macro_cor),
        type = "heatmap", 
        colors = colorRamp(c("blue", "white", "red"))) %>%
  layout(title = "Correlation: Tax components (rows) vs Macro indicators (cols)")

# ------------------------------
# 12. Outlier detection (IQR method) on tax series
# ------------------------------
outlier_list <- map_dfr(tax_vars, function(var) {
  vals <- df_raw[[var]]
  Q1 <- quantile(vals, 0.25, na.rm = TRUE)
  Q3 <- quantile(vals, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  outlier_idx <- which(vals < lower | vals > upper)
  if(length(outlier_idx) > 0) {
    tibble(variable = var,
           date = df_raw$date[outlier_idx],
           value = vals[outlier_idx],
           bound = ifelse(vals[outlier_idx] < lower, "low", "high"))
  } else NULL
})

if(nrow(outlier_list) > 0) {
  datatable(outlier_list, caption = "Outliers in tax series (1.5*IQR rule)")
}

# ------------------------------
# 13. Interactive exploration (optional, use with small subsets)
# ------------------------------
# Plotly dynamic time series for selected tax variables
p <- df_raw %>%
  select(date, `Total Gross Income Tax Division`, `Total Income Tax Division Net`) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  ggplot(aes(x = date, y = value, color = series)) +
  geom_line() + theme_minimal()
ggplotly(p)

# Datatable of the raw data with filters
datatable(df_raw, filter = 'top', options = list(pageLength = 15))

cat("\n=== Exploration script completed ===")
cat("\nCheck the generated plots and tables in the RStudio viewer.\n")



### Part 2 Deep dive in ####


# Choose one variable to study
var_name <- "Total Gross Income Tax Division"

# ----- 1. Quick numerical snapshot -----
df_raw %>%
  select(date, value = all_of(var_name)) %>%
  drop_na() %>%
  summarise(
    n_obs      = n(),
    first_date = min(date),
    last_date  = max(date),
    min_val    = min(value),
    q25        = quantile(value, 0.25),
    median     = median(value),
    mean       = mean(value),
    q75        = quantile(value, 0.75),
    max_val    = max(value),
    sd         = sd(value)
  ) %>%
  glimpse()

df_raw %>%
  select(date, value = all_of(var_name)) %>%
  drop_na() %>%
  ggplot(aes(x = date, y = value)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_smooth(se = FALSE, color = "red") +
  labs(title = paste("Time series:", var_name), y = "Value (NIS thousands?)") +
  theme_minimal()

df_raw %>%
  select(value = all_of(var_name)) %>%
  drop_na() %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "steelblue", bins = 30, alpha = 0.7) +
  labs(title = paste("Distribution:", var_name)) +
  theme_minimal()

df_raw %>%
  select(date, `Total Gross Income Tax Division`, `Total refunds from the Income Tax Department`) %>%
  drop_na() %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  ggplot(aes(x = date, y = value, color = series)) +
  geom_line(alpha = 0.8) +
  labs(title = "Gross income tax vs. refunds", y = "Value") +
  theme_minimal()


var2 <- "Total refunds from the Income Tax Department"

df_raw %>%
  select(date, value = all_of(var2)) %>%
  drop_na() %>%
  summarise(
    n_obs = n(),
    min = min(value),
    q25 = quantile(value, 0.25),
    median = median(value),
    mean = mean(value),
    q75 = quantile(value, 0.75),
    max = max(value),
    sd = sd(value)
  ) %>% glimpse()

df_raw %>%
  ggplot(aes(x = date, y = .data[[var2]])) +
  geom_line(color = "darkred") +
  labs(title = var2) +
  theme_minimal()

df_raw %>%
  select(date, gross = `Total Gross Income Tax Division`, refunds = `Total refunds from the Income Tax Department`) %>%
  mutate(refunds_abs = -refunds) %>%
  ggplot(aes(x = gross, y = refunds_abs)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Gross income vs. absolute refunds", x = "Gross income", y = "Refunds (absolute)") +
  theme_minimal()


df_net <- df_raw %>%
  mutate(net_income_tax = `Total Gross Income Tax Division` + `Total refunds from the Income Tax Department`)

# Compare all three
df_net %>%
  select(date, gross = `Total Gross Income Tax Division`, 
         refunds = `Total refunds from the Income Tax Department`, 
         net = net_income_tax) %>%
  pivot_longer(-date, names_to = "series", values_to = "value") %>%
  ggplot(aes(x = date, y = value, color = series)) +
  geom_line() +
  labs(title = "Gross, refunds, and net income tax", y = "NIS thousands") +
  theme_minimal()
