# Sparsity analysis for df (the large panel)
library(Matrix)
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Overall missing rate
total_cells <- nrow(df) * ncol(df)
missing_cells <- sum(is.na(df))
cat(sprintf("Overall missing rate: %.1f%% (%d / %d cells)\n", 
            100 * missing_cells / total_cells, missing_cells, total_cells))

# 2. Missing rate per variable (sorted)
missing_per_var <- df %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "frac_missing") %>%
  arrange(frac_missing)

ggplot(missing_per_var, aes(x = reorder(variable, frac_missing), y = frac_missing)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Fraction of missing values per variable", y = "Missing proportion", x = "") +
  theme_minimal()

# 3. How many complete rows (no NAs at all)?
complete_rows <- sum(complete.cases(df))
cat(sprintf("Complete rows (no missing anywhere): %d out of %d (%.1f%%)\n", 
            complete_rows, nrow(df), 100 * complete_rows / nrow(df)))

# 4. For numeric variables (excluding date), count observations after dropping NAs
obs_per_var <- df %>%
  select(-date, -year, -month) %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_obs") %>%
  arrange(n_obs)

head(obs_per_var, 10)  # variables with fewest observations
tail(obs_per_var, 10)  # variables with most observations

# 5. Effective rank (numerical rank of the data matrix after imputing NAs with 0)
# This tells you how many truly independent dimensions exist.
# We'll use a simple SVD on the non‑missing part – but must handle NAs.
# Option: use only complete cases (may lose many rows)
df_complete_cases <- df[complete.cases(df %>% select(-date, -year, -month)), ]
if(nrow(df_complete_cases) > ncol(df_complete_cases)) {
  svd_res <- svd(scale(df_complete_cases %>% select(-date, -year, -month)))
  effective_rank <- sum(svd_res$d > 1e-6)  # singular values > tolerance
  cat("Effective rank (based on complete cases):", effective_rank, 
      "out of", ncol(df_complete_cases) - 3, "variables\n")
} else {
  cat("Not enough complete rows to compute rank.\n")
}




# Impute with median (for numeric columns only)
df_imp <- df %>%
  select(-date, -year, -month) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Now compute SVD on the centered/scaled matrix
mat_scaled <- scale(df_imp)
svd_res <- svd(mat_scaled)
singular_vals <- svd_res$d
variance_explained <- singular_vals^2 / sum(singular_vals^2)

# Effective rank: number of components needed to explain 90% variance
cum_var <- cumsum(variance_explained)
k90 <- which(cum_var >= 0.9)[1]
cat("Effective rank (90% variance):", k90, "out of", ncol(df_imp), "variables\n")

# Plot scree
plot(variance_explained[1:20], type = "b", 
     xlab = "Principal component", ylab = "Proportion of variance",
     main = "Scree plot after median imputation")

# Impute missing with median
df_imp <- df %>%
  select(-date, -year, -month) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Center and scale
X <- scale(df_imp)

# Covariance matrix
S <- cov(X)

# Volume of the confidence ellipsoid (up to a constant)
log_volume <- 0.5 * log(det(S))   # log of the hypervolume
cat("Log determinant of covariance:", log_volume, "\n")

# Compute average pairwise Euclidean distance (on complete cases only)
complete_rows <- which(complete.cases(X))
if(length(complete_rows) > 1) {
  X_complete <- X[complete_rows, ]
  n_pts <- nrow(X_complete)
  # Sample to avoid O(n^2) (n=~100 maybe)
  set.seed(123)
  sample_idx <- sample(1:n_pts, min(200, n_pts))
  dist_mat <- dist(X_complete[sample_idx, ])
  mean_dist <- mean(dist_mat)
  max_dist <- max(dist_mat)
  ratio <- mean_dist / max_dist
  cat("Mean distance / max distance ratio:", ratio, "\n")
}
