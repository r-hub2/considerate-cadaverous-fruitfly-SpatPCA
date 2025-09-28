## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(SpatPCA)
library(ggplot2)
base_theme <- theme_classic(base_size = 18, base_family = "Times")

## -----------------------------------------------------------------------------
set.seed(1024)
position <- matrix(seq(-5, 5, length = 100))
true_eigen_fn <- exp(-position^2) / norm(exp(-position^2), "F")

plot_df <- data.frame(position = position, eigenfunction = true_eigen_fn)

ggplot(plot_df, aes(position, eigenfunction)) +
  geom_line() +
  base_theme

## -----------------------------------------------------------------------------
realizations <- rnorm(n = 100, sd = 20) %*% t(true_eigen_fn) + matrix(rnorm(n = 100 * 100), 100, 100)

## -----------------------------------------------------------------------------
subset_idx <- seq(1, 100, length.out = 9)
matplot(
  t(realizations[subset_idx, ]), type = "l", lty = 1,
  ylim = c(-10, 10),
  xlab = "position index", ylab = "realization"
)

## -----------------------------------------------------------------------------
cv <- spatpca(x = position, Y = realizations)
eigen_est <- cv$eigenfn

## -----------------------------------------------------------------------------
plot_df <- data.frame(
  position = position,
  true = true_eigen_fn,
  spatpca = eigen_est[, 1],
  pca = svd(realizations)$v[, 1]
)

plot_df_long <- data.frame(
  position = rep(plot_df$position, 3),
  estimate = rep(c("true", "spatpca", "pca"), each = nrow(plot_df)),
  eigenfunction = c(plot_df$true, plot_df$spatpca, plot_df$pca)
)

ggplot(plot_df_long, aes(x = position, y = eigenfunction, color = estimate)) +
  geom_line() +
  base_theme

## -----------------------------------------------------------------------------
realizations <- rnorm(n = 100, sd = 3) %*% t(true_eigen_fn) + matrix(rnorm(n = 100 * 100), 100, 100)

## -----------------------------------------------------------------------------
subset_idx <- seq(1, 100, length.out = 9)
matplot(
  t(realizations[subset_idx, ]), type = "l", lty = 1,
  ylim = c(-10, 10),
  xlab = "position index", ylab = "realization"
)

## -----------------------------------------------------------------------------
cv <- spatpca(x = position, Y = realizations)
eigen_est <- cv$eigenfn

plot_df <- data.frame(
  position = position,
  true = true_eigen_fn,
  spatpca = eigen_est[, 1],
  pca = svd(realizations)$v[, 1]
)

plot_df_long <- data.frame(
  position = rep(plot_df$position, 3),
  estimate = rep(c("true", "spatpca", "pca"), each = nrow(plot_df)),
  eigenfunction = c(plot_df$true, plot_df$spatpca, plot_df$pca)
)

ggplot(plot_df_long, aes(x = position, y = eigenfunction, color = estimate)) +
  geom_line() +
  base_theme

