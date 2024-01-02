rm(list = ls())
###############################################################################
################################# Load Packages ###############################
###############################################################################
### Seed for reproducibility
seed <- 123 #as.numeric(Sys.getenv("SEED"))
set.seed(seed)
### For regression
library(glmnet)
### Used for tsboot bootstrap
library(boot)
### Determines blocklength with b.star
library(np)
### Dplyr as well as purrr
library(plyr)
library(tidyverse)
### ODE solver
library(deSolve)
### Smoothing
library(signal)
### Parallel processing
library(doParallel)
### Use Python for ODE solver
library(reticulate)
###############################################################################
################################# Load Functions ##############################
###############################################################################
source_python("~/lorenz_ode.py")
source("~/sg_optimal_combination.R")
###############################################################################
################################ Generate Data ################################
###############################################################################
start <- 1 #Sys.getenv("START")
end <- 61 #Sys.getenv("END")
by <- 3 #Sys.getenv("BY")
num_init <- 1 #as.numeric(Sys.getenv("NUM_INIT"))
s <- rep(c(seq(as.numeric(start),
               as.numeric(end),
               by = as.numeric(by))),
         each = num_init)
num_state_var <- 3
init_sequence <- list()
for (i in 1:num_init) {
  x <- runif(1, min = -15, max = 15) # x1 initial condition bounds
  y <- runif(1, min = -15, max = 15) # x2 initial condition bounds
  z <- runif(1, min = 10, max = 40) # x3 initial condition bounds
  init_coniditons <- c(x, y, z)
  init_sequence[[i]] <- init_coniditons
}
init_sequence <- rep(init_sequence, times = (length(s) / num_init), each = 1)
n_obs <- 5000 #Sys.getenv("N_OBS") # n = 7000
monomial_degree <- 5 # theta matrix
dt <- 0.001 # time-step
systems <- list()
for (i in seq_along(init_sequence)) {
  systems[[i]] <-
    lorenz_ode(
      n = as.numeric(n_obs),
      dt = dt,
      init_conditions = init_sequence[[i]],
      snr = s[[i]]
    )
}
x_t <- systems
# 80/20 split
x_t_train <- list()
for (i in seq_along(x_t)) {
  x_t_train[[i]] <- x_t[[i]][1:round((nrow(x_t[[i]])*0.8), 0), ]
}
###############################################################################
############################### Generate Combinations #########################
################################# Define methods ##############################
###############################################################################
### Test each x_t
xt_1 <- list()
for (i in seq_along(x_t_train)) {
  xt_1[[i]] <- as.matrix(x_t_train[[i]][, 1])
}
xt_2 <- list()
for (i in seq_along(x_t_train)) {
  xt_2[[i]] <- as.matrix(x_t_train[[i]][, 2])
}
xt_3 <- list()
for (i in seq_along(x_t_train)) {
  xt_3[[i]] <- as.matrix(x_t_train[[i]][, 3])
}
lorenz_sg_combinations_x1 <- lapply(X = xt_1,
                                    FUN = sg_optimal_combination,
                                    dt = dt)
lorenz_porder_wl_combos_x1 <- sapply(lorenz_sg_combinations_x1,
                                     function(x)
                                       x[2])
lorenz_min_fdist_x1 <- sapply(lorenz_sg_combinations_x1,
                                     function(x)
                                       min(x[3]$f_dist))
lorenz_sg_combinations_x2 <- lapply(X = xt_2,
                                    FUN = sg_optimal_combination,
                                    dt = dt)
lorenz_porder_wl_combos_x2 <- sapply(lorenz_sg_combinations_x2,
                                     function(x)
                                       x[2])
lorenz_min_fdist_x2 <- sapply(lorenz_sg_combinations_x2,
                              function(x)
                                min(x[3]$f_dist))
lorenz_sg_combinations_x3 <- lapply(X = xt_3,
                                    FUN = sg_optimal_combination,
                                    dt = dt)
lorenz_porder_wl_combos_x3 <- sapply(lorenz_sg_combinations_x3,
                                     function(x)
                                       x[2])
lorenz_min_fdist_x3 <- sapply(lorenz_sg_combinations_x3,
                              function(x)
                                min(x[3]$f_dist))

library(tibble)
library(tidyr)
s_new <- s

df <- tibble(
  x = s_new,  # Assuming all vectors are of the same length
  lorenz_min_fdist_x1 = lorenz_min_fdist_x1,
  lorenz_min_fdist_x2 = lorenz_min_fdist_x2,
  lorenz_min_fdist_x3 = lorenz_min_fdist_x3
)

df_long <- df %>% 
  gather(key = "variable", value = "value", -x)

library(ggplot2)
library(latex2exp)
x_labels <- x_breaks <- seq(1, 61, by = 6)

library(ggh4x)
# Calculate the minimum and maximum values from the data
y_min <- min(df_long$value, na.rm = TRUE)
y_max <- max(df_long$value, na.rm = TRUE)
# Adjusting them slightly for the log scale
y_min_adj <- y_min * 0.9  # Adjusting to 90% of the minimum
y_max_adj <- y_max * 1.1  # Adjusting to 110% of the maximum


sg_comparison_plot <- ggplot(df_long, aes(
  x = x,
  y = value,
  color = variable
)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1) +
  scale_x_continuous(limits = c(min(x_breaks),
                                max(x_breaks)),
                     labels = x_labels,
                     breaks = x_breaks) +
  labs(
    title = "Lorenz System",
    x = "SNR",
    y = TeX("MSE"),
    color = "Variables"  # Change the legend title here
  ) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x)
      10 ^ x),
    labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
  ) +
  coord_cartesian(ylim = c(10^-6, 10^2)) +
  scale_color_manual(
    values = c(
      "lorenz_min_fdist_x1" = "#cb6a49",
      "lorenz_min_fdist_x2" = "#a46cb7",
      "lorenz_min_fdist_x3" = "#7aa457"
    ),
    labels = c(TeX("$x_1(t)$"),
               TeX("$x_2(t)$"),
               TeX("$x_3(t)$"))
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    axis.title.y = element_text(size = 28),
    axis.title.x = element_text(size = 28),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 32),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black")
  )

sg_comparison_plot
ggsave(sg_comparison_plot, filename = '~/Figures/additional_figs/sg_filter_comparison_lorenz.pdf',
       width = 13, height = 7, dpi = 300)



legend_plot <- ggplot(df_long, aes(
  x = x,
  y = log10(value),
  color = variable
)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1) +
  scale_x_continuous(limits = c(min(x_breaks),
                                max(x_breaks)),
                     labels = x_labels,
                     breaks = x_breaks) +
  labs(
    title = "Lorenz System",
    x = "SNR",
    y = TeX("MSE ($log_{10})$"),
    color = "State-space variable"  # Change the legend title here
  ) +
  scale_y_continuous(limits = c(-20, 5)) +
  scale_color_manual(
    values = c(
      "lorenz_min_fdist_x1" = "#cb6a49",
      "lorenz_min_fdist_x2" = "#a46cb7",
      "lorenz_min_fdist_x3" = "#7aa457"
    ),
    labels = c(TeX("$x_1(t)$"),
               TeX("$x_2(t)$"),
               TeX("$x_3(t)$"))
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16, face = "italic"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black")
  )

legend <- get_legend(legend_plot)

cowplot::plot_grid(legend)
ggsave(cowplot::plot_grid(legend), filename = '~/Figures/additional_figs/sg_filter_comparison_legend.pdf',
       width = 5, height = 5, dpi = 300)
