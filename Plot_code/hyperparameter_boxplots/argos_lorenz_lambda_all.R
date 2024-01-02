rm(list = ls())
library(tidyr)
library(tidyverse)
library(scales)
library(latex2exp)
library(cowplot)
library(ggh4x)
library(ggpubr)
library(gridExtra)
library(patchwork)
library(reticulate)
library(RColorBrewer)
library(stringr)
library(gridExtra)
# Load the .RData file back into the R workspace
load("~/all_combined_df_increasing_snr_lambda.RData")
load("~/all_combined_df_increasing_n_lambda.RData")

colors_plot <- c("#a3d5d3",
                 "#cfbcd5",
                 "#d6cdae")
x_axis_breaks_n <- c("2", "2.5", "3", "3.5", "4", "4.5", "5")
x_axis_labels_n <- c(expression(10^2), expression(10^{2.5}), expression(10^3),
                   expression(10^{3.5}), expression(10^4), expression(10^{4.5}),
                   expression(10^5))

x_axis_breaks_snr <- c(1, 13, 25, 37, 49, 61, 73)
x_axis_labels_snr <- c(1, 13, 25, 37, 49, 61, expression(infinity))

plot_title <- 25

equation_labels <- my_expressions <- c(expression(dot(x)[1]),
                                       expression(dot(x)[2]),
                                       expression(dot(x)[3]))
x_axis_text_size <- 20
y_axis_text_size <- 20
y_axis_label_size <- 24
x_axis_label_size <- 24
# a simple function to help make the segments
create_separators <- function(x, extra_x, y, extra_y, angle = 45, scale = 1, length = .1){
  add_y <-  length * sin(angle * pi/180) /2
  add_x <- length * cos(angle * pi/180) 
  list(x = x - add_x*scale, xend = x + add_x*scale + extra_x, 
       y = rep(y - add_y*scale - extra_y, length(x)), yend = rep(y + add_y*scale - extra_y/2, length(x)))
}
# Lasso ----
# Increasing N ----
lasso_n_lambda <- all_combined_df_increasing_n_lambda %>%
  dplyr::filter(Model == "Lasso")
min_y_lasso_n <- 10^-5
max_y_lasso_n <- 1
lasso_n_plot <-
  ggplot(lasso_n_lambda, aes(x = n_value, y = lambda, fill = data_source)) +
  geom_boxplot() +
  ggtitle("ARGOS-Lasso") +
  xlab("n") +
  ylab(expression(lambda[L]^"*")) +
  scale_x_discrete(labels = x_axis_labels_n) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x)
      10 ^ x),
    labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
  ) +
  coord_cartesian(ylim = c(min_y_lasso_n, max_y_lasso_n)) +
  scale_color_manual(
    name = "Equation",
    breaks = c("xdot", "ydot", "zdot"),
    labels = equation_labels,
    values = colors_plot
  ) +
  scale_fill_manual(
    name = "Equation",
    breaks = c("xdot", "ydot", "zdot"),
    labels = equation_labels,
    values = colors_plot
  ) +
  theme(
    plot.title = element_text(size = plot_title),
    axis.text.x = element_text(size = x_axis_text_size),
    axis.text.y = element_text(size = y_axis_text_size),
    axis.title.y = element_text(
      size = y_axis_label_size,
      angle = 360,
      vjust = 0.5
    ),
    axis.title.x = element_text(size = x_axis_label_size, face = "italic"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black")
  )
# Increasing SNR ----
lasso_snr_lambda <- all_combined_df_increasing_snr_lambda %>%
  dplyr::filter(Model == "Lasso")
min_y_lasso_snr <- min(lasso_snr_lambda$lambda, na.rm = TRUE)
max_y_lasso_snr <- max(lasso_snr_lambda$lambda, na.rm = TRUE)
xstart <- 65.5
xend <- 69.5
extra_x <- 1
y_sep <- min_y_lasso_snr - 0.05*(min_y_lasso_snr)
myseg <- create_separators(c(xstart, xend), extra_x = 1, y = y_sep, extra_y = 0.1, angle = 75)
safe_y <- 10 ^ (log10(min_y_lasso_snr) - 0.1)  # Lower y-values safely in log scale
lasso_snr_plot <- ggplot(lasso_snr_lambda,
                          aes(x = snr_value, y = lambda, fill = data_source)) +
  geom_boxplot() +
  ggtitle("ARGOS-Lasso") +
  xlab("SNR") +
  # ylab(expression(lambda[L])) +
  ylab(expression(lambda[L]^"*")) +
  scale_x_discrete(labels = x_axis_labels_snr,
                   limits = unique(lasso_snr_lambda$snr_value)) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  coord_cartesian(ylim = c(min_y_lasso_snr,
                           max_y_lasso_snr)) +
  scale_color_manual(
    name = "Equation",
    breaks = c("xdot", "ydot", "zdot"),
    labels = equation_labels,
    values = colors_plot
  ) +
  scale_fill_manual(
    name = "Equation",
    breaks = c("xdot", "ydot", "zdot"),
    labels = equation_labels,
    values = colors_plot
  ) +
  theme(
    plot.title = element_text(size = plot_title),
    axis.text.x = element_text(size = x_axis_text_size, hjust = 0.5),
    axis.text.y = element_text(size = y_axis_text_size),
    axis.title.y = element_text(size = y_axis_label_size, angle = 360, vjust = 0.5),
    axis.title.x = element_text(size = x_axis_label_size, angle = 360, vjust = 0.5),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black")
  )

# Adaptive Lasso ----
# Increasing N ----
alasso_n_lambda <- all_combined_df_increasing_n_lambda %>%
  dplyr::filter(Model == "Adaptive Lasso")
min_y_alasso_n <- min(alasso_n_lambda$lambda, na.rm = TRUE)
max_y_alasso_n <- max(alasso_n_lambda$lambda, na.rm = TRUE)
alasso_n_plot <-
  ggplot(alasso_n_lambda, aes(x = n_value, y = lambda, fill = data_source)) +
  geom_boxplot() +
  ggtitle("ARGOS-Adaptive Lasso") +
  xlab("n") +
  ylab(expression(lambda[AL]^"*")) +
  scale_x_discrete(labels = x_axis_labels_n) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x)
      10 ^ x),
    labels = scales::trans_format("log10", scales::math_format(10 ^ .x))
  ) +
  coord_cartesian(ylim = c(min_y_alasso_n, max_y_alasso_n)) +
  scale_color_manual(
    name = "Equation",
    breaks = c("xdot", "ydot", "zdot"),
    labels = equation_labels,
    values = colors_plot
  ) +
  scale_fill_manual(
    name = "Equation",
    breaks = c("xdot", "ydot", "zdot"),
    labels = equation_labels,
    values = colors_plot
  ) +
  theme(
    plot.title = element_text(size = plot_title),
    axis.text.x = element_text(size = x_axis_text_size),
    axis.text.y = element_text(size = y_axis_text_size),
    axis.title.y = element_text(
      size = y_axis_label_size,
      angle = 360,
      vjust = 0.5
    ),
    axis.title.x = element_text(size = x_axis_label_size, face = "italic"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black")
  )
# Increasing SNR ----
alasso_snr_lambda <- all_combined_df_increasing_snr_lambda %>%
  dplyr::filter(Model == "Adaptive Lasso")
# min_y_alasso_snr <- min(alasso_snr_lambda$lambda, na.rm = TRUE)
min_y_alasso_snr <- 1
max_y_alasso_snr <- max(alasso_snr_lambda$lambda, na.rm = TRUE)
alasso_snr_plot <- ggplot(alasso_snr_lambda,
                          aes(x = snr_value, y = lambda, fill = data_source)) +
  geom_boxplot() +
  ggtitle("ARGOS-Adaptive Lasso") +
  xlab("SNR") +
  ylab(expression(lambda[AL]^"*")) +
  scale_x_discrete(labels = x_axis_labels_snr) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  coord_cartesian(ylim = c(min_y_alasso_snr,
                           max_y_alasso_snr)) +
  scale_color_manual(
    name = "Equation",
    breaks = c("xdot", "ydot", "zdot"),
    labels = equation_labels,
    values = colors_plot
  ) +
  scale_fill_manual(
    name = "Equation",
    breaks = c("xdot", "ydot", "zdot"),
    labels = equation_labels,
    values = colors_plot
  ) +
  theme(
    plot.title = element_text(size = plot_title),
    axis.text.x = element_text(size = x_axis_text_size),
    axis.text.y = element_text(size = y_axis_text_size),
    axis.title.y = element_text(size = y_axis_label_size, angle = 360, vjust = 0.5),
    axis.title.x = element_text(size = x_axis_label_size, angle = 360, vjust = 0.5),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black")
  )




layout_matrix <- matrix(c(0,0,rep(1,20),rep(2,20),rep(2,20)), ncol=1)
stacked_lorenz_n <- grid.arrange(
  ggplot() + labs(tag = expression(bold('a'))) + theme(
    text = element_text(size = 35),
    plot.background = element_blank(),
    panel.background = element_blank()
  ),
  lasso_n_plot,
  alasso_n_plot,
  nrow = 3,
  ncol = 1,
  heights = c(1, 10, 10)
)

stacked_lorenz_snr <- grid.arrange(
  ggplot() + labs(tag = expression(bold('b'))) + theme(
    text = element_text(size = 35),
    plot.background = element_blank(),
    panel.background = element_blank()
  ),
  lasso_snr_plot,
  alasso_snr_plot,
  nrow = 3,
  ncol = 1,
  heights = c(1, 10, 10)
)

ggsave(stacked_lorenz_n, filename = '~/Figures/additional_figs/lambda_lorenz_n.pdf', width = 10, height = 15, dpi = 300)
ggsave(stacked_lorenz_n, filename = '~/Figures/additional_figs/lambda_lorenz_n.png', width = 10, height = 15, dpi = 300)

ggsave(stacked_lorenz_snr, filename = '~/Figures/additional_figs/lambda_lorenz_snr.pdf', width = 10, height = 15, dpi = 300)
ggsave(stacked_lorenz_snr, filename = '~/Figures/additional_figs/lambda_lorenz_snr.png', width = 10, height = 15, dpi = 300)


library(cowplot)
equation_labels <- my_expressions <- c(expression(dot(x)[1]),
                                       expression(dot(x)[2]),
                                       expression(dot(x)[3]))
# lines are for model
legend_ggplot <- ggplot(all_lasso_long, aes(x = snr_value, y = threshold, fill = data_source)) +
  geom_boxplot() +
  xlab("SNR") +
  ylab(expression(eta[AL])) +
  scale_x_discrete(labels = x_axis_labels) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  coord_cartesian(ylim = c(min_y, 100)) +
  scale_color_manual(
    name = "Equation",
    breaks = c("xdot", "ydot", "zdot"),
    labels = equation_labels,
    values = colors_plot
  ) +
  scale_fill_manual(
    name = "Equation",
    breaks = c("xdot", "ydot", "zdot"),
    labels = equation_labels,
    values = colors_plot
  ) +
  theme(
    axis.text = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 24, angle = 360, vjust = 0.5),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.key = element_rect(fill = "white")
  )

legend <- get_legend(legend_ggplot)

cowplot::plot_grid(legend)
