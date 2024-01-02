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
load("~/all_combined_df_increasing_snr_threshold.RData")
load("~/all_combined_df_increasing_n_threshold.RData")

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
lasso_n_threshold <- all_combined_df_increasing_n_threshold %>%
  dplyr::filter(Model == "Lasso")
min_y_lasso_n <- min(lasso_n_threshold$threshold, na.rm = TRUE)
max_y_lasso_n <- max(lasso_n_threshold$threshold, na.rm = TRUE)
# min_y_lasso_n <- 10^-5
max_y_lasso_n <- 100
lasso_n_plot <-
  ggplot(lasso_n_threshold, aes(x = n_value, y = threshold, fill = data_source)) +
  geom_boxplot() +
  ggtitle("ARGOS-Lasso") +
  xlab("n") +
  # ylab(expression(eta[L])) +
  ylab(expression(eta[L]^"*")) +
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
lasso_snr_threshold <- all_combined_df_increasing_snr_threshold %>%
  dplyr::filter(Model == "Lasso")
min_y_lasso_snr <- min(lasso_snr_threshold$threshold, na.rm = TRUE)
max_y_lasso_snr <- 100
lasso_snr_plot <- ggplot(lasso_snr_threshold,
                          aes(x = snr_value, y = threshold, fill = data_source)) +
  geom_boxplot() +
  ggtitle("ARGOS-Lasso") +
  xlab("SNR") +
  # ylab(expression(eta[L])) +
  ylab(expression(eta[L]^"*")) +
  scale_x_discrete(labels = x_axis_labels_snr,
                   limits = unique(lasso_snr_threshold$snr_value)) +
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
alasso_n_threshold <- all_combined_df_increasing_n_threshold %>%
  dplyr::filter(Model == "Adaptive Lasso")
min_y_alasso_n <- min(alasso_n_threshold$threshold, na.rm = TRUE)
# max_y_alasso_n <- max(alasso_n_threshold$threshold, na.rm = TRUE)
max_y_alasso_n <- 100
alasso_n_plot <-
  ggplot(alasso_n_threshold, aes(x = n_value, y = threshold, fill = data_source)) +
  geom_boxplot() +
  ggtitle("ARGOS-Adaptive Lasso") +
  xlab("n") +
  ylab(expression(eta[AL]^"*")) +
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
alasso_snr_threshold <- all_combined_df_increasing_snr_threshold %>%
  dplyr::filter(Model == "Adaptive Lasso")
min_y_alasso_snr <- min(alasso_snr_threshold$threshold, na.rm = TRUE)
max_y_alasso_snr <- 100
alasso_snr_plot <- ggplot(alasso_snr_threshold,
                          aes(x = snr_value, y = threshold, fill = data_source)) +
  geom_boxplot() +
  ggtitle("ARGOS-Adaptive Lasso") +
  xlab("SNR") +
  ylab(expression(eta[AL]^"*")) +
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

ggsave(stacked_lorenz_n, filename = '~/Figures/additional_figs/threshold_lorenz_n.pdf', width = 10, height = 15, dpi = 300)
ggsave(stacked_lorenz_n, filename = '~/Figures/additional_figs/threshold_lorenz_n.png', width = 10, height = 15, dpi = 300)

ggsave(stacked_lorenz_snr, filename = '~/Figures/additional_figs/threshold_lorenz_snr.pdf', width = 10, height = 15, dpi = 300)
ggsave(stacked_lorenz_snr, filename = '~/Figures/additional_figs/threshold_lorenz_snr.png', width = 10, height = 15, dpi = 300)
