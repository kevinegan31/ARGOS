rm(list = ls())
library(ggplot2)
library(plot3D)
library(tidyr)
library(tidyverse)
library(scales)
library(latex2exp)
library(cowplot)
library(ggh4x)
library(ggpubr)
library(gridExtra)
library(patchwork)

file_wd <- "/Users/kevinegan/Documents/GitHub.nosync/ARGOS/" # github path
file_wd2 <-
  paste(file_wd, "Data/", sep = "")
setwd(file_wd2)
# setwd('D:/GitHub/ARGOS/Data')
# setwd('/Users/cfzh32/GitHub/ARGOS/Data')
# setwd('C:/Users/cfzh32/Documents/GitHub/ARGOS/Data')
## main ---------------
noise_levels <- c(1, 37, Inf)
n_obs = c(10 ^ 2, 10 ^ 3.5, 10 ^ 4)+1
n_log10 <- c(2, 3.5, 5)
ggplot_3d_path <- '../Plot_code/ggplot2_3d'
load_r <- list.files(ggplot_3d_path, '*.R')
sapply(load_r, function(i)
  source(paste0(ggplot_3d_path, '/', i)))

gglabel <- function(label, bg_col = "#f4f0e6", label_size = 30){ # the label of each plot
  ggplot() + ggtitle(label) + theme(plot.title = element_text(size = label_size,face = "bold", vjust=2.5,
                                                              margin=margin(0,0,-33,0)),
                                    plot.margin = unit(c(0,0,18,0),'pt'),
                                    plot.background = element_rect(fill = bg_col, color = bg_col))
}

gglabel <- function(label, bg_col = "#f4f0e6", label_size = 30){ # the label of each plot
  ggplot() + ggtitle(label) + theme(plot.title = element_text(size = label_size,face = "bold", vjust=2.5, margin=margin(0,0,0,0)),
                                    plot.margin = unit(c(0,0,-14,0),'pt'),
                                    plot.background = element_rect(fill = bg_col, color = bg_col))
}

gglabel2 <- function(label, bg_col = "#f4f0e6", label_size = 50){ # the label of each plot
  ggplot() + ggtitle(label) + theme(plot.title = element_text(size = label_size,face = "bold", vjust=1.5, margin=margin(0,0,0,0)),
                                    plot.margin = unit(c(0,0,-14,0),'pt'),
                                    plot.background = element_rect(fill = bg_col, color = bg_col),
                                    panel.background = element_rect(fill = bg_col, color = bg_col))
}

gg_title <- function(label, bg_col = "#f4f0e6", label_size = 36, vjust=0){ # the label of each plot
  ggplot() + ggtitle(label) + theme(plot.title = element_text(size = label_size,face = "bold",vjust=vjust),
                                    plot.margin = unit(c(2,0,0,0),'pt'),
                                    plot.background = element_rect(fill = bg_col, color = bg_col),
                                    panel.background = element_rect(fill = bg_col, color = bg_col))
}
empty_ggplot <- ggplot()+
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = unit(c(15,5.5,5.5,5.5),'pt'))

# layout_matrix <- matrix(c(rep(0,1),rep(1,8),rep(2,20)), ncol = 1)
height_rate_label <- c(8,20)
height_rate_title <- c(1,8,1)
widths_traj <- c(1,6,6,6,1)
colors_correct <- c("#9c72be", "#87a14d", "#cb6054")
shapes <- c(15, 16, 17)
# grid_label <- function(text, color="#f4f0e6", fontsize=27){
#   grobTree(rectGrob(gp=gpar(fill=color,col=color)), textGrob(text, x=0.02, vjust=0.5, gp=gpar(fontsize=fontsize, fontface=2)))
# }
# grid_title <- function(text, color="#f4f0e6", fontsize=35){
#   grobTree(rectGrob(gp=gpar(fill=color,col=color)), textGrob(text, y=0.5,vjust=0.5, gp=gpar(fontsize=fontsize, fontface=2)))
# }

## ggplot theme ------------------
ggplot_theme0 <- theme(
  # plot.background = element_rect(fill = 0, colour = 1),
  axis.line = element_line(colour = "black"),
  axis.ticks.length = unit(.25, "cm"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  legend.key = element_blank(),
  legend.text = element_text(size = 16),
  # Changed for legend
  legend.title = element_text(face = "bold", size = 16),
  axis.text = element_text(size = 20),
  axis.title.x = element_text(size = 24),
  axis.title.y = element_text(
    size = 24,
    angle = 90,
    vjust = 0.5
  ),
  plot.title = element_text(size = 24),
  plot.tag = element_text(size = 18),
  # plot.background = element_rect(fill = "#f4f0e6", colour = 1),
  legend.position = "none",
  legend.text.align = 0
)
ggplot_theme <- ggplot_theme0 + theme(plot.background = element_rect(fill = "#f4f0e6", colour = 0))
ggplot_theme1 <- ggplot_theme0 + theme(plot.background = element_rect(fill = 0, colour = 0))

dynamics2d_theme0 <- theme(
  # panel.border = element_rect(color='black',fill=0),
  # aspect.ratio = 2/(1+sqrt(5)),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  #element_rect(color = 0),
  # plot.background = element_rect(fill = "#f4f0e6", colour = 1),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  plot.title = element_text(hjust = 0.5, size = 22)
)
dynamics2d_theme <- dynamics2d_theme0 + theme(plot.background = element_rect(fill = "#f4f0e6", colour = 0))
dynamics2d_theme1 <- dynamics2d_theme0 + 
  theme(plot.background = element_rect(fill = 0, colour = 0))

dynamic3d_theme <- theme(
  plot.background = element_rect(fill = 0, colour = 0),
  plot.title = element_text(hjust = 0.5, size = 22)
)
# a simple function to help make the segments
create_separators <- function(x, extra_x, y, extra_y, angle = 45, scale = 1, length = .1){
  add_y <-  length * sin(angle * pi/180) /2
  add_x <- length * cos(angle * pi/180) 
  list(x = x - add_x*scale, xend = x + add_x*scale + extra_x, 
       y = rep(y - add_y*scale - extra_y, length(x)), yend = rep(y + add_y*scale - extra_y/2, length(x)))
}

## linear2d plot -----------------
source('../Additional_functions/dynamical_systems_models/R/linear2d_system.R')

init_conditions = c(2, 0)
mu = matrix(c(-0.1,-2, 2,-0.1), 2, 2)
dt = 0.01

## noisy plot
n_obs2 = 5001
# noise_levels <- c(1, 25, 49, Inf)
# snr_volt = 10 ** -(noise_levels / 20)
t_span = seq(0, n_obs2 * dt, dt)

snr_data <-
  lapply(noise_levels, function(i)
    as.data.frame(linear2d_system(n_obs2, init_conditions, dt, snr = i)[seq(1,n_obs2,10),]))
snr_ggplot_list <- list()
for (i in seq_along(snr_data)) {
  if (noise_levels[i] == Inf) {
    plot_title <- TeX('$SNR_{dB} = \\infty^ $')
  } else{
    plot_title <- TeX(paste0('$SNR_{dB}$ = ', noise_levels[i]))
  }
  snr_ggplot_list[[i]] <-
    ggplot(data = snr_data[[i]]) + geom_path(aes(x = `1`, y = `2`), color =
                                               '#3b9a9c') +
    labs(x = NULL, y = NULL, title = plot_title) +
    lims(x = c(min(snr_data[[i]][, 1]), max(snr_data[[i]][, 1])), y = c(min(snr_data[[i]][, 2]), max(snr_data[[i]][, 2]))) +
    dynamics2d_theme1
}
# arrangeGrob
# grid.arrange(snr_ggplot_list[[1]], snr_ggplot_list[[2]], snr_ggplot_list[[3]], snr_ggplot_list[[4]], nrow=1,ncol=4)
snr_plot_all <-
  arrangeGrob(
    empty_ggplot,
    snr_ggplot_list[[1]],
    snr_ggplot_list[[2]],
    snr_ggplot_list[[3]],
    # snr_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = widths_traj
  )
## n plot
# n_obs = c(10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5)+1 #c(10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5)+1
n_data <- lapply(n_obs, function(i) {
  # t_span = seq(0, n_obs[i]*dt, dt)
  as.data.frame(linear2d_system(i, init_conditions, dt, snr = 49)[seq(1,i,10),])
})

n_ggplot_list <- list()
for (i in seq_along(n_data)) {
  plot_title <- TeX(paste0('\\textit{n} = ', '$10^{', n_log10[i], '}$'))
  n_ggplot_list[[i]] <-
    ggplot(data = n_data[[i]], aes(x = `1`, y = `2`)) + geom_path(color = '#3b9a9c') +
    labs(x = NULL, y = NULL, title = plot_title) +
    lims(x = c(min(n_data[[i]][, 1]), max(n_data[[i]][, 1])), y = c(min(n_data[[i]][, 2]), max(n_data[[i]][, 2]))) +
    dynamics2d_theme1
}

n_plot_all <-
  arrangeGrob(
    empty_ggplot,
    n_ggplot_list[[1]],
    n_ggplot_list[[2]],
    n_ggplot_list[[3]],
    # n_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = c(1,6,6,6,1)
  )

## linear2d success rate n ---------------------------------
load("Linear2D/success_rate_RData/linear2d_inc_n_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_n_df

total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

x_labels <- c(
  expression(paste("10" ^ "2")),
  expression(paste("10" ^ "2.5")),
  expression(paste("10" ^ "3")),
  expression(paste("10" ^ "3.5")),
  expression(paste("10" ^ "4")),
  expression(paste("10" ^ "4.5")),
  expression(paste("10" ^ "5"))
)

x_breaks <- pretty_breaks()(n_seq)
y_breaks <-
  y_labels <- pretty_breaks()(c(0, max(total_correct$Value)))
prob_increase_n <-
  ggplot(total_correct,
         aes(
           x = n,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = expression(italic("n"))) +
  scale_x_continuous(labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(0, max(y_labels))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)

linear2d_n <-
  arrangeGrob(#gglabel('a'), 
              n_plot_all, prob_increase_n, heights=height_rate_label)

## linear2d success rate snr bic intercept -------------------------------------
load(
  "Linear2D/success_rate_RData/linear2d_inc_snr_success_rate_new_sg.RData"
)
total_correct <- total_correct_increasing_snr_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

# x_labels <- x_breaks <- pretty_breaks()(c(0, max(total_correct$eta)))
x_labels <- x_breaks <- seq(1, 73, by = 12)
# x_labels[length(x_labels)] <- "Inf"
x_labels[length(x_labels)] <- TeX("$\\infty$")

y_labels <-
  y_breaks <-
  pretty_breaks()(c(min(total_correct$Value), max(total_correct$Value)))

xstart <- 65.5
xend <- 69.5
extra_x <- 1
y_sep <- min(total_correct$Value) - 0.05*(min(total_correct$Value))
myseg <- create_separators(c(xstart, xend), extra_x = 1, y = y_sep, extra_y = 0.1, angle = 75)

# myseg$y <- -0.00005
prob_inrease_snr_bic_inter <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = TeX("$SNR_{dB}$")) +
  scale_x_continuous(limits = c(min(x_breaks),
                                max(x_breaks)),
                     labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(NA, max(y_breaks))
                     ) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name) +
  guides(x = guide_axis_truncated(
    trunc_lower = c(-Inf, xend + extra_x/2),
    trunc_upper = c(xstart + extra_x/2, Inf)
  )) +
  annotate("segment", 
           x = myseg$x, xend = myseg$xend,
           y = myseg$y + 0.05, yend = myseg$yend)  +
  coord_cartesian(clip = "off", ylim = c(-0.0005, NA))
linear2d_snr <-
  arrangeGrob(#gglabel('b'), 
              snr_plot_all, prob_inrease_snr_bic_inter, heights=height_rate_label)
## linear3d plot -----------------
source('../Additional_functions/dynamical_systems_models/R/linear3d_system.R')

init_conditions = c(2, 0, 1)
mu = matrix(c(-0.1,-2, 0,
              2,-0.1, 0,
              0, 0,-0.3), 3, 3)
dt = 0.01

## noisy plot
n_obs2 = 5000+1
# noise_levels <- c(1, 25, 49, Inf)
# snr_volt = 10 ** -(noise_levels / 20)
t_span = seq(0, n_obs2 * dt, dt)

snr_data <-
  lapply(noise_levels, function(i)
    as.data.frame(linear3d_system(n_obs2, init_conditions, dt, snr = i)[seq(1,n_obs2,10),]))
snr_ggplot_list <- list()

theta = 135
phi = 50
for (i in seq_along(snr_data)) {
  if (noise_levels[i] == Inf) {
    plot_title <- TeX('$SNR_{dB} = \\infty^ $')
  } else{
    plot_title <- TeX(paste0('$SNR_{dB}$ = ', noise_levels[i]))
  }
  snr_ggplot_list[[i]] <-
    ggplot(snr_data[[i]], aes(x = `1`, y = `2`, z = `3`)) + axes_3D(theta =
                                                                      theta, phi = phi) +
    stat_3D(
      theta = theta,
      phi = phi,
      geom = 'path',
      color = '#3b9a9c'
    ) +
    # axis_labs_3D(theta=theta, phi=phi, size=3, hjust=c(1,1,1.2,1.2,1.2,1.2), vjust=c(-.5,-.5,-.2,-.2,1.2,1.2)) +
    labs_3D(
      theta = theta,
      phi = phi,
      hjust = c(1, 0, 0),
      vjust = c(1.5, 1, -.2),
      labs = c("", "", "")
    ) + theme_void() +
    labs(title = plot_title) +
    dynamic3d_theme
}
# arrangeGrob
# grid.arrange(snr_ggplot_list[[1]], snr_ggplot_list[[2]], snr_ggplot_list[[3]], snr_ggplot_list[[4]], nrow=1,ncol=4)
snr_plot_all <-
  arrangeGrob(
    empty_ggplot,
    snr_ggplot_list[[1]],
    snr_ggplot_list[[2]],
    snr_ggplot_list[[3]],
    # snr_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = widths_traj
  )
## n plot
# n_obs = c(10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5)+1
n_data <- lapply(n_obs, function(i) {
  # t_span = seq(0, n_obs[i]*dt, dt)
  as.data.frame(linear3d_system(i, init_conditions, dt, snr = 49)[seq(1,i,10),])
})

axis_lims = list(
  xlim = c(min(n_data[[3]][, 1]), max(n_data[[3]][, 1])),
  ylim = c(min(n_data[[3]][, 2]), max(n_data[[3]][, 2])),
  zlim = c(min(n_data[[3]][, 3]), max(n_data[[3]][, 3]))
)

n_ggplot_list <- list()
for (i in seq_along(n_data)) {
  plot_title <- TeX(paste0('\\textit{n} = ', '$10^{', n_log10[i], '}$'))
  n_ggplot_list[[i]] <-
    ggplot(n_data[[i]], aes(x = `1`, y = `2`, z = `3`)) + axes_3D(theta =
                                                                    theta,
                                                                  phi = phi,
                                                                  axis_lims = axis_lims) +
    stat_3D(
      theta = theta,
      phi = phi,
      geom = 'path',
      color = '#3b9a9c',
      axis_lims = axis_lims
    ) +
    # axis_labs_3D(theta=theta, phi=phi, size=3, hjust=c(1,1,1.2,1.2,1.2,1.2), vjust=c(-.5,-.5,-.2,-.2,1.2,1.2),
    #              axis_lims = axis_lims) +
    labs_3D(
      theta = theta,
      phi = phi,
      hjust = c(1, 0, 0),
      vjust = c(1.5, 1, -.2),
      labs = c("", "", ""),
      axis_lims = axis_lims
    ) + theme_void() +
    labs(title = plot_title) +
    dynamic3d_theme
}
# grid.arrange(n_ggplot_list[[1]], n_ggplot_list[[2]], n_ggplot_list[[3]], n_ggplot_list[[4]], nrow=1,ncol=4)
n_plot_all <-
  arrangeGrob(
    empty_ggplot,
    n_ggplot_list[[1]],
    n_ggplot_list[[2]],
    n_ggplot_list[[3]],
    # n_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = c(1,6,6,6,1)
  )


## linear3d success rate n ---------------------------------
load("Linear3D/success_rate_RData/linear3d_inc_n_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_n_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

x_labels <- c(
  expression(paste("10" ^ "2")),
  expression(paste("10" ^ "2.5")),
  expression(paste("10" ^ "3")),
  expression(paste("10" ^ "3.5")),
  expression(paste("10" ^ "4")),
  expression(paste("10" ^ "4.5")),
  expression(paste("10" ^ "5"))
)

x_breaks <- pretty_breaks()(n_seq)
y_breaks <-
  y_labels <- pretty_breaks()(c(0, max(total_correct$Value)))
prob_increase_n <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = expression(italic("n"))) +
  scale_x_continuous(labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(0, max(y_labels))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)
linear3d_n <-
  arrangeGrob(#gglabel('c'), 
    n_plot_all, prob_increase_n, heights=height_rate_label)

## linear3d success rate snr bic intercept -------------------------------------
load("Linear3D/success_rate_RData/linear3d_inc_snr_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_snr_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

# x_labels <- x_breaks <- pretty_breaks()(c(0, max(total_correct$eta)))
x_labels <- x_breaks <- seq(1, 73, by = 12)
# x_labels[length(x_labels)] <- "Inf"
x_labels[length(x_labels)] <- TeX("$\\infty$")

y_labels <-
  y_breaks <-
  pretty_breaks()(c(min(total_correct$Value), max(total_correct$Value)))

xstart <- 65.5
xend <- 69.5
extra_x <- 1
y_sep <- min(total_correct$Value) - 0.05*(min(total_correct$Value))
myseg <- create_separators(c(xstart, xend), extra_x = 1, y = y_sep, extra_y = 0.1, angle = 75)

prob_inrease_snr_bic_inter <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = TeX("$SNR_{dB}$")) +
  scale_x_continuous(limits = c(min(x_breaks),
                                max(x_breaks)),
                     labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(NA, max(y_breaks))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)+
  guides(x = guide_axis_truncated(
    trunc_lower = c(-Inf, xend + extra_x/2),
    trunc_upper = c(xstart + extra_x/2, Inf)
  )) +
  annotate("segment", 
           x = myseg$x, xend = myseg$xend,
           y = myseg$y + 0.05, yend = myseg$yend)  +
  coord_cartesian(clip = "off", ylim = c(-0.0005, NA))
linear3d_snr <-
  arrangeGrob(#gglabel('d'), 
              snr_plot_all, prob_inrease_snr_bic_inter, heights=height_rate_label)
## cubic2d plot -----------------
source('../Additional_functions/dynamical_systems_models/R/cubic2d_system.R')

init_conditions = c(2, 0)
matrix_a <- matrix(c(-0.1,-2, 2,-0.1), 2, 2)
dt = 0.01

## noisy plot
n_obs2 = 5000+1
# noise_levels <- c(1, 25, 49, Inf)
# snr_volt = 10 ** -(noise_levels / 20)
t_span = seq(0, n_obs2 * dt, dt)

snr_data <-
  lapply(noise_levels, function(i)
    as.data.frame(cubic2d_system(n_obs2, init_conditions, dt, snr = i)[seq(1,n_obs2,3),]))
snr_ggplot_list <- list()
for (i in seq_along(snr_data)) {
  if (noise_levels[i] == Inf) {
    plot_title <- TeX('$SNR_{dB} = \\infty^ $')
  } else{
    plot_title <- TeX(paste0('$SNR_{dB}$ = ', noise_levels[i]))
  }
  snr_ggplot_list[[i]] <-
    ggplot(data = snr_data[[i]]) + geom_path(aes(x = `1`, y = `2`), color =
                                               '#3b9a9c') +
    labs(x = NULL, y = NULL, title = plot_title) +
    lims(x = c(min(snr_data[[i]][, 1]), max(snr_data[[i]][, 1])), y = c(min(snr_data[[i]][, 2]), max(snr_data[[i]][, 2]))) +
    dynamics2d_theme1
}
# arrangeGrob
# grid.arrange(snr_ggplot_list[[1]], snr_ggplot_list[[2]], snr_ggplot_list[[3]], snr_ggplot_list[[4]], nrow=1,ncol=4)
snr_plot_all <-
  arrangeGrob(
    empty_ggplot,
    snr_ggplot_list[[1]],
    snr_ggplot_list[[2]],
    snr_ggplot_list[[3]],
    # snr_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = widths_traj
  )
## n plot
# n_obs = c(10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5)+1
n_data <- lapply(n_obs, function(i) {
  # t_span = seq(0, n_obs[i]*dt, dt)
  as.data.frame(cubic2d_system(i, init_conditions, dt, snr = 49)[seq(1,i,3),])
})

n_ggplot_list <- list()
for (i in seq_along(n_data)) {
  plot_title <- TeX(paste0('\\textit{n} = ', '$10^{', n_log10[i], '}$'))
  n_ggplot_list[[i]] <-
    ggplot(data = n_data[[i]], aes(x = `1`, y = `2`)) + geom_path(color = '#3b9a9c') +
    labs(x = NULL, y = NULL, title = plot_title) +
    lims(x = c(min(n_data[[i]][, 1]), max(n_data[[i]][, 1])), y = c(min(n_data[[i]][, 2]), max(n_data[[i]][, 2]))) +
    dynamics2d_theme1
}
# grid.arrange(n_ggplot_list[[1]], n_ggplot_list[[2]], n_ggplot_list[[3]], n_ggplot_list[[4]], nrow=1,ncol=4)
n_plot_all <-
  arrangeGrob(
    empty_ggplot,
    n_ggplot_list[[1]],
    n_ggplot_list[[2]],
    n_ggplot_list[[3]],
    # n_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = c(1,6,6,6,1)
  )


## cubic2d success rate n ---------------------------------
load("Cubic2D/success_rate_RData/cubic2d_inc_n_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_n_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

x_labels <- c(
  expression(paste("10" ^ "2")),
  expression(paste("10" ^ "2.5")),
  expression(paste("10" ^ "3")),
  expression(paste("10" ^ "3.5")),
  expression(paste("10" ^ "4")),
  expression(paste("10" ^ "4.5")),
  expression(paste("10" ^ "5"))
)

x_breaks <- pretty_breaks()(n_seq)
y_breaks <-
  y_labels <- pretty_breaks()(c(0, max(total_correct$Value)))
prob_increase_n <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = expression(italic("n"))) +
  scale_x_continuous(labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(0, max(y_labels))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)

cubic2d_n <-
  arrangeGrob(#gglabel('e'), 
              n_plot_all, prob_increase_n, heights=height_rate_label)

## cubic2d success rate snr bic intercept -------------------------------------
load("Cubic2D/success_rate_RData/cubic2d_inc_snr_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_snr_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]


# x_labels <- x_breaks <- pretty_breaks()(c(0, max(total_correct$eta)))
x_labels <- x_breaks <- seq(1, 73, by = 12)
# x_labels[length(x_labels)] <- "Inf"
x_labels[length(x_labels)] <- TeX("$\\infty$")

y_labels <-
  y_breaks <-
  pretty_breaks()(c(min(total_correct$Value), max(total_correct$Value)))

xstart <- 65.5
xend <- 69.5
extra_x <- 1
y_sep <- min(total_correct$Value) - 0.05*(min(total_correct$Value))
myseg <- create_separators(c(xstart, xend), extra_x = 1, y = 0.043, extra_y = 0.1, angle = 75, scale=0.12)

prob_inrease_snr_bic_inter <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = TeX("$SNR_{dB}$")) +
  scale_x_continuous(limits = c(min(x_breaks),
                                max(x_breaks)),
                     labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(NA, max(y_breaks))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)+
  guides(x = guide_axis_truncated(
    trunc_lower = c(-Inf, xend + extra_x/2),
    trunc_upper = c(xstart + extra_x/2, Inf)
  )) +
  annotate("segment", 
           x = myseg$x, xend = myseg$xend,
           y = myseg$y + 0.05, yend = myseg$yend)  +
  coord_cartesian(clip = "off", ylim = c(-0.0005, NA))
  # coord_flip()
cubic2d_snr <-
  arrangeGrob(#gglabel('f'), 
              snr_plot_all, prob_inrease_snr_bic_inter, heights=height_rate_label)
## rossler plot -----------------
source('../Additional_functions/dynamical_systems_models/R/rossler_system.R')
init_conditions = c(1, 1, 1)
a = 0.2
b = 0.2
c = 5.7
dt = 0.01
## noisy plot
n_obs2 = 5000+1
# noise_levels <- c(1, 25, 49, Inf)
# snr_volt = 10 ** -(noise_levels / 20)
t_span = seq(0, n_obs2 * dt, dt)

snr_data <-
  lapply(noise_levels, function(i)
    as.data.frame(rossler_system(n_obs2, dt, init_conditions, a, b, c, snr = i)[seq(1,n_obs2,5),]))
snr_ggplot_list <- list()
theta = 0
phi = 0 # theta=135; phi=70
for (i in seq_along(snr_data)) {
  if (noise_levels[i] == Inf) {
    plot_title <- TeX('$SNR_{dB} = \\infty^ $')
  } else{
    plot_title <- TeX(paste0('$SNR_{dB}$ = ', noise_levels[i]))
  }
  snr_ggplot_list[[i]] <-
    ggplot(snr_data[[i]], aes(x = `1`, y = `2`, z = `3`)) + axes_3D(theta =
                                                                      theta, phi = phi) +
    stat_3D(
      theta = theta,
      phi = phi,
      geom = 'path',
      color = '#3b9a9c'
    ) +
    # axis_labs_3D(theta=theta, phi=phi, size=3, hjust=c(1,1,1.2,1.2,1.2,1.2), vjust=c(-.5,-.5,-.2,-.2,1.2,1.2)) +
    labs_3D(
      theta = theta,
      phi = phi,
      hjust = c(1, 0, 0),
      vjust = c(1.5, 1, -.2),
      labs = c("", "", "")
    ) + theme_void() +
    labs(title = plot_title) +
    dynamic3d_theme
}
# arrangeGrob
# grid.arrange(snr_ggplot_list[[1]], snr_ggplot_list[[2]], snr_ggplot_list[[3]], snr_ggplot_list[[4]], nrow=1,ncol=4)
snr_plot_all <-
  arrangeGrob(
    empty_ggplot,
    snr_ggplot_list[[1]],
    snr_ggplot_list[[2]],
    snr_ggplot_list[[3]],
    # snr_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = widths_traj
  )
## n plot
# n_obs = c(10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5)+1
n_data <- lapply(n_obs, function(i) {
  # t_span = seq(0, n_obs[i]*dt, dt)
  as.data.frame(rossler_system(i, dt, init_conditions, a, b, c, snr = 49)[seq(1,i,5),])
})

axis_lims = list(
  xlim = c(min(n_data[[3]][, 1]), max(n_data[[3]][, 1])),
  ylim = c(min(n_data[[3]][, 2]), max(n_data[[3]][, 2])),
  zlim = c(min(n_data[[3]][, 3]), max(n_data[[3]][, 3]))
)

n_ggplot_list <- list()
for (i in seq_along(n_data)) {
  plot_title <- TeX(paste0('\\textit{n} = ', '$10^{', n_log10[i], '}$'))
  n_ggplot_list[[i]] <-
    ggplot(n_data[[i]], aes(x = `1`, y = `2`, z = `3`)) + axes_3D(theta =
                                                                    theta,
                                                                  phi = phi,
                                                                  axis_lims = axis_lims) +
    stat_3D(
      theta = theta,
      phi = phi,
      geom = 'path',
      color = '#3b9a9c',
      axis_lims = axis_lims
    ) +
    # axis_labs_3D(theta=theta, phi=phi, size=3, hjust=c(1,1,1.2,1.2,1.2,1.2), vjust=c(-.5,-.5,-.2,-.2,1.2,1.2),
    #              axis_lims = axis_lims) +
    labs_3D(
      theta = theta,
      phi = phi,
      hjust = c(1, 0, 0),
      vjust = c(1.5, 1, -.2),
      labs = c("", "", ""),
      axis_lims = axis_lims
    ) + theme_void() +
    labs(title = plot_title) +
    dynamic3d_theme
}
# grid.arrange(n_ggplot_list[[1]], n_ggplot_list[[2]], n_ggplot_list[[3]], n_ggplot_list[[4]], nrow=1,ncol=4)
n_plot_all <-
  arrangeGrob(
    empty_ggplot,
    n_ggplot_list[[1]],
    n_ggplot_list[[2]],
    n_ggplot_list[[3]],
    # n_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = c(1,6,6,6,1)
  )


## rossler success rate n ---------------------------------
load("Rossler/success_rate_RData/rossler_inc_n_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_n_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]


x_labels <- c(
  expression(paste("10" ^ "2")),
  expression(paste("10" ^ "2.5")),
  expression(paste("10" ^ "3")),
  expression(paste("10" ^ "3.5")),
  expression(paste("10" ^ "4")),
  expression(paste("10" ^ "4.5")),
  expression(paste("10" ^ "5"))
)

x_breaks <- pretty_breaks()(n_seq)
y_breaks <-
  y_labels <- pretty_breaks()(c(0, max(total_correct$Value)))
prob_increase_n <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = expression(italic("n"))) +
  scale_x_continuous(labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(0, max(y_labels))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)
rossler_n <-
  arrangeGrob(#gglabel('g')+theme(plot.title = element_text(vjust = 3.4)), 
              n_plot_all, prob_increase_n, heights=height_rate_label)
## rossler success rate snr bic intercept -------------------------------------
load("Rossler/success_rate_RData/rossler_inc_snr_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_snr_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

# x_labels <- x_breaks <- pretty_breaks()(c(0, max(total_correct$eta)))
x_labels <- x_breaks <- seq(1, 73, by = 12)
# x_labels[length(x_labels)] <- "Inf"
x_labels[length(x_labels)] <- TeX("$\\infty$")

y_labels <-
  y_breaks <-
  pretty_breaks()(c(min(total_correct$Value), max(total_correct$Value)))

xstart <- 65.5
xend <- 69.5
extra_x <- 1
y_sep <- min(total_correct$Value) - 0.05*(min(total_correct$Value))
myseg <- create_separators(c(xstart, xend), extra_x = 1, y = y_sep, extra_y = 0.1, angle = 75)

prob_inrease_snr_bic_inter <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = TeX("$SNR_{dB}$")) +
  scale_x_continuous(limits = c(min(x_breaks),
                                max(x_breaks)),
                     labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(NA, max(y_breaks))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)+  
  guides(x = guide_axis_truncated(
    trunc_lower = c(-Inf, xend + extra_x/2),
    trunc_upper = c(xstart + extra_x/2, Inf)
  )) +
  annotate("segment", 
           x = myseg$x, xend = myseg$xend,
           y = myseg$y + 0.05, yend = myseg$yend)  +
  coord_cartesian(clip = "off", ylim = c(-0.0005, NA))
rossler_snr <-
  arrangeGrob(#gglabel('h'), 
              snr_plot_all, prob_inrease_snr_bic_inter, heights=height_rate_label)

## lorenz plot -----------------
source('../Additional_functions/dynamical_systems_models/R/lorenz_system.R')

init_conditions = c(2, 0, 1)
mu = matrix(c(-0.1,-2, 0,
              2,-0.1, 0,
              0, 0,-0.3), 3, 3)
dt = 0.01

## noisy plot
n_obs2 = 5000+1
# noise_levels <- c(1, 25, 49, Inf)
# snr_volt = 10 ** -(noise_levels / 20)
t_span = seq(0, n_obs2 * dt, dt)

snr_data <-
  lapply(noise_levels, function(i)
    as.data.frame(lorenz_system(n_obs2, init_conditions, dt, snr = i)[seq(1,n_obs2,1),]))
snr_ggplot_list <- list()

# theta=135; phi=50
theta = 0
phi = 0
for (i in seq_along(snr_data)) {
  if (noise_levels[i] == Inf) {
    plot_title <- TeX('$SNR_{dB} = \\infty^ $')
  } else{
    plot_title <- TeX(paste0('$SNR_{dB}$ = ', noise_levels[i]))
  }
  snr_ggplot_list[[i]] <-
    ggplot(snr_data[[i]], aes(x = `1`, y = `2`, z = `3`)) + axes_3D(theta =
                                                                      theta, phi = phi) +
    stat_3D(
      theta = theta,
      phi = phi,
      geom = 'path',
      color = '#3b9a9c'
    ) +
    # axis_labs_3D(theta=theta, phi=phi, size=3, hjust=c(1,1,1.2,1.2,1.2,1.2), vjust=c(-.5,-.5,-.2,-.2,1.2,1.2)) +
    labs_3D(
      theta = theta,
      phi = phi,
      hjust = c(1, 0, 0),
      vjust = c(1.5, 1, -.2),
      labs = c("", "", "")
    ) + theme_void() +
    labs(title = plot_title) +
    dynamic3d_theme
}
# arrangeGrob
# grid.arrange(snr_ggplot_list[[1]], snr_ggplot_list[[2]], snr_ggplot_list[[3]], snr_ggplot_list[[4]], nrow=1,ncol=4)
snr_plot_all <-
  arrangeGrob(
    empty_ggplot,
    snr_ggplot_list[[1]],
    snr_ggplot_list[[2]],
    snr_ggplot_list[[3]],
    # snr_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = widths_traj
  )
## n plot
# n_obs = c(10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5)+1
n_data <- lapply(n_obs, function(i) {
  # t_span = seq(0, n_obs[i]*dt, dt)
  as.data.frame(lorenz_system(i, init_conditions, dt, snr = 49)[seq(1,i,1),])
})

axis_lims = list(
  xlim = c(min(n_data[[3]][, 1]), max(n_data[[3]][, 1])),
  ylim = c(min(n_data[[3]][, 2]), max(n_data[[3]][, 2])),
  zlim = c(min(n_data[[3]][, 3]), max(n_data[[3]][, 3]))
)

n_ggplot_list <- list()
for (i in seq_along(n_data)) {
  plot_title <- TeX(paste0('\\textit{n} = ', '$10^{', n_log10[i], '}$'))
  n_ggplot_list[[i]] <-
    ggplot(n_data[[i]], aes(x = `1`, y = `2`, z = `3`)) + axes_3D(theta =
                                                                    theta,
                                                                  phi = phi,
                                                                  axis_lims = axis_lims) +
    stat_3D(
      theta = theta,
      phi = phi,
      geom = 'path',
      color = '#3b9a9c',
      axis_lims = axis_lims
    ) +
    # axis_labs_3D(theta=theta, phi=phi, size=3, hjust=c(1,1,1.2,1.2,1.2,1.2), vjust=c(-.5,-.5,-.2,-.2,1.2,1.2),
    #              axis_lims = axis_lims) +
    labs_3D(
      theta = theta,
      phi = phi,
      hjust = c(1, 0, 0),
      vjust = c(1.5, 1, -.2),
      labs = c("", "", ""),
      axis_lims = axis_lims
    ) + theme_void() +
    labs(title = plot_title) +
    dynamic3d_theme
}
# grid.arrange(n_ggplot_list[[1]], n_ggplot_list[[2]], n_ggplot_list[[3]], n_ggplot_list[[4]], nrow=1,ncol=4)
n_plot_all <-
  arrangeGrob(
    empty_ggplot,
    n_ggplot_list[[1]],
    n_ggplot_list[[2]],
    n_ggplot_list[[3]],
    # n_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = c(1,6,6,6,1)
  )


## lorenz success rate n ---------------------------------
load("Lorenz/success_rate_RData/lorenz_inc_n_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_n_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

x_labels <- c(
  expression(paste("10" ^ "2")),
  expression(paste("10" ^ "2.5")),
  expression(paste("10" ^ "3")),
  expression(paste("10" ^ "3.5")),
  expression(paste("10" ^ "4")),
  expression(paste("10" ^ "4.5")),
  expression(paste("10" ^ "5"))
)

x_breaks <- pretty_breaks()(n_seq)
y_breaks <-
  y_labels <- pretty_breaks()(c(0, max(total_correct$Value)))
prob_increase_n <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = expression(italic("n"))) +
  scale_x_continuous(labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(0, max(y_labels))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)
lorenz_n <-
  arrangeGrob(#gglabel('i'), 
              n_plot_all, prob_increase_n, heights=height_rate_label)

## lorenz success rate snr bic intercept -------------------------------------
load("Lorenz/success_rate_RData/lorenz_inc_snr_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_snr_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

# x_labels <- x_breaks <- pretty_breaks()(c(0, max(total_correct$eta)))
x_labels <- x_breaks <- seq(1, 73, by = 12)
# x_labels[length(x_labels)] <- "Inf"
x_labels[length(x_labels)] <- TeX("$\\infty$")

y_labels <-
  y_breaks <-
  pretty_breaks()(c(min(total_correct$Value), max(total_correct$Value)))

xstart <- 65.5
xend <- 69.5
extra_x <- 1
y_sep <- min(total_correct$Value) - 0.05*(min(total_correct$Value))
myseg <- create_separators(c(xstart, xend), extra_x = 1, y = y_sep, extra_y = 0.1, angle = 75)

prob_inrease_snr_bic_inter <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = TeX("$SNR_{dB}$")) +
  scale_x_continuous(limits = c(min(x_breaks),
                                max(x_breaks)),
                     labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(NA, max(y_breaks))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)+
  guides(x = guide_axis_truncated(
    trunc_lower = c(-Inf, xend + extra_x/2),
    trunc_upper = c(xstart + extra_x/2, Inf)
  )) +
  annotate("segment", 
           x = myseg$x, xend = myseg$xend,
           y = myseg$y + 0.05, yend = myseg$yend)  +
  coord_cartesian(clip = "off", ylim = c(-0.0005, NA))
lorenz_snr <-
  arrangeGrob(#gglabel('j')+theme(plot.title = element_text(vjust = 3.4)), 
              snr_plot_all, prob_inrease_snr_bic_inter, heights=height_rate_label)
## vdp plots ----------------------
source('../Additional_functions/dynamical_systems_models/R/vdp_oscillator.R')
init_conditions = c(2, 0)
mu = 1.2
dt = 0.01

## noisy plot
n_obs2 = 5000+1
# noise_levels <- c(1, 25, 49, Inf)
# snr_volt = 10 ** -(noise_levels / 20)
t_span = seq(0, n_obs2 * dt, dt)

snr_data <-
  lapply(noise_levels, function(i)
    as.data.frame(vdp_oscillator(n_obs2, dt, init_conditions, mu, snr = i)[seq(1,n_obs2,10),]))
snr_ggplot_list <- list()
for (i in seq_along(snr_data)) {
  if (noise_levels[i] == Inf) {
    plot_title <- TeX('$SNR_{dB} = \\infty^ $')
  } else{
    plot_title <- TeX(paste0('$SNR_{dB}$ = ', noise_levels[i]))
  }
  snr_ggplot_list[[i]] <-
    ggplot(data = snr_data[[i]]) + geom_path(aes(x = `1`, y = `2`), color =
                                               '#3b9a9c') +
    labs(x = NULL, y = NULL, title = plot_title) +
    lims(x = c(min(snr_data[[i]][, 1]), max(snr_data[[i]][, 1])), y = c(min(snr_data[[i]][, 2]), max(snr_data[[i]][, 2]))) +
    dynamics2d_theme1
}
# arrangeGrob
# grid.arrange(snr_ggplot_list[[1]], snr_ggplot_list[[2]], snr_ggplot_list[[3]], snr_ggplot_list[[4]], nrow=1,ncol=4)
snr_plot_all <-
  arrangeGrob(
    empty_ggplot,
    snr_ggplot_list[[1]],
    snr_ggplot_list[[2]],
    snr_ggplot_list[[3]],
    # snr_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = widths_traj
  )
## n plot
# n_obs = c(10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5)+1
n_data <- lapply(n_obs, function(i) {
  # t_span = seq(0, n_obs[i]*dt, dt)
  as.data.frame(vdp_oscillator(i, dt, init_conditions, mu, snr = 49)[seq(1,i,10),])
})

n_ggplot_list <- list()
for (i in seq_along(n_data)) {
  plot_title <- TeX(paste0('\\textit{n} = ', '$10^{', n_log10[i], '}$'))
  n_ggplot_list[[i]] <-
    ggplot(data = n_data[[i]], aes(x = `1`, y = `2`)) + geom_path(color = '#3b9a9c') +
    labs(x = NULL, y = NULL, title = plot_title) +
    lims(x = c(min(n_data[[3]][, 1]), max(n_data[[3]][, 1])), y = c(min(n_data[[3]][, 2]), max(n_data[[3]][, 2]))) +
    dynamics2d_theme1
}
# grid.arrange(n_ggplot_list[[1]], n_ggplot_list[[2]], n_ggplot_list[[3]], n_ggplot_list[[4]], nrow=1,ncol=4)
n_plot_all <-
  arrangeGrob(
    empty_ggplot,
    n_ggplot_list[[1]],
    n_ggplot_list[[2]],
    n_ggplot_list[[3]],
    # n_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = c(1,6,6,6,1)
  )

## vdp success rate n ---------------------------------
load("VdP/success_rate_RData/vdp_inc_n_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_n_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

x_labels <- c(
  expression(paste("10" ^ "2")),
  expression(paste("10" ^ "2.5")),
  expression(paste("10" ^ "3")),
  expression(paste("10" ^ "3.5")),
  expression(paste("10" ^ "4")),
  expression(paste("10" ^ "4.5")),
  expression(paste("10" ^ "5"))
)

x_breaks <- pretty_breaks()(n_seq)
y_breaks <-
  y_labels <- pretty_breaks()(c(0, max(total_correct$Value)))
prob_increase_n <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = expression(italic("n"))) +
  scale_x_continuous(labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(0, max(y_labels))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)
vdp_n <-
  arrangeGrob(#gglabel('k'), 
              n_plot_all, prob_increase_n, heights=height_rate_label)

## vdp success rate snr bic intercept -------------------------------------
load("VdP/success_rate_RData/vdp_inc_snr_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_snr_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

# x_labels <- x_breaks <- pretty_breaks()(c(0, max(total_correct$eta)))
x_labels <- x_breaks <- seq(1, 73, by = 12)
# x_labels[length(x_labels)] <- "Inf"
x_labels[length(x_labels)] <- TeX("$\\infty$")

y_labels <-
  y_breaks <-
  pretty_breaks()(c(min(total_correct$Value), max(total_correct$Value)))

xstart <- 65.5
xend <- 69.5
extra_x <- 1
y_sep <- min(total_correct$Value) - 0.05*(min(total_correct$Value))
myseg <- create_separators(c(xstart, xend), extra_x = 1, y = y_sep, extra_y = 0.1, angle = 75)

prob_inrease_snr_bic_inter <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = TeX("$SNR_{dB}$")) +
  scale_x_continuous(limits = c(min(x_breaks),
                                max(x_breaks)),
                     labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(NA, max(y_breaks))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)+ 
  guides(x = guide_axis_truncated(
    trunc_lower = c(-Inf, xend + extra_x/2),
    trunc_upper = c(xstart + extra_x/2, Inf)
  )) +
  annotate("segment", 
           x = myseg$x, xend = myseg$xend,
           y = myseg$y + 0.05, yend = myseg$yend)  +
  coord_cartesian(clip = "off", ylim = c(-0.0005, NA))
vdp_snr <-
  arrangeGrob(#gglabel('l'), 
              snr_plot_all, prob_inrease_snr_bic_inter, heights=height_rate_label)
## duffing plot -----------------
source('../Additional_functions/dynamical_systems_models/R/duffing_oscillator.R')

init_conditions = c(2, 0)
mu = matrix(c(-0.1,-2, 2,-0.1), 2, 2)
dt = 0.01

gamma_value = 0.1
kappa_value = 1
epsilon_value = 5
## noisy plot
n_obs2 = 5000+1
# noise_levels <- c(1, 25, 49, Inf)
# snr_volt = 10 ** -(noise_levels / 20)
t_span = seq(0, n_obs2 * dt, dt)

snr_data <- lapply(noise_levels, function(i)
  as.data.frame(
    duffing_oscillator(
      n_obs2,
      dt,
      init_conditions,
      gamma_value,
      kappa_value,
      epsilon_value,
      snr = i
    )[seq(1,n_obs2,5),]
  ))
snr_ggplot_list <- list()
for (i in seq_along(snr_data)) {
  if (noise_levels[i] == Inf) {
    plot_title <- TeX('$SNR_{dB} = \\infty^ $')
  } else{
    plot_title <- TeX(paste0('$SNR_{dB}$ = ', noise_levels[i]))
  }
  snr_ggplot_list[[i]] <-
    ggplot(data = snr_data[[i]]) + geom_path(aes(x = `1`, y = `2`), color =
                                               '#3b9a9c') +
    labs(x = NULL, y = NULL, title = plot_title) +
    lims(x = c(min(snr_data[[i]][, 1]), max(snr_data[[i]][, 1])), y = c(min(snr_data[[i]][, 2]), max(snr_data[[i]][, 2]))) +
    dynamics2d_theme1
}
# arrangeGrob
# grid.arrange(snr_ggplot_list[[1]], snr_ggplot_list[[2]], snr_ggplot_list[[3]], snr_ggplot_list[[4]], nrow=1,ncol=4)
snr_plot_all <-
  arrangeGrob(
    empty_ggplot,
    snr_ggplot_list[[1]],
    snr_ggplot_list[[2]],
    snr_ggplot_list[[3]],
    # snr_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = widths_traj
  )
## n plot
# n_obs = c(10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5)+1
n_data <- lapply(n_obs, function(i) {
  # t_span = seq(0, n_obs[i]*dt, dt)
  as.data.frame(
    duffing_oscillator(
      i,
      dt,
      init_conditions,
      gamma_value,
      kappa_value,
      epsilon_value,
      snr = 49
    )[seq(1,i,5),]
  )
})

n_ggplot_list <- list()
for (i in seq_along(n_data)) {
  plot_title <- TeX(paste0('\\textit{n} = ', '$10^{', n_log10[i], '}$'))
  n_ggplot_list[[i]] <-
    ggplot(data = n_data[[i]], aes(x = `1`, y = `2`)) + geom_path(color = '#3b9a9c') +
    labs(x = NULL, y = NULL, title = plot_title) +
    lims(x = c(min(n_data[[i]][, 1]), max(n_data[[i]][, 1])), y = c(min(n_data[[i]][, 2]), max(n_data[[i]][, 2]))) +
    dynamics2d_theme1
}

n_plot_all <-
  arrangeGrob(
    empty_ggplot,
    n_ggplot_list[[1]],
    n_ggplot_list[[2]],
    n_ggplot_list[[3]],
    # n_ggplot_list[[4]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = c(1,6,6,6,1)
  )


## duffing success rate n ---------------------------------
load("Duffing/success_rate_RData/duffing_inc_n_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_n_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

x_labels <- c(
  expression(paste("10" ^ "2")),
  expression(paste("10" ^ "2.5")),
  expression(paste("10" ^ "3")),
  expression(paste("10" ^ "3.5")),
  expression(paste("10" ^ "4")),
  expression(paste("10" ^ "4.5")),
  expression(paste("10" ^ "5"))
)

x_breaks <- pretty_breaks()(n_seq)
y_breaks <-
  y_labels <- pretty_breaks()(c(0, max(total_correct$Value)))
prob_increase_n <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = expression(italic("n"))) +
  scale_x_continuous(labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(0, max(y_labels))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)
duffing_n <-
  arrangeGrob(#gglabel('m'), 
              n_plot_all, prob_increase_n, heights=height_rate_label)

## duffing success rate snr bic intercept -------------------------------------
load("Duffing/success_rate_RData/duffing_inc_snr_success_rate_new_sg.RData")
total_correct <- total_correct_increasing_snr_df
total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
new_levels <- models_name[c(2,3,1)]
total_correct$Model <- factor(total_correct$Model, levels = new_levels)
total_correct <- arrange(total_correct, Model)
models_name <- new_levels[c(1,3,2)]

# x_labels <- x_breaks <- pretty_breaks()(c(0, max(total_correct$eta)))
x_labels <- x_breaks <- seq(1, 73, by = 12)
# x_labels[length(x_labels)] <- "Inf"
x_labels[length(x_labels)] <- TeX("$\\infty$")

y_labels <-
  y_breaks <-
  pretty_breaks()(c(min(total_correct$Value), max(total_correct$Value)))

xstart <- 65.5
xend <- 69.5
extra_x <- 1
y_sep <- min(total_correct$Value) - 0.05*(min(total_correct$Value))
myseg <- create_separators(c(xstart, xend), extra_x = 1, y = y_sep, extra_y = 0.1, angle = 75)

prob_inrease_snr_bic_inter <-
  ggplot(total_correct,
         aes(
           x = eta,
           y = Value,
           fill = Model,
           col = Model,
           shape = Model
         )) +
  geom_point(size = 4) +
  labs(y = "Success Rate",
       x = TeX("$SNR_{dB}$")) +
  scale_x_continuous(limits = c(min(x_breaks),
                                max(x_breaks)),
                     labels = x_labels,
                     breaks = x_breaks) +
  scale_y_continuous(labels = y_labels,
                     breaks = y_breaks,
                     limits = c(NA, max(y_breaks))) +
  ggplot_theme1 +
  scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)+
  guides(x = guide_axis_truncated(
    trunc_lower = c(-Inf, xend + extra_x/2),
    trunc_upper = c(xstart + extra_x/2, Inf)
  )) +
  annotate("segment", 
           x = myseg$x, xend = myseg$xend,
           y = myseg$y + 0.05, yend = myseg$yend)  +
  coord_cartesian(clip = "off", ylim = c(-0.0005, NA))
duffing_snr <-
  arrangeGrob(#gglabel('n'), 
              snr_plot_all, prob_inrease_snr_bic_inter, heights=height_rate_label)
## out plot ---------------
# linear system
## linear2d 
linear2d_exp <- ggplot()+annotate('label',x=0,y=0,
  label=TeX('$\\dot{x}_1 = -0.1x_1 + 2x_2,$\n 
    $\\dot{x}_2 = -2x_1 - 0.1x_2 .$'),
  size=10, fill = 'grey90',label.size=NA)+
  theme_nothing()
linear2d_out <- grid.arrange(
  arrangeGrob(gg_title('a',0,label_size=65,vjust=1),
              gg_title('Two-dimensional linear system',0,vjust=-0.5),
              nrow=1,widths=c(1,9)),
  arrangeGrob(linear2d_n,
              linear2d_snr,
              ncol = 2),
  linear2d_exp,
  nrow=3,
  heights=height_rate_title
)
ggsave(linear2d_out,
       filename = '../Figures/succ_rate/linear2d_systems_success_rate.pdf',
       width = 13,
       height = 7)

## linear3d 
linear3d_exp <- ggplot() +
  annotate('label', x = 0, y = 0,
    label = TeX('$\\dot{x}_1 = -0.1x_1 + 2x_2 ,$\n
      $\\dot{x}_2 = -2x_1 - 0.1x_2,$\n
      $\\dot{x}_3 = -0.3x_3.$'),
    size = 10, fill = 'grey90', label.size = NA) +
  theme_nothing()
linear3d_out <- grid.arrange(
  arrangeGrob(gg_title('b',0,label_size=65),gg_title('Three-dimensional linear system',0,vjust=-0.5),nrow=1,widths=c(1,9)),
  # gg_title('Three-dimensional linear system'),
  arrangeGrob(linear3d_n,
              linear3d_snr,
              ncol = 2),
  linear3d_exp,
  nrow = 3,
  heights = height_rate_title
)
ggsave(linear3d_out,
       filename = '../Figures/succ_rate/linear3d_systems_success_rate.pdf',
       width = 13,
       height = 7)

## cubic2d
cubic2d_exp <- ggplot() +
  annotate('label', x = 0, y = 0,
           label = TeX('$\\dot{x}_1 = -0.1x_1^{3} + 2x_2^{3},$\n
    $\\dot{x}_2 = -2x_1^{3} - 0.1x_2^{3}.$'),
    size = 10, fill = 'grey90', label.size = NA) +
  theme_nothing()
cubic2d_out <- grid.arrange(
  arrangeGrob(gg_title('c',0,label_size=65,vjust=1),gg_title('Two-dimensional cubic system',0,vjust=-0.5),nrow=1,widths=c(1,9)),
  # gg_title('Two-dimensional cubic system'),
  arrangeGrob(cubic2d_n,
              cubic2d_snr,
              ncol = 2),
  cubic2d_exp,
  nrow = 3,
  heights = height_rate_title
)
ggsave(cubic2d_out,
       filename = '../Figures/succ_rate/cubic2d_systems_success_rate.pdf',
       width = 13,
       height = 7)

## rossler
rossler_exp <- ggplot() +
  annotate('label', x = 0, y = 0,
           label = TeX('$\\dot{x}_1 = -x_2-x_3,$\n
    $\\dot{x}_2 = x_1 + 0.2x_2,$\n
    $\\dot{x}_3 = 0.2 +x_3(x_1-5.7).$'),
    size = 10, fill = 'grey90', label.size = NA) +
  theme_nothing()
rossler_out <- grid.arrange(
  arrangeGrob(gg_title('d',0,label_size=65),gg_title('Rossler system',0,vjust=-0.5),nrow=1,widths=c(1,9)),
  # gg_title('Rossler system'),
  arrangeGrob(rossler_n,
              rossler_snr,
              ncol = 2),
  rossler_exp,
  nrow = 3,
  heights = height_rate_title
)
ggsave(rossler_out,
       filename = '../Figures/succ_rate/rossler_systems_success_rate.pdf',
       width = 13,
       height = 7)

## Lorenz 
lorenz_exp <- ggplot()+
  annotate('label',x=0,y=0,label=TeX('$\\dot{x}_1 = 10(x_2-x_1),$\n
     $\\dot{x}_2 = x_1(28 - x_3) -x_2,$\n
     $\\dot{x}_3 = x_1x_2 - 8/3 x_3.$'),
    size=10, fill = 'grey90',label.size=NA)+
  theme_nothing()
lorenz_out <- grid.arrange(
  arrangeGrob(gg_title('e',0,label_size=65,vjust=1),gg_title('Lorenz system',0,vjust=-0.5),nrow=1,widths=c(1,9)),
  # gg_title('Lorenz system'),
  arrangeGrob(lorenz_n,
              lorenz_snr,
              ncol = 2),
  lorenz_exp,
  nrow = 3,
  heights = height_rate_title
)
ggsave(lorenz_out,
       filename = '../Figures/succ_rate/lorenz_systems_success_rate.pdf',
       width = 13,
       height = 7)

## vdp
vdp_exp <- ggplot()+
  annotate('label',x=0,y=0,label=TeX('$\\dot{x}_1 = x_2,$\n
    $\\dot{x}_2 = 1.2(1-x_1^2)x_2 - x_1.$'),
    size=10, fill = 'grey90', label.size=NA)+
  theme_nothing()
vdp_out <- grid.arrange(
  arrangeGrob(gg_title('f',0,label_size=65),gg_title('Van der Pol oscillator',0,vjust=-0.5),nrow=1,widths=c(1,9)),
  # gg_title('Van der Pol oscillator'),
  arrangeGrob(vdp_n,
              vdp_snr,
              ncol = 2),
  vdp_exp,
  nrow = 3,
  heights = height_rate_title
)
ggsave(vdp_out,
       filename = '../Figures/succ_rate/vdp_systems_success_rate.pdf',
       width = 13,
       height = 7)

#duffing 
duffing_exp <- ggplot()+
  annotate('label',x=0,y=0,label=TeX('$\\dot{x}_1 = x_2,$\n
    $\\dot{x}_2 = - x_2 -  x_1 - 5 x_1^3.$'),
           size=10, fill = 'grey90', label.size=NA)+
  theme_nothing()
duffing_out <- grid.arrange(
  arrangeGrob(gg_title('g',0,label_size=65,vjust=1),gg_title('Duffing oscillator',0,vjust=-0.5),nrow=1,widths=c(1,9)),
  # gg_title('Duffing oscillator'),
  arrangeGrob(duffing_n,
              duffing_snr,
              ncol = 2),
  duffing_exp,
  nrow = 3,
  heights = height_rate_title
)
ggsave(duffing_out,
       filename = '../Figures/succ_rate/duffing_systems_success_rate.pdf',
       width = 13,
       height = 7)


