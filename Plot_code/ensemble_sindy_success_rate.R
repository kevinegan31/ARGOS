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
## main ---------------
noise_levels <- c(1, 25, Inf)
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
height_rate_label <- c(8,20)
# height_rate_title <- c(1,8,1)
height_rate_title <- c(1,8)
widths_traj <- c(1,6,6,6,1)
colors_correct <- c("#9c72be", "#87a14d", "#cb8a69")
shapes <- c(15, 16, 17)
rect1<- data.frame(xmin=-Inf, xmax=Inf, ymin=0.8, ymax=Inf)
## ggplot theme ------------------
ggplot_theme0 <- theme(
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
  legend.position = "none",
  legend.text.align = 0
)
ggplot_theme <- ggplot_theme0 + theme(plot.background = element_rect(fill = "#f4f0e6", colour = 0))
ggplot_theme1 <- ggplot_theme0 + theme(plot.background = element_rect(fill = 0, colour = 0))

dynamics2d_theme0 <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
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
## lorenz plot -----------------
source('../Additional_functions/dynamical_systems_models/R/lorenz_system.R')

init_conditions = c(2, 0, 1)
mu = matrix(c(-0.1,-2, 0,
              2,-0.1, 0,
              0, 0,-0.3), 3, 3)
dt = 0.01

## noisy plot
n_obs2 = 5000+1
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
    plot_title <- TeX('$SNR = \\infty^ $')
  } else{
    plot_title <- TeX(paste0('SNR = ', noise_levels[i], ' dB'))
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
snr_plot_all <-
  arrangeGrob(
    empty_ggplot,
    snr_ggplot_list[[1]],
    snr_ggplot_list[[2]],
    snr_ggplot_list[[3]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = widths_traj
  )
## n plot
n_data <- lapply(n_obs, function(i) {
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

n_plot_all <-
  arrangeGrob(
    empty_ggplot,
    n_ggplot_list[[1]],
    n_ggplot_list[[2]],
    n_ggplot_list[[3]],
    empty_ggplot,
    nrow = 1,
    ncol = 5,
    widths = c(1,6,6,6,1)
  )


## lorenz success rate n ---------------------------------
load("Lorenz/success_rate_RData/ensemble_sindy_increasing_n_success_rate_snr_49_n_models_100_updated_seed.RData")
total_correct <- total_correct_increasing_n_df
# total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
# new_levels <- models_name[c(2,3,1)]
# total_correct$Model <- factor(total_correct$Model, levels = new_levels)
# total_correct <- arrange(total_correct, Model)
# models_name <- new_levels[c(1,3,2)]
colors_correct <- c("#9a963f",
                    "#7878cd",
                    "#64ac48",
                    "#c45ca2",
                    "#4aad92",
                    "#cc5452",
                    "#c8803e")
shapes <- c(15, 16, 17,
            15, 16, 17, 15)
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
  ggplot() +
  geom_hline(yintercept = 0.8, lty=2)+
  geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.2,fill="#9de0e6")+
  geom_point(data=total_correct,
             aes(
               x = eta,
               y = Value,
               fill = Model,
               col = Model,
               shape = Model
             ),size = 3) +
  geom_line(data=total_correct,
            aes(
              x = eta,
              y = Value,
              col = Model,
            ),linewidth = 0.5) +
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
# lorenz_n <-
#   arrangeGrob(#n_plot_all,
#               prob_increase_n, heights = height_rate_label)
lorenz_n <-
  arrangeGrob(#n_plot_all,
    prob_increase_n)

## lorenz success rate snr bic intercept -------------------------------------
load("Lorenz/success_rate_RData/ensemble_sindy_increasing_snr_success_rate_N5000_n_models_100.RData")
total_correct <- total_correct_increasing_snr_df
total_correct$eta[total_correct$eta == Inf] <- 73
# total_correct[which(total_correct$Model == 'STLS'), ]$Model <- 'SINDy-AIC'
models_name <- unique(total_correct$Model)
# new_levels <- models_name[c(2,3,1)]
# total_correct$Model <- factor(total_correct$Model, levels = new_levels)
# total_correct <- arrange(total_correct, Model)
# models_name <- new_levels[c(1,3,2)]

x_labels <- x_breaks <- seq(1, 73, by = 12)
x_labels[length(x_labels)] <- TeX("$\\infty$")

y_labels <-
  y_breaks <-
  pretty_breaks()(c(min(total_correct$Value), max(total_correct$Value)))

xstart <- 65.5
xend <- 69.5
extra_x <- 1
y_sep <- min(total_correct$Value) - 0.05*(min(total_correct$Value))
myseg <- create_separators(c(xstart, xend), extra_x = 1, y = y_sep, extra_y = 0.1, angle = 75)

# prob_inrease_snr_bic_inter <-
#   ggplot() +
#   geom_hline(yintercept = 0.8, lty=2)+
#   geom_rect(data=rect1,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.2,fill="#9de0e6")+
#   geom_point(data=total_correct,
#              aes(
#                x = eta,
#                y = Value,
#                fill = Model,
#                col = Model,
#                shape = Model
#              ),size = 2) +
#   geom_line(data=total_correct,
#             aes(
#               x = eta,
#               y = Value,
#               col = Model,
#             ),linewidth = 0.5) +
#   labs(y = "Success Rate",
#        x = TeX("SNR(dB)")) +
#   scale_x_continuous(limits = c(min(x_breaks),
#                                 max(x_breaks)),
#                      labels = x_labels,
#                      breaks = x_breaks) +
#   scale_y_continuous(labels = y_labels,
#                      breaks = y_breaks,
#                      limits = c(NA, max(y_breaks))) +
#   ggplot_theme1 +
#   scale_fill_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
#   scale_colour_manual(values = colors_correct, breaks = models_name, labels  = models_name) +
#   scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name)+
#   guides(x = guide_axis_truncated(
#     trunc_lower = c(-Inf, xend + extra_x/2),
#     trunc_upper = c(xstart + extra_x/2, Inf)
#   )) +
#   annotate("segment", 
#            x = myseg$x, xend = myseg$xend,
#            y = myseg$y + 0.05, yend = myseg$yend)  +
#   coord_cartesian(clip = "off", ylim = c(-0.0005, NA))
# Create a subset of the data excluding the part where eta is between 61 and 73
total_correct_sub <- subset(total_correct, !(eta > 61 & eta <= 73))

# Create the ggplot
prob_inrease_snr_bic_inter <-
  ggplot() +
  geom_hline(yintercept = 0.8, lty = 2) +
  geom_rect(data = rect1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0.2, fill = "#9de0e6") +
  geom_point(data = total_correct,  # using the original data set to maintain all points
             aes(
               x = eta,
               y = Value,
               fill = Model,
               col = Model,
               shape = Model
             ), size = 3) +
  geom_line(data = total_correct_sub,  # using the subset to exclude the segment
            aes(
              x = eta,
              y = Value,
              col = Model
            ), linewidth = 0.5) +
  labs(y = "Success Rate",
       x = TeX("SNR(dB)")) +
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
  scale_shape_manual(values = shapes, breaks = models_name, labels  = models_name) +
  guides(x = guide_axis_truncated(
    trunc_lower = c(-Inf, xend + extra_x / 2),
    trunc_upper = c(xstart + extra_x / 2, Inf)
  )) +
  annotate("segment", 
           x = myseg$x, xend = myseg$xend,
           y = myseg$y + 0.05, yend = myseg$yend)  +
  coord_cartesian(clip = "off", ylim = c(-0.0005, NA))

# Print the plot
print(prob_inrease_snr_bic_inter)


# lorenz_snr <-
#   arrangeGrob(#snr_plot_all,
#               prob_inrease_snr_bic_inter, heights = height_rate_label)
lorenz_snr <-
  arrangeGrob(#snr_plot_all,
    prob_inrease_snr_bic_inter)


## Lorenz -----------------------------
lorenz_exp <- ggplot()+
  annotate('label',x=0,y=0,label=TeX('$\\dot{x}_1 = 10(x_2-x_1),$\n
     $\\dot{x}_2 = x_1(28 - x_3) -x_2,$\n
     $\\dot{x}_3 = x_1x_2 - 8/3 x_3.$'),
           size=10, fill = 'grey90',label.size=NA)+
  theme_nothing()
# height_rate_title <- c(3,1)
# height_rate_title <- c(1,8)
# height_rate_title <- c(8,1)
# lorenz_out <- grid.arrange(
#   arrangeGrob(#gg_title('f',0,label_size=65,vjust=0.3),
#               top = gg_title('Lorenz',0,vjust=-0.5),
#               nrow=1,widths=c(1,9)),
#   arrangeGrob(lorenz_n,
#               lorenz_snr,
#               ncol = 2),
#   lorenz_exp,
#   nrow = 2,
#   heights = height_rate_title
# )
# Combine the lorenz_n and lorenz_snr horizontally
# # Assemble plots and title horizontally
lorenz_plots <- arrangeGrob(lorenz_n, lorenz_snr, ncol = 2)
# 
# Assemble everything
gg_title <- function(label, bg_col = "#f4f0e6", label_size = 52, vjust=0){ # the label of each plot
  ggplot() + ggtitle(label) + theme(plot.title = element_text(size = label_size,face = "bold",vjust=vjust),
                                    plot.margin = unit(c(2,0,0,0),'pt'),
                                    plot.background = element_rect(fill = bg_col, color = bg_col),
                                    panel.background = element_rect(fill = bg_col, color = bg_col))
}
lorenz_out <- grid.arrange(
  arrangeGrob(gg_title('Lorenz', 0, vjust = -0.5),
              nrow = 1,
              heights = 2), # Adjust as needed
  lorenz_plots,
  lorenz_exp,
  nrow = 3,
  heights = c(1, 4, 1)  # Adjust these numbers
)
# title_size <- 36
# label_size <- 65
# lorenz_out <- arrangeGrob(
#   arrangeGrob(#gg_title('a',0,label_size=label_size,vjust=1),
#               gg_title('Lorenz',0,vjust=-0.5),nrow=1,widths=c(1,9)),
#   arrangeGrob(lorenz_n,
#               lorenz_snr,
#               ncol = 2),
#   nrow=2,
#   heights=height_rate_title
# )
# ggsave(lorenz_out,
#        filename = '/Users/kevinegan/Documents/GitHub.nosync/PrivateAutomaticSparseRegression/Figures/succ_rate/ensemble_sindy_lorenz_systems_success_rate.pdf',
#        width = 13,
#        height = 7)


ggsave(lorenz_out,
       filename = '/Users/kevinegan/Documents/GitHub.nosync/PrivateAutomaticSparseRegression/Figures/succ_rate/ensemble_sindy_lorenz_systems_success_rate_updated_seed.pdf',
       width = 13,
       height = 7)

legend_levels <- unique(total_correct$Model)
# c("Lasso", "Adaptive Lasso", "SINDy-AIC", "ESINDy-Bagging",
#   "ESINDy-Bragging", "Lib-ESINDy-Bagging", "Lib-ESINDy-Bragging")
total_correct$Model <- factor(total_correct$Model,
                              levels = legend_levels)


# lines are for model
legend_ggplot <- ggplot(total_correct, aes(x = eta, y = Value,
                                           fill = Model,
                                           col = Model,
                                           shape = Model)) +
  geom_point(size = 4) +
  geom_line(linewidth = 0.5) +
  # geom_bar(stat = "identity", position = "dodge", width = 0.0065) +
  # geom_bar(stat="identity", position=position_dodge(0.7),width=0.5) +
  ylab("Identification Probability") +
  xlab(TeX("$\\eta$")) +
  ggtitle("Two-dimensional damped linear oscillator") +
  # scale_x_continuous(limits = c(0, 0.3),
  #                    labels = seq(0, 0.3, by = 0.1)) +
  # scale_x_discrete(
  #   breaks = x_breaks
  #   # labels = x_limits
  #   # limits = x_limits
  # ) +
  scale_x_continuous(#limits = c(-0.01, 0.11),
    #expand = c(0, 0),
    labels = seq(0, 0.1, by = 0.01),
    breaks = seq(0, 0.1, by = 0.01)) +
  scale_y_continuous(#limits = c(0, 0.3),
    #labels = seq(0, 0.3, by = 0.1),
    expand = expansion(mult = c(0, .05))) +
  theme(
    axis.line = element_line(colour = "black"),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.key = element_blank(),
    # legend.text = element_text(size = 12),
    legend.text = element_text(size = 10),
    # Changed for legend
    legend.title = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    axis.text = element_text(size = 16),
    axis.title.x = element_text(size = 36),
    axis.title.y = element_text(
      size = 20,
      angle = 90,
      vjust = 0.5
    ),
    plot.title = element_text(size = 20),
    legend.position = "bottom",
    legend.text.align = 0
  ) +
  scale_fill_manual(values = colors_correct) +
  scale_colour_manual(values = colors_correct) +
  scale_shape_manual(values = shapes)

legend <- get_legend(legend_ggplot)

cowplot::plot_grid(legend)
