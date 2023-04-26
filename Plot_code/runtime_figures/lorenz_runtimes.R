rm(list = ls())
library(tidyr)
library(tidyverse)
library(scales)
library(latex2exp)
library(cowplot)
library(gtools)
file_wd <- paste(
  "~/ARGOS",
  sep = ""
)
file_wd2 <- paste(file_wd, "./Data/run_time/", sep = "")
setwd(file_wd2)
num_init <- 30

total_df <- read.csv('lorenz_run_time_data.csv')


total_gather_df <- total_df %>%
  gather("model", "value")

total_gather_df <- total_gather_df %>%
  separate(model, c('model', 'n'))

total_gather_df$model <-
  factor(total_gather_df$model,
         levels = c("alasso", "lasso", "sindy"),
         labels = c("ARGOS-Adaptive Lasso", "ARGOS-Lasso", "SINDy-AIC"))

total_gather_filtered_df <- total_gather_df %>%
  dplyr::filter(n %in% c("300", "350", "400", "450", "500"))
total_gather_filtered_df$n <- as.numeric(total_gather_filtered_df$n)

total_gather_filtered_df$n <-
  factor(total_gather_filtered_df$n,
         levels = unique(total_gather_filtered_df$n))



colors <-
  c("#9c72be",
    "#87a14d",
    "#cb6054"
  )

x_labels <-  c(expression(paste("10" ^ "3")),
               expression(paste("10" ^ "3.5")),
               expression(paste("10" ^ "4")),
               expression(paste("10" ^ "4.5")),
               expression(paste("10" ^ "5")))
y_breaks <- trans_breaks("log10", function(x)
  10 ^ x)(c(min(
    purrr::discard(total_gather_filtered_df$value, is.na)
  ), max(
    purrr::discard(total_gather_filtered_df$value, is.na)
  )))
y_breaks <-
  y_labels <- pretty_breaks()(c(0, max(total_gather_filtered_df$value)))


total_gather_filtered_sindy <- total_gather_filtered_df %>%
  dplyr::filter(model ==  "SINDy-AIC")
total_gather_filtered_sindy$n <- as.numeric(total_gather_filtered_sindy$n)

total_gather_filtered_sindy2 <- total_gather_filtered_sindy
total_gather_filtered_sindy2$n <- (total_gather_filtered_sindy$n*0.5 + 1.5)
total_gather_filtered_sindy2$value <- log10(total_gather_filtered_sindy$value)
lm(value ~ poly(n, 2), data = total_gather_filtered_sindy2) # log10T ~ 3.009 + 13.052log10n + 2.433(log10n)^2

total_gather_filtered_lasso <- total_gather_filtered_df %>%
  dplyr::filter(model ==  "ARGOS-Lasso")
total_gather_filtered_lasso$n <- as.numeric(total_gather_filtered_lasso$n)

total_gather_filtered_lasso2 <- total_gather_filtered_lasso
total_gather_filtered_lasso2$n <- (total_gather_filtered_lasso$n*0.5 + 1.5)
total_gather_filtered_lasso2$value <- log10(total_gather_filtered_lasso$value)
lm(value ~ poly(n, 2), data = total_gather_filtered_lasso2) # log10T ~ 3.256 + 6.285log10n + 2.313(log10n)^2


total_gather_filtered_alasso <- total_gather_filtered_df %>%
  dplyr::filter(model ==  "ARGOS-Adaptive Lasso")
total_gather_filtered_alasso$n <- as.numeric(total_gather_filtered_alasso$n)

total_gather_filtered_alasso2 <- total_gather_filtered_alasso
total_gather_filtered_alasso2$n <- (total_gather_filtered_alasso$n*0.5 + 1.5)
total_gather_filtered_alasso2$value <- log10(total_gather_filtered_alasso$value)
lm(value ~ poly(n, 2), data = total_gather_filtered_alasso2) # log10T ~ 3.597 + 6.259log10n + 1.897(log10n)^2

breaks_log10 <- function(x) {
  low <- floor(log10(min(x)))
  high <- ceiling(log10(max(x)))
  
  10^(seq.int(low, high))
}

runtime_ggplot <-
  ggplot() +
  geom_boxplot(data = total_gather_filtered_df, aes(x = n, y = value, fill = model), lwd = 0.3) +
  geom_smooth(data = total_gather_filtered_alasso, aes(x = n, y = value),
              method='lm', formula='y ~ poly(x,2)', alpha = 0.5, lty=2, color = colors[1], se = FALSE) +
  geom_smooth(data = total_gather_filtered_lasso, aes(x = n, y = value),
              method='lm', formula='y ~ poly(x, 2)', alpha = 0.5, lty=2, color = colors[2], se = FALSE) +
  geom_smooth(data = total_gather_filtered_sindy, aes(x = n, y = value),
              method='lm', formula='y ~ poly(x, 2)', alpha = 0.5, lty=2, color = colors[3], se = FALSE) +
  annotate(geom = "text", x = 1, y = 3200,
           label = TeX('$\\sim 0.42(log_{10}n)^2$', output = 'character'),
           parse = TRUE,size = 9
  ) +
  annotate(geom = "text", x = 1, y = 650,
           label = TeX('$\\sim 0.21(log_{10}n)^2$', output = 'character'),
           parse = TRUE, size = 9
  ) +
  annotate(geom = "text", x = 2.5, y = 250,
           label = TeX('$\\sim 0.724(log10n)^2$', output = 'character'),
           parse = TRUE, size = 9
  ) +
  scale_y_log10(breaks = breaks_log10,
              labels = trans_format(log10, math_format(10^.x))) +
  scale_x_discrete(drop = FALSE,
                   labels = x_labels) +
  labs(x = TeX("$\\textit{n}$"),
       y = "Time [s]",
       tag='b',
       title='Lorenz') +
  theme(
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
    axis.title.x = element_text(size = 28),
    axis.title.y = element_text(
      size = 28,
      angle = 90,
      vjust = 0.5
    ),
    plot.title = element_text(size = 45,face='bold'),
    plot.tag = element_text(size = 55, vjust=-3,face='bold'),
    legend.position = "none",
    legend.text.align = 0,
    plot.margin = unit(c(-35,0,0,0),'pt')
  ) +
  scale_fill_manual(values = colors) +
  scale_colour_manual(values = colors)

runtime_ggplot

colours <-
  c("#9c72be",
    "#87a14d",
    "#cb6054")
shapes <- c(15, 16, 17)
legend_ggplot <-
  ggplot(total_gather_filtered_df, aes(x = n,
                                       y = value,
                                       fill = model,
                                       col = model)) +
  geom_line(linewidth = 5) +
  labs(y = "Identification Probability",
       x = expression(italic("n"))) +
  theme(
    axis.line = element_line(colour = "black"),
    axis.ticks.length = unit(.25, "cm"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 12),
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
  scale_fill_manual(values = colours) +
  scale_colour_manual(values = colours)

legend <- get_legend(legend_ggplot)

cowplot::plot_grid(legend)
