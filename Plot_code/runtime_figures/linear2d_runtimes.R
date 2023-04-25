### Create new data
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
library(gtools)
file_wd <- paste(
  "~/ARGOS",
  sep = ""
)
file_wd2 <- paste(file_wd, "./Data/Linear2D/", sep = "")
setwd(file_wd2)
num_init <- 30

### lasso ----
lasso_model_grid_pattern <-
  list.files(
    path = paste(
      "./Lasso/",
      "Runtimes/",
      sep = ""
    ),
    pattern = "*runtime_df.RData",
    full.names = TRUE
  )
### xdot
xdot_lasso_grid_files <- mixedsort(str_subset(lasso_model_grid_pattern, "xdot"))
xdot_lasso_rt_list <- list()
for (i in seq_along(xdot_lasso_grid_files)) {
  xdot_lasso_rt_df <- load(xdot_lasso_grid_files[i])
  xdot_runtime_new <- sapply(xdot_run_time, FUN = function(x) {
    if (units(x) == "hours") {
      a <- as.numeric(x) * 60 * 60
    } else if (units(x) == "mins") {
      a <- as.numeric(x) * 60
    } else {
      a <- as.numeric(x)
    }
    return(a)
  })
  if (length(xdot_runtime_new) > num_init) {
    new_xdot_runtime <- xdot_runtime_new[c(1:num_init)]
  } else {
    new_xdot_runtime <- xdot_runtime_new
  }
  xdot_lasso_rt_list[[i]] <- new_xdot_runtime
}

ydot_lasso_grid_files <- mixedsort(str_subset(lasso_model_grid_pattern, "ydot"))
ydot_lasso_rt_list <- list()
for (i in seq_along(ydot_lasso_grid_files)) {
  ydot_lasso_rt_df <- load(ydot_lasso_grid_files[i])
  ydot_runtime_new <- sapply(ydot_run_time, FUN = function(x) {
    if (units(x) == "hours") {
      a <- as.numeric(x) * 60 * 60
    } else if (units(x) == "mins") {
      a <- as.numeric(x) * 60
    } else {
      a <- as.numeric(x)
    }
    return(a)
  })
  if (length(ydot_runtime_new) > num_init) {
    new_ydot_runtime <- ydot_runtime_new[c(1:num_init)]
  } else {
    new_ydot_runtime <- ydot_runtime_new
  }
  ydot_lasso_rt_list[[i]] <- new_ydot_runtime
}


xdot_lasso_rt_vector <- as.matrix(unlist(xdot_lasso_rt_list))
ydot_lasso_rt_vector <- as.matrix(unlist(ydot_lasso_rt_list))
lasso_runtime_vector <- xdot_lasso_rt_vector + ydot_lasso_rt_vector
lasso_runtime_df <- data.frame(matrix(lasso_runtime_vector, nrow = num_init))

### alasso ----
alasso_model_grid_pattern <-
  list.files(
    path = paste(
      "./adaptive_lasso_ridgew/",
      "Runtimes/",
      sep = ""
    ),
    pattern = "*runtime_df.RData",
    full.names = TRUE
  )
### xdot
xdot_alasso_grid_files <- mixedsort(str_subset(alasso_model_grid_pattern, "xdot"))
xdot_alasso_rt_list <- list()
for (i in seq_along(xdot_alasso_grid_files)) {
  xdot_alasso_rt_df <- load(xdot_alasso_grid_files[i])
  xdot_runtime_new <- sapply(xdot_run_time, FUN = function(x) {
    if (units(x) == "hours") {
      a <- as.numeric(x) * 60 * 60
    } else if (units(x) == "mins") {
      a <- as.numeric(x) * 60
    } else {
      a <- as.numeric(x)
    }
    return(a)
  })
  if (length(xdot_runtime_new) > num_init) {
    new_xdot_runtime <- xdot_runtime_new[c(1:num_init)]
  } else {
    new_xdot_runtime <- xdot_runtime_new
  }
  xdot_alasso_rt_list[[i]] <- new_xdot_runtime
}

ydot_alasso_grid_files <- mixedsort(str_subset(alasso_model_grid_pattern, "ydot"))
ydot_alasso_rt_list <- list()
for (i in seq_along(ydot_alasso_grid_files)) {
  ydot_alasso_rt_df <- load(ydot_alasso_grid_files[i])
  ydot_runtime_new <- sapply(ydot_run_time, FUN = function(x) {
    if (units(x) == "hours") {
      a <- as.numeric(x) * 60 * 60
    } else if (units(x) == "mins") {
      a <- as.numeric(x) * 60
    } else {
      a <- as.numeric(x)
    }
    return(a)
  })
  if (length(ydot_runtime_new) > num_init) {
    new_ydot_runtime <- ydot_runtime_new[c(1:num_init)]
  } else {
    new_ydot_runtime <- ydot_runtime_new
  }
  ydot_alasso_rt_list[[i]] <- new_ydot_runtime
}

xdot_alasso_rt_vector <- as.matrix(unlist(xdot_alasso_rt_list))
ydot_alasso_rt_vector <- as.matrix(unlist(ydot_alasso_rt_list))
alasso_runtime_vector <- xdot_alasso_rt_vector + ydot_alasso_rt_vector
alasso_runtime_df <- data.frame(matrix(alasso_runtime_vector, nrow = num_init))

### sindy ----
sindy_model_grid_pattern <-
  list.files(
    path = paste("./sindy/",
                 "Runtimes/",
                 sep = ""),
    pattern = "*runtime_df.csv",
    full.names = TRUE
  )
sindy_runtimes_list <- list()
for (i in seq_along(sindy_model_grid_pattern)) {
  sindy_df <-
    read.csv(sindy_model_grid_pattern[i])[-1]
  sindy_runtimes_list[[i]] <- sindy_df
}
sindy_runtime_matrix <- as.matrix(unlist(sindy_runtimes_list))
sindy_runtime_df <-
  data.frame(matrix(sindy_runtime_matrix, nrow = num_init))*20

#### Plots ----
n_seq <- seq(200, 500, 50)
colnames(lasso_runtime_df) <- paste("lasso", n_seq)
colnames(alasso_runtime_df) <- paste("alasso", n_seq)
colnames(sindy_runtime_df) <- paste("sindy", n_seq)

# Combine Dataframes
total_df <-
  as.data.frame(
    cbind(
      alasso_runtime_df,
      lasso_runtime_df,
      sindy_runtime_df
    )
  )

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
lm(value ~ poly(n, 1), data = total_gather_filtered_sindy2) # log10T ~ 2.157 + 8.106log10n

total_gather_filtered_lasso <- total_gather_filtered_df %>%
  dplyr::filter(model ==  "ARGOS-Lasso")
total_gather_filtered_lasso$n <- as.numeric(total_gather_filtered_lasso$n)

total_gather_filtered_lasso2 <- total_gather_filtered_lasso
total_gather_filtered_lasso2$n <- (total_gather_filtered_lasso$n*0.5 + 1.5)
total_gather_filtered_lasso2$value <- log10(total_gather_filtered_lasso$value)
lm(value ~ poly(n, 2), data = total_gather_filtered_lasso2) # log10T ~ 3.042 + 6.270log10n + 2.709(log10n)^2

total_gather_filtered_alasso <- total_gather_filtered_df %>%
  dplyr::filter(model ==  "ARGOS-Adaptive Lasso")
total_gather_filtered_alasso$n <- as.numeric(total_gather_filtered_alasso$n)

total_gather_filtered_alasso2 <- total_gather_filtered_alasso
total_gather_filtered_alasso2$n <- (total_gather_filtered_alasso$n*0.5 + 1.5)
total_gather_filtered_alasso2$value <- log10(total_gather_filtered_alasso$value)
lm(value ~ poly(n, 2), data = total_gather_filtered_alasso2) # log10T ~ 3.396 + 6.531log10n + 2.025(log10n)^2

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
              method='lm', formula='y ~ poly(x, 1)', alpha = 0.5, lty=2, color = colors[3], se = FALSE) +
  annotate(geom = "text", x = 1.15, y = 3100,
    label = TeX('$\\sim 0.76(log_{10}n)^2$', output = 'character'),
    parse = TRUE,size = 9
  ) +
  annotate(geom = "text", x = 1.25, y = 300,
    label = TeX('$\\sim 0.72(log_{10}n)^2$', output = 'character'),
    parse = TRUE, size = 9
  ) +
  annotate(geom = "text", x = 1.75, y = 80,
    label = TeX('$\\sim 4.6log_{10}n$', output = 'character'),
    parse = TRUE, size = 9
  ) +
  scale_y_log10(breaks = breaks_log10,
              labels = trans_format(log10, math_format(10^.x))) +
  scale_x_discrete(drop = FALSE,
                   labels = x_labels) +
  labs(x = TeX("$\\textit{n}$"),
       y = "Time [s]",
       tag='a',
       title='Two-dimensional linear') +
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
