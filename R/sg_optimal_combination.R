library(signal)
library(Metrics)
### SG Combination Finder
sg_optimal_combination <- function(x_t, dt = 1, polyorder) {
  ### Need new name for special_digit
  dt <- dt
  ### Create Combinations
  wl_max <- round((nrow(as.matrix(x_t)) * 0.05), 0)
  if (round((wl_max %% 2), 0) == 0) {
    wl_max <- wl_max + 1
  }
  ### Polynomial Order
  if (missing(polyorder)) {
    polyorder <- 4
  }
  ### If the Window length calculation is less than 11
  ### we will just try the two minimum values.
  if (wl_max < 13) {
    ### Combinations
    sg_combinations <- cbind(4, 13)
  } else {
    ### Combinations
    if (wl_max > 101) {
      window_length <- seq(5, 101, by = 2)
    } else {
      if (wl_max %% 2 == 0) {
        wl_max <- wl_max + 1
        window_length <- seq(5, wl_max, by = 2)
      } else {
        window_length <- seq(5, wl_max, by = 2)
      }
    }
    sg_combinations <- expand_grid(polyorder, window_length) %>%
      subset(window_length > polyorder + 7 - polyorder%%2) %>%
      as.matrix()
    if (nrow(sg_combinations) == 1) {
      sg_combinations <- cbind(4, 13)
    }
  }
  ### Determine Frobenius Norm for Combinations
  f_dist <- list()
  for (i in seq_along(sg_combinations[, 1])) {
    ### Smooth data
    x_t_smoothed <- x_t %>% sgolayfilt(p = sg_combinations[i, 1], n = sg_combinations[i, 2], m = 0, ts = dt)
    f_dist[[i]] <- Metrics::mse(x_t,
                                x_t_smoothed)
  }
  f_dist_df <- as.data.frame(do.call(rbind,
                                     lapply(f_dist,
                                            unlist)))
  colnames(f_dist_df)[1] <- 'f_dist'
  sg_best_combination <- which.min(f_dist_df$f_dist)
  sg_order_wl <- cbind(sg_combinations[which.min(f_dist_df$f_dist), 1],
                       sg_combinations[which.min(f_dist_df$f_dist), 2])
  return(list(sg_combinations = sg_combinations,
              sg_order_wl = sg_order_wl,
              f_dist = f_dist_df))
}
