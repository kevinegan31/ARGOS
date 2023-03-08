rm(list = ls())
library(tidyr)
library(tidyverse)
library(scales)
library(latex2exp)
library(cowplot)
library(ggh4x)
file_wd <-  "C:/Users/cfzh32/Documents/GitHub/ARGOS/"# Github path
file_wd2 <- paste(file_wd, "Data/Linear2D/", sep = "") # 
setwd(file_wd2)
### Read functions
source("../../Additional_functions/dynamical_systems_models/R/linear2d_system.R")
source('../../Create_data/functions/get_candidate_names.R')
source("../../Create_data/functions/conver_functions.R")
linear2d_col_names <- get_can_names(linear2d_system(100,c(2,0),0.01))
prediction_model_names <- paste(linear2d_col_names, "_", sep="")
prediction_model_names[1] <- "X1_"
eta_final <- 61
eta_start <- 1
eta <- c(seq(eta_start, eta_final, by = 3), Inf)
eta_seq <- rep(eta, each = 100)

## read files -----------------------------------------
x_dot_lasso_ci <- read.csv('pred_model_csv/SNR/linear2d_inc_snr_xdot_lasso_ci_df.csv')[,-1]
x_dot_lasso_pe <- read.csv('pred_model_csv/SNR/linear2d_inc_snr_xdot_lasso_pe_df.csv')[,-1]
y_dot_lasso_ci <- read.csv('pred_model_csv/SNR/linear2d_inc_snr_ydot_lasso_ci_df.csv')[,-1]
y_dot_lasso_pe <- read.csv('pred_model_csv/SNR/linear2d_inc_snr_ydot_lasso_pe_df.csv')[,-1]

x_dot_alasso_ci <- read.csv('pred_model_csv/SNR/linear2d_inc_snr_xdot_alasso_ci_df.csv')[,-1]
x_dot_alasso_pe <- read.csv('pred_model_csv/SNR/linear2d_inc_snr_xdot_alasso_pe_df.csv')[,-1]
y_dot_alasso_ci <- read.csv('pred_model_csv/SNR/linear2d_inc_snr_ydot_alasso_ci_df.csv')[,-1]
y_dot_alasso_pe <- read.csv('pred_model_csv/SNR/linear2d_inc_snr_ydot_alasso_pe_df.csv')[,-1]

x_dot_pysindy_pe <- read.csv('pred_model_csv/SNR/linear2d_inc_snr_xdot_pysindy_pe_df.csv')[,-1]
y_dot_pysindy_pe <- read.csv('pred_model_csv/SNR/linear2d_inc_snr_ydot_pysindy_pe_df.csv')[,-1]

## Generate Prediction Models -------------------------
xdot_lasso_prediction_model <- out_csv2bar_csv_argos_snr(x_dot_lasso_pe, x_dot_lasso_ci, eta_seq)
ydot_lasso_prediction_model <- out_csv2bar_csv_argos_snr(y_dot_lasso_pe, y_dot_lasso_ci, eta_seq)

xdot_alasso_prediction_model <- out_csv2bar_csv_argos_snr(x_dot_alasso_pe, x_dot_alasso_ci, eta_seq)
ydot_alasso_prediction_model <- out_csv2bar_csv_argos_snr(y_dot_alasso_pe, y_dot_alasso_ci, eta_seq)

############ create RData for success rate plots ---------------------
### lasso ---------------------
### xdot
### Update Colnames
### Count number of times model is correct per 100 rows
xdot_lasso_correct_rows <- rep(NA, nrow(xdot_lasso_prediction_model))
for (i in seq_len(nrow(xdot_lasso_prediction_model))) {
  if (
    xdot_lasso_prediction_model[i,]$x != 0 &&
    xdot_lasso_prediction_model[i,]$y != 0 &&
    rowSums(xdot_lasso_prediction_model[i,][, !names(xdot_lasso_prediction_model) %in% c("x", "y")]
            != 0) == 0) {
    xdot_lasso_correct_rows[i] <- 1
  } else {
    xdot_lasso_correct_rows[i] <- 0
  }
}
### df
xdot_lasso_eta_df <-
  as.data.frame(matrix(xdot_lasso_correct_rows, nrow = 100))
colnames(xdot_lasso_eta_df) <- paste("eta_", eta)
### ydot
ydot_lasso_correct_rows <- rep(NA, nrow(ydot_lasso_prediction_model))
for (i in seq_len(nrow(ydot_lasso_prediction_model))) {
  if (
    ydot_lasso_prediction_model[i,]$x != 0 &&
    ydot_lasso_prediction_model[i,]$y != 0 &&
    rowSums(ydot_lasso_prediction_model[i,][, !names(ydot_lasso_prediction_model) %in% c("x", "y")]
            != 0) == 0) {
    ydot_lasso_correct_rows[i] <- 1
  } else {
    ydot_lasso_correct_rows[i] <- 0
  }
}
### df
ydot_lasso_eta_df <-
  as.data.frame(matrix(ydot_lasso_correct_rows, nrow = 100))
# Rename columns
colnames(ydot_lasso_eta_df) <- paste("eta_", eta) 
###
lasso_total_correct_eta <- rep(NA, nrow(xdot_lasso_eta_df))
lasso_total_correct_eta_df <- as.data.frame(matrix(NA, ncol = ncol(xdot_lasso_eta_df), nrow = 100))
for (i in seq_len(ncol(xdot_lasso_eta_df))) {
  new_df <- cbind(xdot_lasso_eta_df[, i],
                  ydot_lasso_eta_df[, i])
  for (j in seq_len(nrow(new_df))) {
    lasso_identification <- rep(NA, nrow(xdot_lasso_eta_df))
    if (any(new_df[j, ] == 0)) {
      lasso_total_correct_eta[j] <- 0
    } else {
      lasso_total_correct_eta[j] <- 1
    }
  }
  lasso_total_correct_eta_df[, i] <- lasso_total_correct_eta
}
colnames(lasso_total_correct_eta_df) <- eta

lasso_count_vector <-  data.frame(eta = colnames(lasso_total_correct_eta_df),
                                  Correct = colSums(lasso_total_correct_eta_df == 1) / 100,
                                  Incorrect = colSums(lasso_total_correct_eta_df == 0) / 100)
rownames(lasso_count_vector) <- seq(1, nrow(lasso_count_vector))
lasso_eta_gather_df <- lasso_count_vector %>%
  gather("Condition", "Value",
         2:ncol(lasso_count_vector))
lasso_eta_gather_df$eta <-
  factor(lasso_eta_gather_df$eta, levels = unique(lasso_eta_gather_df$eta))

### alasso ------------------------------
### xdot
### Update Colnames
### Count number of times model is correct per 100 rows
xdot_alasso_correct_rows <- rep(NA, nrow(xdot_alasso_prediction_model))
for (i in seq_len(nrow(xdot_alasso_prediction_model))) {
  if (
    xdot_alasso_prediction_model[i,]$x != 0 &&
    xdot_alasso_prediction_model[i,]$y != 0 &&
    rowSums(xdot_alasso_prediction_model[i,][, !names(xdot_alasso_prediction_model) %in% c("x", "y")]
            != 0) == 0) {
    xdot_alasso_correct_rows[i] <- 1
  } else {
    xdot_alasso_correct_rows[i] <- 0
  }
}
### df
xdot_alasso_eta_df <-
  as.data.frame(matrix(xdot_alasso_correct_rows, nrow = 100))
colnames(xdot_alasso_eta_df) <- paste("eta_", eta)
### ydot
ydot_alasso_correct_rows <- rep(NA, nrow(ydot_alasso_prediction_model))
for (i in seq_len(nrow(ydot_alasso_prediction_model))) {
  if (
    ydot_alasso_prediction_model[i,]$x != 0 &&
    ydot_alasso_prediction_model[i,]$y != 0 &&
    rowSums(ydot_alasso_prediction_model[i,][, !names(ydot_alasso_prediction_model) %in% c("x", "y")]
            != 0) == 0) {
    ydot_alasso_correct_rows[i] <- 1
  } else {
    ydot_alasso_correct_rows[i] <- 0
  }
}
### df
ydot_alasso_eta_df <-
  as.data.frame(matrix(ydot_alasso_correct_rows, nrow = 100))
# Rename columns
colnames(ydot_alasso_eta_df) <- paste("eta_", eta) 
###
alasso_total_correct_eta <- rep(NA, nrow(xdot_alasso_eta_df))
alasso_total_correct_eta_df <- as.data.frame(matrix(NA, ncol = ncol(xdot_alasso_eta_df), nrow = 100))
for (i in seq_len(ncol(xdot_alasso_eta_df))) {
  new_df <- cbind(xdot_alasso_eta_df[, i],
                  ydot_alasso_eta_df[, i])
  for (j in seq_len(nrow(new_df))) {
    alasso_identification <- rep(NA, nrow(xdot_alasso_eta_df))
    if (any(new_df[j, ] == 0)) {
      alasso_total_correct_eta[j] <- 0
    } else {
      alasso_total_correct_eta[j] <- 1
    }
  }
  alasso_total_correct_eta_df[, i] <- alasso_total_correct_eta
}
colnames(alasso_total_correct_eta_df) <- eta

alasso_count_vector <-  data.frame(eta = colnames(alasso_total_correct_eta_df),
                                   Correct = colSums(alasso_total_correct_eta_df == 1) / 100,
                                   Incorrect = colSums(alasso_total_correct_eta_df == 0) / 100)
rownames(alasso_count_vector) <- seq(1, nrow(alasso_count_vector))
alasso_eta_gather_df <- alasso_count_vector %>%
  gather("Condition", "Value",
         2:ncol(alasso_count_vector))
### sindy -------------------------------
### xdot
### Count number of times model is correct per 100 rows
xdot_pysindy_prediction_model <- data.frame(x_dot_pysindy_pe)
colnames(xdot_pysindy_prediction_model) <-
  sub("\\_.*", "", colnames(xdot_pysindy_prediction_model))
xdot_pysindy_correct_rows <- rep(NA, nrow(xdot_pysindy_prediction_model))
for (i in seq_len(nrow(xdot_pysindy_prediction_model))) {
  if (
    xdot_pysindy_prediction_model[i,]$x != 0 &&
    xdot_pysindy_prediction_model[i,]$y != 0 &&
    rowSums(xdot_pysindy_prediction_model[i,][, !names(xdot_pysindy_prediction_model) %in% c("x", "y")]
            != 0) == 0) {
    xdot_pysindy_correct_rows[i] <- 1
  } else {
    xdot_pysindy_correct_rows[i] <- 0
  }
}
### df
xdot_pysindy_eta_df <-
  as.data.frame(matrix(xdot_pysindy_correct_rows, nrow = 100))
# Rename columns
colnames(xdot_pysindy_eta_df) <- paste("eta_", eta)
### ydot
ydot_pysindy_prediction_model <- data.frame(y_dot_pysindy_pe)
colnames(ydot_pysindy_prediction_model) <-
  sub("\\_.*", "", colnames(ydot_pysindy_prediction_model))
ydot_pysindy_correct_rows <- rep(NA, nrow(ydot_pysindy_prediction_model))
for (i in seq_len(nrow(ydot_pysindy_prediction_model))) {
  if (
    ydot_pysindy_prediction_model[i,]$x != 0 &&
    ydot_pysindy_prediction_model[i,]$y != 0 &&
    rowSums(ydot_pysindy_prediction_model[i,][, !names(ydot_pysindy_prediction_model) %in% c("x", "y")]
            != 0) == 0) {
    ydot_pysindy_correct_rows[i] <- 1
  } else {
    ydot_pysindy_correct_rows[i] <- 0
  }
}
### df
ydot_pysindy_eta_df <-
  as.data.frame(matrix(ydot_pysindy_correct_rows, nrow = 100))
# Rename columns
colnames(ydot_pysindy_eta_df) <- paste("eta_", eta)
###
pysindy_total_correct_eta <- rep(NA, nrow(xdot_pysindy_eta_df))
pysindy_total_correct_eta_df <- as.data.frame(matrix(NA, ncol = 11, nrow = 100))
for (i in seq_len(ncol(xdot_pysindy_eta_df))) {
  new_df <- cbind(xdot_pysindy_eta_df[, i],
                  ydot_pysindy_eta_df[, i])
  for (j in seq_len(nrow(new_df))) {
    pysindy_identification <- rep(NA, nrow(xdot_pysindy_eta_df))
    if (any(new_df[j, ] == 0)) {
      pysindy_total_correct_eta[j] <- 0
    } else {
      pysindy_total_correct_eta[j] <- 1
    }
  }
  pysindy_total_correct_eta_df[, i] <- pysindy_total_correct_eta
}
colnames(pysindy_total_correct_eta_df) <- eta
pysindy_count_vector <-  data.frame(eta = colnames(pysindy_total_correct_eta_df),
                                    Correct = colSums(pysindy_total_correct_eta_df == 1) / 100,
                                    Incorrect = colSums(pysindy_total_correct_eta_df == 0) / 100)
rownames(pysindy_count_vector) <- seq(1, nrow(pysindy_count_vector))
pysindy_eta_gather_df <- pysindy_count_vector %>%
  gather("Condition", "Value",
         2:ncol(pysindy_count_vector))
### ## save files --------------------
## lasso
write.csv(xdot_lasso_prediction_model,
          "stacked_bar_csv/SNR/linear2d_inc_snr_xdot_lasso_pred_models_new_sg.csv")
write.csv(ydot_lasso_prediction_model,
          "stacked_bar_csv/SNR/linear2d_inc_snr_ydot_lasso_pred_models_new_sg.csv")

## alasso 
write.csv(xdot_alasso_prediction_model,
          "stacked_bar_csv/SNR/linear2d_inc_snr_xdot_alasso_pred_models_new_sg.csv")
write.csv(ydot_alasso_prediction_model,
          "stacked_bar_csv/SNR/linear2d_inc_snr_ydot_alasso_pred_models_new_sg.csv")

## sindy 
write.csv(xdot_pysindy_prediction_model,
          "stacked_bar_csv/SNR/linear2d_inc_snr_xdot_sindy_pred_models_new_sg.csv")
write.csv(ydot_pysindy_prediction_model,
          "stacked_bar_csv/SNR/linear2d_inc_snr_ydot_sindy_pred_models_new_sg.csv")

## more_together
pysindy_eta_gather_df$Model <- "STLS"
lasso_eta_gather_df$Model <- "Lasso"
alasso_eta_gather_df$Model <- "Adaptive Lasso"
lasso_eta_gather_df$eta <- as.numeric(lasso_eta_gather_df$eta)
alasso_eta_gather_df$eta <- as.numeric(alasso_eta_gather_df$eta)
pysindy_eta_gather_df$eta <- as.numeric(pysindy_eta_gather_df$eta)

total_correct <- rbind(lasso_eta_gather_df, 
                       alasso_eta_gather_df,
                       pysindy_eta_gather_df) %>%
  dplyr::filter(Condition == "Correct")
total_correct$eta <- eta
total_correct$eta[which(total_correct$eta == Inf)] <- 73
total_correct_increasing_snr_df <- total_correct
save(eta, total_correct_increasing_snr_df, file = "success_rate_RData/linear2d_inc_snr_success_rate_new_sg.RData")
