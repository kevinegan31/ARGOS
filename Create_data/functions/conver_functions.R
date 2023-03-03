### transform output csv to stacked barcharts csv ----------------------
out_csv2bar_csv_argos_snr <- function(pe_data, ci_data, eta_seq){
  xdot_pe_current_df <- pe_data
  xdot_ci_current_df <- ci_data
  xdot_current_all_df <-
    cbind(eta_seq,
          xdot_pe_current_df,
          xdot_ci_current_df)
  ### Create xdot prediction model dataframe
  xdot_prediction_model <-
    data.frame(matrix(
      ncol = ncol(xdot_pe_current_df),
      nrow = nrow(xdot_pe_current_df)
    ))
  colnames(xdot_prediction_model) <- prediction_model_names[seq_along(xdot_pe_current_df)]
  ### Generate model containing coefficient values based on whether or not we
  ### select variable in model.
  for (i in 1:((ncol(xdot_current_all_df[-1])) / 3)) {
    new_df <- xdot_current_all_df %>%
      dplyr::select(starts_with(prediction_model_names[i])) %>%
      replace(is.na(.), 0)
    value <- 0
    new_df <- new_df %>%
      mutate(ok = value >= new_df[, 2] & value <= new_df[, 3]) %>%
      mutate(
        w_in_ci = round(new_df[, 1], 4) >= round(new_df[, 2], 4)
        & round(new_df[, 1], 4) <= round(new_df[, 3], 4)
        & ok == FALSE
      )
    ### If we do not select variable, put 0 in position
    for (j in 1:nrow(new_df)) {
      if (new_df$w_in_ci[j] == FALSE) {
        xdot_prediction_model[j, prediction_model_names[i]] <- 0
      } else {
        ### if we select the variable, keep coefficient.
        xdot_prediction_model[j, prediction_model_names[i]] <-
          new_df[j, 1, drop = FALSE]
      }
    }
  }
  ### Update Colnames
  colnames(xdot_prediction_model) <-
    sub("\\_.*", "", colnames(xdot_prediction_model))
  return(xdot_prediction_model)
}
out_csv2bar_csv_argos_n <- function(pe_data, ci_data, n_seq_total){
  xdot_pe_current_df <- pe_data
  xdot_ci_current_df <- ci_data
  xdot_current_all_df <-
    cbind(n_seq_total,
          xdot_pe_current_df[1:length(n_seq_total),],
          xdot_ci_current_df[1:length(n_seq_total),])
  ### Create xdot prediction model dataframe
  xdot_prediction_model <-
    data.frame(matrix(
      ncol = ncol(xdot_pe_current_df[1:length(n_seq_total),]),
      nrow = nrow(xdot_pe_current_df[1:length(n_seq_total),])
    ))
  colnames(xdot_prediction_model) <-
    prediction_model_names
  ### Generate model containing coefficient values based on whether or not we
  ### select variable in model.
  for (i in 1:((ncol(xdot_current_all_df[-1])) / 3)) {
    new_df <- xdot_current_all_df %>%
      dplyr::select(starts_with(prediction_model_names[i])) %>%
      replace(is.na(.), 0)
    value <- 0
    new_df <- new_df %>%
      mutate(ok = value >= new_df[, 2] & value <= new_df[, 3]) %>%
      mutate(
        w_in_ci = round(new_df[, 1], 4) >= round(new_df[, 2], 4)
        & round(new_df[, 1], 4) <= round(new_df[, 3], 4)
        & ok == FALSE
      )
    ### If we do not select variable, put 0 in position
    for (j in 1:nrow(new_df)) {
      if (new_df$w_in_ci[j] == FALSE) {
        xdot_prediction_model[j, prediction_model_names[i]] <-
          0
      } else {
        ### if we select the variable, keep coefficient.
        xdot_prediction_model[j, prediction_model_names[i]] <-
          new_df[j, 1, drop = FALSE]
      }
    }
  }
  ### Update Colnames
  colnames(xdot_prediction_model) <-
    sub("\\_.*", "", colnames(xdot_prediction_model))
  return(xdot_prediction_model)
}
