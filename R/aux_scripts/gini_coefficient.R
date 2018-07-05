library(rlang)

gini_coefficient <- function(data, target, prediction, weight = NULL, na_replace = TRUE){

  # -- Error Handling -------

  # Missing predictions
  if(sum(is.na(data[prediction]) > 0)){
    stop("There are NAs in ",prediction,", please fix. Note na_replace does not apply to predictions.")
  }

  # Missing target
  if(na_replace==TRUE){ # replace NAs with 0s
    data[target] <- ifelse(is.na(data[[target]]),0,data[[target]])
  }else if(sum(is.na(data[prediction]) > 0)){
    stop("There are NAs in ",target,", please fix, or set na_replace to TRUE.")
  }

  # NULL weight - assume weight 1 per row
  if(is.null(weight)){
    weight = "weight_col"
    data[weight] <- rep(1,nrow(data))
  }

  # -- Remove rows with zero/missing weight -------

  # Determine the filter expression
  filter_exprs = rlang::parse_exprs(
    paste0("!(",weight,"== 0);","!is.na(",weight,")")
  )

  # Apply the filter
  data <- data %>%
    dplyr::filter(!!!filter_exprs)

  # -- Sort data -------

  # First sort by prediction - this is to ensure a consistent normalised gini
  data <- data %>% dplyr::arrange(!!!rlang::parse_expr(prediction))

  # Sort by target to enable gini coefficient calculation
  data <- data %>% dplyr::arrange(!!!rlang::parse_expr(target))

  # -- Calculate AUC --------

  # Create an origin
  first_row <- data.frame(cum_weight =0, cum_target=0)

  # Calculate cumulative totals
  cum_weight = cumsum(data[[weight]]) / sum(data[[weight]],na.rm=TRUE)
  cum_target = cumsum(data[[target]]) / sum(data[[target]],na.rm=TRUE)

  # Combine all data for auc calc
  auc_df_prep <- rbind.data.frame(first_row,cbind.data.frame(cum_weight,cum_target))
  auc_df_prep_lagged = rbind(first_row,auc_df_prep[-nrow(auc_df_prep),])
  colnames(auc_df_prep_lagged) = c("cum_weight_lag","cum_target_lag")
  auc_df <- cbind.data.frame(auc_df_prep,auc_df_prep_lagged)

  # Calculate the AUC
  AUC <-
    auc_df %>%
    mutate(
      segment_area = (cum_weight-cum_weight_lag ) * (cum_target+cum_target_lag)/2
    )%>%
    summarise(AUC = sum(segment_area)) %>%
    .[["AUC"]]

  # -- Output -------

  return(AUC)

}

# -- TESTS -------

# var11 <- c(0, 2, NA, 4, 3)
# pred <- c(0.1, 0.4, 0.3, 1.2, 0.0)
# targets <- c(0, 0, 1, 0, 1)
#
# data=cbind.data.frame(var11,pred,targets)
#
#
# gini_coefficient(data=data,target="targets",prediction="pred",weight="var11")

