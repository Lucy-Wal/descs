#' Create a Descriptive Statistics and Correlation Table
#'
#' This function calculates descriptive statistics (mean, SD) and 
#' a correlation matrix with confidence intervals for a dataset.
#'
#' @name descs
#' @param data A data frame containing numeric variables.
#' @return A data frame with descriptive statistics and correlations.
#' @export
#' @examples
#' data <- mtcars
#' descs(data)
#' 
require(dplyr)
require(tidyr)
descs <- function(data) {
  var_names <- colnames(data)
  

  desc_stats_temp <- dplyr::summarise(data, across(all_of(var_names), list(mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE))))
  
  #Pivot
  desc_stats <- tidyr::pivot_longer(desc_stats_temp, cols = everything(), names_to = c("variable", ".value"), names_sep = "_")
  
  #Format
  mean_sd_col <- paste0(round(desc_stats$mean, 2), " (", round(desc_stats$sd, 2), ")")
  
  
  # Create an empty matrix to store correlation results
  n <- length(var_names)
  corr_matrix <- matrix("", nrow = n, ncol = n)
  rownames(corr_matrix) <- var_names
  colnames(corr_matrix) <- var_names
  
  # Fill in the correlation matrix (lower triangle only)
  for(i in 1:n){
    for(j in 1:n){
      if(i == j){
        corr_matrix[i, j] <- "--"  # Diagonal
      } else if(j < i){  # Lower triangle
        test <- cor.test(data[[var_names[i]]], data[[var_names[j]]], use = "pairwise.complete.obs")
        r_value <- round(test$estimate, 2)
        ci_lower <- round(test$conf.int[1], 2)
        ci_upper <- round(test$conf.int[2], 2)
        
        # Format p-value
        if(test$p.value < 0.001) {
          p_value <- "< .001"
        } else {
          p_value <- formatC(test$p.value, format = "f", digits = 3)
        }
        
        # Compose the cell text
        corr_matrix[i, j] <- paste0(r_value,
                                    " (", p_value,
                                    ") [", ci_lower, ", ", ci_upper, "]")
      } else {
        corr_matrix[i, j] <- ""  # Upper triangle left blank
      }
    }
  }
  
  # Convert the matrix to a data frame for printing
  corr_df <- as.data.frame(corr_matrix)
  
  # Insert the "Mean (SD)" values as the first column
  corr_df$`Mean (SD)` <- mean_sd_col
  
  # Rearrange the data frame to place "Mean (SD)" first, followed by the correlation values
  final_corr_df <- corr_df[, c("Mean (SD)", var_names)]
  
  # Remove the last column (the redundant right column)
  final_corr_df <- final_corr_df[, -ncol(final_corr_df)]
  
  return(final_corr_df)
}
