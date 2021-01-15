#' Generate multivariate Gaussian distribution parameters
#'
#' \code{generate_mvg_params} takes as input the experimental data along its correlation matrix to estimate the Sigma matrix and the vector of means to compute the multivariate Gaussian distribution
#'
#' @param splitted_data Splitted experimental data.
#' @param cor_matrix Correlation matrix
#' @return Returns the parameters of the multivariate Gaussian distributions as well as a list of concatenated HEM readings
#' @importFrom dplyr %>%
#' @keywords MUCH
#' @export
#' @examples
#' # merge_hems(data = data,
#' #            anchor_hem = "PROCESSOR_CYCLES",
#' #            n_runs = 1000,
#' #            n_subexp = 3,
#' #            n_pmcs = 6,
#' #            processed = FALSE)
generate_mvg_params <- function(splitted_data = NULL, cor_matrix = NULL) {
  hem_list <- list()
  hems <- colnames(cor_matrix)
  sd_hem <- vector(length = length(hems))
  mean_hem <- vector(length = length(hems))
  names(sd_hem) <- hems
  names(mean_hem) <- hems
  for (hem in hems) {
    df_with_h1h2 <- purrr::map(splitted_data, ~
    sum(colnames(.x) %in% hem))
    index <- which(df_with_h1h2 == 1)
    conc_vect <- c()

    for (ind in index) {
      h1h2_df_sc <- splitted_data[[ind]] %>%
        dplyr::select(dplyr::all_of(hem))
      outliers <- outliers::outlier(h1h2_df_sc[[hem]])
      h1h2_df_sc <- h1h2_df_sc %>%
        dplyr::filter(get(hem) != outliers) %>%
        dplyr::pull(hem)
      conc_vect <- c(conc_vect, h1h2_df_sc)
    }
    sd_hem[hem] <- sd(conc_vect)
    mean_hem[hem] <- mean(conc_vect)
    hem_list[[hem]] <- conc_vect
  }

  sd_diag <- diag(sd_hem)

  # Now compute the Sigma matrix
  sigma_matrix <- sd_diag %*% cor_matrix %*% sd_diag
  return(list(sigma_matrix = sigma_matrix, means = mean_hem, hems = hem_list))
}
