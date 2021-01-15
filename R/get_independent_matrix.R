#' Generate independent HEMs
#'
#' There may be some HEMs which are linear dependent on another HEM.
#' This produces conflicts when generating the multivariate Gaussian distribution as the Sigma matrix will not be invertible, causing an error.
#' To avoid this, \code{get_independent_matrix} tells the user which HEMs are independent so they can choose which HEMs to use.
#' To do so the user must input the maximum correlation between HEMs by \code{dep_lvl}.
#' If \code{dep_lvl} is too high (>0.95) the user may leave very dependent HEMs and thus the Sigma matrix will not be invertible.
#' We advise the user to lower the \code{dep_lvl} until the Sigma matrix is invertible.
#'
#' @param cor_matrix Correlation matrix
#' @param dep_lvl Dependency level allowed. A number between 0 and 1 indicating the maximum correlation allowed between HEMs.
#' @return Returns a list of independent HEMs
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
get_independent_matrix <- function(cor_matrix = NULL, dep_lvl = NULL) {
  dependant_list <- list()
  hems <- colnames(cor_matrix)
  cor_matrix_ind <- cor_matrix
  for (i in 1:length(hems)) {
    matrix_names <- colnames(cor_matrix_ind)
    name <- hems[i]
    if (name %in% matrix_names) {
      dep_var <- which(cor_matrix_ind[, name] > dep_lvl)[-1]
      if (!rlang::is_empty(dep_var)) {
        dependant_list[[name]] <- dep_var
        cor_matrix_ind <- cor_matrix_ind[-dep_var, -dep_var]
      }
    }
  }
  return(cor_matrix_ind)
}
