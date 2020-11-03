#' Process T2080 data
#'
#' This functions processes the rawexperimental data into a dataframe by blocks separating each subexperiment.
#' @param data Raw experimental data from T2080.
#' @param n_pmcs Number of PMCs used on the experiment
#' @return Returns the processed raw experimental data.
#' @keywords HRM, MUCH
#' @export
#' @examples
process_raw_experiments <- function(data, n_pmcs) {

  m <- nrow(data)
  d <- 1:ncol(data)
  n_groups <- ceiling(ncol(data) / n_pmcs)
  d <- rep(split(d, ceiling(seq_along(d) / n_pmcs)), m)
  ind <- rep(1:m, each = n_groups)
  total_new_rows <- length(d)
  new_data <- data.frame(matrix(NA, ncol = ncol(data), nrow = total_new_rows))

  names(new_data) <- names(data)
  for (j in 1:total_new_rows) {
    new_data[j, c(1, d[[j]][-1])] <- data[ind[j], d[[j]]]
  }

  new_data <- new_data %>%
    purrr::map(~.x) %>%
    purrr::discard(~all(is.na(.x))) %>%
    purrr::map_df(~.x)

  new_data_names <- names(new_data) %>% stringr::str_replace_all("X", "")
  new_names <- mergingTools::code2hem(new_data_names)
  names(new_data) <- new_names
  return(new_data)
}
