#' HRM merge
#'
#' This function merges HEMs coming from separate subexperiments into one single dataset. HRM sorts each subexperiment by the anchor HEM, and the substitutes the anchor HEM by its quantiles.
#' @param data Sample data.
#' @param anchor_hem Reference HEM for merging
#' @param n_runs Number of runs on each subexperiment
#' @param n_pmcs Number of PMCS used on the experiment
#' @param n_subexp Number of subexperiments
#' @param processed Is the data already processed in the proper format to merge?
#' @return Returns a merged dataframe
#' @keywords HRM
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom data.table .N
#' @importFrom data.table .I
#' @import stats
#' @export
#' @references S. Vilardell, I. Serra, R. Santalla, E. Mezzetti, J. Abella and F. J. Cazorla, "HRM: Merging Hardware Event Monitors for Improved Timing Analysis of Complex MPSoCs," in IEEE Transactions on Computer-Aided Design of Integrated Circuits and Systems, vol. 39, no. 11, pp. 3662-3673, Nov. 2020, <doi:10.1109/TCAD.2020.3013051>.
#' @examples

merge_hems <- function(data = NULL, anchor_hem = NULL, n_runs = NULL, n_subexp = NULL, n_pmcs = NULL, processed = F) {
  if(!processed){
    data <- mergingTools::process_raw_experiments(data = data, n_pmcs = n_pmcs)
  }
  # Add grouping variable for ordering
  group <- rep(1:n_subexp, n_runs)
  data$group <- group
  data_ET_sorted <- data %>%
    as.data.table() %>%
    .[.[order(get(anchor_hem)),.I[1:.N],"group"]$V1] %>%
    .[, c(anchor_hem, "group"):=NULL] %>%
    as.matrix()

  merged_data <- purrr::map_df(data_ET_sorted %>% as.data.frame(), ~.x[!is.na(.x), drop = F])
  new_names <- colnames(data_ET_sorted)
  colnames(merged_data) <- new_names
  merged_data$PROCESSOR_CYCLES <- stats::quantile(data[[anchor_hem]], probs = seq(0, 1, length.out = n_runs), type = 2)
  return(merged_data)
}

