#' Code to HEM name
#'
#' This function returns the name of the HEM corresponding to the code of the T2080 manual.
#' @param cod HEM code.
#' @return Returns the name of the HEM that corresponds to the input code.
#' @keywords code
#' @import stats
#' @export
#' @examples

code2hem <- function(cod){
  # Codes as names. Now it does not matter theorder of the hems because it is not hardcoded.
  pmcs_n_codes <- T2080_code2name$hems
  names(pmcs_n_codes) <- T2080_code2name$code
  hem <- pmcs_n_codes[cod]
  return(hem)
}
