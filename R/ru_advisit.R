#' Add visit variables to a dataset.
#'
#' Pass in a data with codelist for AVISITN/AVISIT and return data with AvISITN/AVISIT.
#'
#' @param dsetin input dataset.
#' @param avisitnfmt Name of the codelist to derive AVISITN.
#' @param avisitfmt Name of the codelist to derive AVISIT.
#' @param codelistnames A list of codelist (format) 
#' 
#' @return A data frame with all records and variables from DSETIN and extra variables from DSETINCO.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' @export

ru_advisit <- function(dsetin, avisitnfmt=NULL, avisitfmt=NULL, codelistnames=list()) {
  
  bln_CheckParam <- FALSE
  if (!is.null(avisitnfmt)) {
    if (codelistnames[[avisitnfmt]]) { 
      message(paste0("Given AVISITNFMT ", avisitnfmt, " is not in CODELISTNAMES"))
      bln_CheckParam <- TRUE
    } 
  } 
  if (!is.null(avisitfmt)) {
    if (codelistnames[[avisitfmt]]) { 
      message(paste0("Given AVISITFMT ", avisitfmt, " is not in CODELISTNAMES"))
      bln_CheckParam <- TRUE
    }
  }
  
  if (!all(c("VISITNUM", "VISIT") %in% colnames(dsetin))) {
    message("Input dataset must contain 'VISITNUM' and 'VISIT' columns")
    bln_CheckParam <- TRUE
  }
  
  if (bln_CheckParam) {
    stop()
    return(base::invisible())
  }
  
    
  if (! is.null(avisitnfmt) && ! is.null(avisitfmt)) {
    # Case 1: Formats are specified
    # In SAS, put(VISITNUM, avisitnfmt) applies a format, then input() converts to numeric
    # In R, we assume a function apply_format() simulates the SAS format
    df_out <- function (dsetin, codedecodevarpairs=c("VISITNNUM", "AVISITN", "VISITNNUM", "AVISIT"), 
                        varcodelistpairs=c("VISITNUM", avisitnfmt, "VISITNNUM", avisitfmt), codelistnames=codelistnames, dsplan=NULL)
    df_out[["AVISITN"]] <- as.numeric(df_out[["AVISITN"]])
  } else {
    # Case 2: Formats are not specified
    df_out <- dsetin %>% dplyr::mutate(
      AVISITN = case_when(
        floor(VISITNUM) != VISITNUM | grepl("UNSCHEDULED", VISIT, ignore.case = TRUE) ~ 999,
        TRUE ~ VISITNUM
      ),
      AVISIT = case_when(
        floor(VISITNUM) != VISITNUM | grepl("UNSCHEDULED", VISIT, ignore.case = TRUE) ~ "UNSCHEDULED",
        TRUE ~ VISIT
      )
    ) 
  }
  return(df_out)
}      
