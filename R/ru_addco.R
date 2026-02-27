#' Merge SDTM CO Domain in
#'
#' Pass in a primary SDTM domain and CO domain to merge CO domain into primary domain.
#'
#' @param dsetin SDTM domain with his DOMAIN in RDOMAIN of CO domain.
#' @param dsetinco SDTM CO domain.
#'
#' @return A data frame with all records and variables from DSETIN and extra variables from DSETINCO.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' df_dm <- ru_addco(sdtmdata$dm(), sdtmdata$co()) 
#'
#' @export
#'

ru_addco <- function(dsetin, dsetinco) {
  print(paste0("RU_ADDCO: ", "Start of RU_ADDCO"))
  
  # Read input datasets
  main_df <- as.data.frame(dsetin)
  co_df <- as.data.frame(dsetinco)
  
  # Identify domain and RDOMAIN
  l_domain <- base::unique(main_df$DOMAIN)
  l_rdomain <- as.character(dplyr::filter(co_df, RDOMAIN == !! l_domain) %>% dplyr::select(RDOMAIN) %>% base::unique())
  
  if (length(l_rdomain) == 0) {
    message(paste("NOTE: Domain", l_domain, "not found in RDOMAIN of input CO dataset"))
    return(main_df)
  }
  
  # Prepare supplemental dataset
  co_df.1 <- co_df %>% dplyr::filter(RDOMAIN == !! l_rdomain) %>% dplyr::group_by(STUDYID, USUBJID, IDVAR, IDVARVAL) %>%
    dplyr::mutate(seq__ = dplyr::row_number(), COREF=ifelse(is.na(COREF), "Comment", COREF)) %>% dplyr::ungroup() %>%
    dplyr::select(-c("DOMAIN", "RDOMAIN"))
    
  
  # Transpose supplemental dataset
  co_df.2 <- co_df.1 %>% dplyr::group_by(STUDYID, USUBJID, IDVAR, IDVARVAL) %>%
    tidyr::pivot_wider(names_from =seq__, values_from = COVAL, names_prefix = paste0(l_domain, "COMM")) %>%
    dplyr::ungroup()
  
  if (max(co_df.1$seq__)[1] == 1)  names(co_df.2)[names(co_df.2) == "PCCOMM1"] <- "PCCOMM"
  
  # Merge datasets
  this.idvars <- as.character(base::unique(co_df.2$IDVAR))
  df.dsetout <- NULL
  for (this.idvar in this.idvars) {
    df.thismain <- main_df %>% dplyr::mutate(IDVARVAL = stringr::str_trim(as.character(!! rlang::sym(this.idvar)))) 
    df.thisco <- co_df.2 %>% dplyr::filter(IDVAR == !! this.idvar) %>% dplyr::mutate(IDVARVAL=stringr::str_trim(IDVARVAL))
    df.thisdset <- base::merge(df.thismain, df.thisco, by = c("STUDYID", "USUBJID", "IDVARVAL"), all.x = TRUE)
    
    if (is.null(df.dsetout)) df.dsetout <- df.thisdset else df.dsetout <- rbind(df.dsetout, df.thisdset)
  }
  
  df.dsetout <- dplyr::select(df.dsetout, -c(IDVARVAL, IDVAR))
  
  
  # Assign final dataset to the output name
  # assign(dsetout, merged_df, envir = .GlobalEnv)

  print(paste0("RU_ADDCO: ", "End of RU_ADDCO"))
  return(df.dsetout)
}
  
# Example Call
# co <- sdtmdata$co()
# pc <- sdtmdata$pc()
# pcco <- ru_addco(dsetin = pc, dsetinco = co)
