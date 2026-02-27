#' Apply attribute from plan file to input data
#'
#' Pass in a data with attribute file and return data with attibute added.
#'
#' @param dsetin input dataset.
#' @param dsplan The hath and filename of tab-delimited file containing HARP A&R dataset plan.
#'
#' @return A data frame with all records and variables from DSETIN and extra variables from DSETINCO.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' @export
#'

ru_attrib <- function (dsetin, dsplan) {
  print(paste0("RU_ATTRIB: ", "Start or RU_ATTRIB")) 
  
  # With function return NA if string is not found. Change it to return 0.
  ru_index <- function(...) {
    rtn <- base::which(...)
    if (length(rtn) < 1) rtn <- 0
    else rtn <- rtn[1]
    rtn
  }
  
  # Dataset label.
  df_varattr_dlabel <- utils::read.table(dsplan, header = FALSE, sep = ":", dec = ".", skip=1, nrow=1,
                                       row.names=NULL, quote = "", stringsAsFactors = FALSE, fill=TRUE)
  # print(df_varattr_dlabel)
  
  # Column title: there is a missing \t in column title for spec created in Harp.
  df_varattr_label <- utils::read.table(dsplan, header = FALSE, sep = "\t", dec = ".", skip=3, nrow=1,
                                 row.names=NULL, quote = "", stringsAsFactors = FALSE, fill=TRUE)

  # Column values
  df_varattr <- utils::read.table(dsplan, header = FALSE, sep = "\t", dec = ".", skip=4,
                                      row.names=NULL, quote = "", stringsAsFactors = FALSE, fill=TRUE)
  
  str_varattr_name <- NULL
  for ( i in 1:length(df_varattr_label)) {
    if (is.null(str_varattr_name)) str_varattr_name <- base::toupper(base::gsub("\\s+", ".", df_varattr_label[i]))
    else str_varattr_name <- c(str_varattr_name, base::toupper(base::gsub("\\s+", ".", df_varattr_label[i])))
  }
  
  # define column names instead of getting column names from file.
  str_varattr_name <- c("VARIABLE.NAME", "VARIABLE.LABEL", "CRT.INCLUSION.FLAG", "TYPE", "SAS.LENGTH", 
                      "SAS.FORMAT", "DERIVATION", "COMMENTS", "ACRF.PAGES", "VARIABLE.ORDER", "SORT.ORDER",
                     "DECODE.FORMAT", "COMPUTATIONAL.METHOD", "REQUIRED", "SIGNIFICANT.DIGITS", "VALUE.LEVEL.METADATA",
                     "ORIGIN.ROLE")
  
  names(df_varattr) <- str_varattr_name
  
  this_attr <<- df_varattr

  # Drop the identified columns
  if (NA %in% names(df_varattr)){
    df_varattr <- df_varattr[, -which(is.na(names(df_varattr)))]  
  }

  # Get Variable Order
  df_varattr_1 <- df_varattr %>% dplyr::mutate(VARIABLE.ORDER=ifelse(is.na(VARIABLE.ORDER), 9999, VARIABLE.ORDER)) %>%
    dplyr::select(c("VARIABLE.ORDER", "VARIABLE.NAME")) %>% dplyr::distinct() %>%
    dplyr::group_by(VARIABLE.ORDER, VARIABLE.NAME) %>% dplyr::arrange(VARIABLE.ORDER, .by_group = TRUE) %>% ungroup()
  
  str_varattr_1 <- unlist(df_varattr_1["VARIABLE.NAME"]) 
  names(str_varattr_1) <- NULL

  # Get sorting variables and orders
  df_varattr_2 <- df_varattr %>% dplyr::filter(!is.na(SORT.ORDER)) %>%                          # dplyr::filter(!is.na(df_varattr["SORT.ORDER"])) %>%
    dplyr::select(c("SORT.ORDER", "VARIABLE.NAME")) %>% dplyr::distinct() %>%
    dplyr::group_by(SORT.ORDER, VARIABLE.NAME) %>% dplyr::arrange(SORT.ORDER, .by_group = TRUE) %>% ungroup()

  str_varattr_2 <- unlist(df_varattr_2["VARIABLE.NAME"])
  names(str_varattr_2) <- NULL
  
  #print(str_varattr_1)
  #print(str_varattr_2)
  
  # Change column types based on type in spec.
  # Get column labels from spec.
  str_indatavars.o <- names(dsetin)
  str_indatavars <- toupper(names(dsetin))

  #===================================================================
  # Create a list of name-value pairs for variables and their labels.
  #===================================================================
  vlbls <- setNames(as.list(df_varattr$VARIABLE.LABEL),df_varattr$VARIABLE.NAME)

  str_statements <- "data.frame("
  for (i in 1:length(str_varattr_1)) { 
    df_sub_1 <- df_varattr %>% dplyr::filter(VARIABLE.NAME == !! str_varattr_1[i])               # dplyr::filter(df_varattr["VARIABLE.NAME"] == !! str_varattr_1[i]) 
    var_type_1 <- toupper(unlist(df_sub_1[1, "TYPE"]))
    var_label_1 <- unlist(df_sub_1[1, "VARIABLE.LABEL"])  
    var_format_1 <- unlist(df_sub_1[1, "SAS.FORMAT"])  
    
    if (i == 1) var_label <- var_label_1
    else var_label <- c(var_label, var_label_1)   
    
    if (i == 1) var_type <- var_type_1
    else var_type <- c(var_type, var_type_1)   
    # print(c(var_format_1, var_type_1, var_label_1))
    
    if (toupper(var_format_1) %in% c("DATE")) {
      str_statements <- paste0(str_statements, str_varattr_1[i], "=", "as.Date(character()),")
    } else if (var_type_1 %in% c("NUMERIC", "CHAR")) {
      str_statements <- paste0(str_statements, str_varattr_1[i], "=", "character(),")
    } else {
      str_statements <- paste0(str_statements, str_varattr_1[i], "=", "numeric(),")
    }
  }
  str_statements <- paste0(str_statements, "stringsAsFactors=FALSE)")
  df_out_1 <- eval(parse(text=str_statements))
  
  #====================================================================
  # Change column orders and keep only columns which are in spec file.
  #====================================================================
  # for (j in 1:length(str_varattr_1)) { 
  #   n.ind <- ru_index(str_indatavars == str_varattr_1[j])
  #   for (i in 1:nrow(dsetin)) {
  #     if (str_varattr_1[j] %in% str_indatavars) {
  #       df_out_1[i, j] <- dsetin[i, n.ind]
  #     } else {
  #       df_out_1[i, j] <- NA        
  #     }
  #   }
  # }

  # Variables in data set but not spec.
  warn1ng <- setdiff(str_indatavars,str_varattr_1)
  if (length(warn1ng)>0){message(paste0('\nW','ARNING: (from ru_attrib.R) The following variables are in the data set but not the spec [',dsplan,'] - ',paste(warn1ng,collapse=', ' ),'\n'))}

  # Variables in spec but not data set.
  warn2ng <- setdiff(str_varattr_1,str_indatavars)
  if (length(warn2ng)>0){message(paste0('\nW','ARNING: (from ru_attrib.R) The following variables are in the spec [',dsplan,'] but not the data set - ',paste(warn2ng,collapse=', ' ),'\n'))}

  df_out_1 <- df_out_1 %>% select(all_of(intersect(str_varattr_1,str_indatavars)))
  dsetin <- dsetin %>% select(all_of(intersect(str_varattr_1,str_indatavars)))
  df_out_1 <- rbind(df_out_1,dsetin)  

  # sort data based on order columns in spec.
  if (! is.null(str_varattr_2) && str_varattr_2[1] != "") {
    df_out_1 <- data.table::setorderv(df_out_1, str_varattr_2)
  }

  VAR_LABEL <<- var_label
  
  # apply labels
  # df_out <- ru_labels(as.data.frame(df_out_1), var_label)
  df_out <- ru_labels(as.data.frame(df_out_1), vlbls)
  base::attr(df_out, "label") <- df_varattr_dlabel

  return(df_out)
}

# df_attr <- ru_attrib(df_pop_1, "/mnt/code/ADSNAP/adsnap_spec.txt")
# df_attr <- ru_attrib(df_lastdset, "C:/usr/R/yw62951_viiv_dev/ADSNAP/adsnap_spec.txt")
