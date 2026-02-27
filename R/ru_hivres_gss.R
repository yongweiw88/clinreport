#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_hivres_gss.R
# Purpose: derive GSS penalty scroe based on Stanford HIV resistance database
# Date:    02/26/2025
#========================================================================= 

ru_hivres_gss <- function (
  dsetingf    =sdtmdata$pf() %>% dplyr::filter(PFTESTCD == "AA"),
  dsetinsuppgf=sdtmdata$supppf(),
  dsetingss   =rfmtdata$gss2024(),
  gregionvar  ="PFGENRI",
  drugnamelist =NULL,
  artlist     =c("CAB", "RPV"),
  insertion   =c("INSERTION", "INS", "+"),
  deletion    =c("DELETION", "DEL", "-"),
  byvars      =c("STUDYID", "USUBJID", "VISITNUM", "VISIT", "PFDTC", "PFDY")
  ) {
  
  print(paste0("RU_HIVRES_GSS: ", "Start of RU_HIVRES_GSS"))
  
  newvarlabels=list("MCRIT1"="Analysis Multi-Response Criterion 1", "MCRIT1ML"="Multi-Response Criterion 1 Evaluation",
                    "MCRIT2"="Analysis Multi-Response Criterion 2", "MCRIT2ML"="Multi-Response Criterion 2 Evaluation",
                    "MCRIT3"="Analysis Multi-Response Criterion 3", "MCRIT3ML"="Multi-Response Criterion 3 Evaluation",
                    "GENROI"="Genetic Region of Interest", "GENROICD"="Genetic Region of Interest",
                    "PARCAT1"=" Parameter Category 1", "PARCAT2"=" Parameter Category 2", "PARAMCD"="Parameter Code", "PARAM"="Parameter", 
                    "ANL01FL"="Analysis Flag 01", "ANL02FL"="Analysis Flag 02", "DTYPE"="Derivation Type", "AVALC"="Analysis Value (C)", 
                    "AVAL"="Analysis Value", "DRUGCLS"="Drug Class", "DRUG"="Drug Name", "DRUGCD"="Drug Name Code",
                    "ONDRGFL"="On ART Drug Flag", "AVALCAT1"="Analysis Value Category 1", "AVALCAT2"="Analysis Value Category 2", 
                    "AVALCA2N"="Analysis Value Category 2 (N)")
  
  artfmt <- list(
    "CABOTEGRAVIR"="CAB",
    "DOLUTEGRAVIR"="DTG",
    "ELVITEGRAVIR"="EVG",
    "RALTEGRAVIR"="RAL",
    "BICTEGRAVIR"="BIC",
    "DELAVIRDINE"="DLV",
    "EFAVIRENZ"="EFV",
    "ETRAVIRINE"="ETR",
    "NEVIRAPINE"="NVP",
    "RILPIVIRINE"="RPV",
    "DORAVIRINE"="DOR",
    "LAMIVUDINE"="3TC",
    "ABACAVIR"="ABC",
    "ZIDOVUDINE"="AZT",
    "EMTRICITABINE"="FTC",
    "TENOFOVIR"="TDF",
    "STAVUDINE"="D4T",
    "DIDANOSINE"="DDI",
    "INDINAVIR"="IDV",
    "ATAZANAVIR"="ATV",
    "ENFUVIRTIDE"="EI",
    "BMS-626529A"="TMV",
    "FOSAMPRENAVIR"="FPV",
    "ATAZANAVIR/R"="ATVR",
    "DARUNAVIR"="DRV",
    "DARUNAVIR/R"="DRVR",
    "FOSAMPRENAVIR/R"="FPVR",
    "INDINAVIR/R"="IDVR",
    "LOPINAVIR"="LPV",
    "LOPINAVIR/R"="LPVR",
    "NELFINAVIR"="NFV",
    "RITONAVIR"="RTV",
    "SAQUINAVIR"="SQV",
    "SAQUINAVIR/R"="SQVR",
    "TIPRANAVIR"="TPV",
    "TIPRANAVIR/R"="TPVR",
    "FOSTEMSAVIR"="FTR",
    "LENACAPAVIR"="LEN"
  )
  
  if (is.null(drugnamelist)) {
    drugnamelist <- as.list(names(artfmt))
    names(drugnamelist) <- unlist(artfmt)
  }
  
  df_gf <- ru_addsupp(dsetingf, dsetinsuppgf)
  str_domain <- as.character(base::unique(df_gf[["DOMAIN"]]))
  resultcvar <- paste0(str_domain, "STRESC")
  str_catvar <- paste0(str_domain, "CAT")
  byvars <- base::unique(c(str_catvar, byvars))

  #
  # Split genotype, which have more they one genotypes, in Stanfor HIV DB
  # data into multipe records. one genotype per records.
  #
  df_gss <- dsetingss
  df_gss[["GENOTYPE_"]] <- stringr::str_split(df_gss[["GENOTYPE"]], "\\+")
  df_gss_1 <- df_gss %>% tidyr::unnest(GENOTYPE_, keep_empty = TRUE) %>% dplyr::mutate(GENOTYPE_=stringr::str_trim(GENOTYPE_))
  
  df_gss_2 <- ru_hivres_splitrules(dsetingf=df_gss_1, resultcvar="GENOTYPE_", insertion=insertion, deletion=deletion)
  
  df_gss_2[["AAS_"]] <- stringr::str_split(df_gss_2[["AAS"]], "")
  
  df_gss_3 <- df_gss_2 %>% 
    tidyr::unnest(AAS_, keep_empty = TRUE) %>%
    dplyr::mutate(AAS=AAS_) %>% dplyr::select(-AAS_)
    
  df_gss_4 <- df_gss_2 %>% 
    dplyr::mutate(        
      GENROICD=case_when(
        CLASS == "INSTI"  ~ "IN",
        CLASS == "PI"     ~ "PR",
        stringr::str_detect(CLASS, "CAPSI") ~ "FI",
        CLASS == "NRTI"   ~ "RT",
        CLASS == "NNRTI"  ~ "RT",
        TRUE ~ NA
      ),
      GENROI=case_when(
        GENROICD == "IN" ~ "INTEGRASE",
        GENROICD == "FI" ~ "FUSION",
        GENROICD == "PR" ~ "PROTEASE",
        GENROICD == "RT" ~ "REVERSE TRANSCRIPTASE",
        TRUE ~ NA
      )  
    ) %>% dplyr::select(GENOTYPE, GENROICD, GENROI, CLASS, DRUG_CODE, GSS, AAS, CODON)
  
  #
  # Merge Stanford HIV DB data with input dataset
  #
  df_gf <- df_gf %>% dplyr::mutate(seqgss__=dplyr::row_number())
  df_gf <- df_gf %>% dplyr::filter(! (!! rlang::sym(resultcvar) %in% c("ND", "NA", "NR", "")) & ! is.na(!! rlang::sym(resultcvar))) 
  df_gf_1 <- df_gf %>% dplyr::select(all_of(c("seqgss__", resultcvar, gregionvar)))
  
  df_gf_2 <- ru_hivres_splitrules(dsetingf=df_gf_1, resultcvar=resultcvar, insertion=insertion, deletion=deletion) %>% 
    dplyr::mutate(
      GENROICD_ = base::substring(!! rlang::sym(gregionvar), 1, 1),
      GENROICD=case_when(
        GENROICD_ == "I" ~ "IN",
        GENROICD_ == "E" ~ "FI",
        GENROICD_ == "P" ~ "PR",
        GENROICD_ == "F" ~ "FI",
        GENROICD_ == "R" ~ "RT",
        TRUE ~ NA
      ),
      GENROI=case_when(
        GENROICD_ == "I" ~ "INTEGRASE",
        GENROICD_ == "E" ~ "FUSION",
        GENROICD_ == "P" ~ "PROTEASE",
        GENROICD_ == "F" ~ "FUSION",
        GENROICD_ == "R" ~ "REVERSE TRANSCRIPTASE",
        TRUE ~ NA
      )      
    ) %>% dplyr::select(-GENROICD_)


  df_gf_3 <- dplyr::left_join(df_gf_2, df_gss_4, by=c("GENROICD", "GENROI", "CODON", "AAS"), relationship = "many-to-many") %>% 
    dplyr::select(-all_of(c(resultcvar, gregionvar)))
  df_gf_4 <- dplyr::left_join(df_gf, df_gf_3, by="seqgss__") 
  
  #
  # Create a data which include all 
  #
  df_gf_all <- base::merge(df_gf %>% dplyr::select(all_of(c(byvars))) %>% distinct(), 
                    df_gss_4 %>% dplyr::select( GENROICD, GENROI, CLASS, DRUG_CODE) %>% distinct(), by=NULL, all.x=TRUE, all.y=TRUE) 

  #
  # Find Genotypes which matches multiple GENOTYPE (separated by "+") in Standford Databse
  #
  df_gf_4_1 <- df_gf_4 %>% dplyr::filter(str_detect(GENOTYPE, "\\+")) %>%
    dplyr::select(all_of(c(byvars, "PFGENRI", "GENROICD", "GENROI", "CLASS", "DRUG_CODE", "GENOTYPE", "CODON"))) %>%
    dplyr::distinct() %>% 
    dplyr::group_by(dplyr::across(all_of(c(byvars, "PFGENRI", "GENROICD", "GENROI", "CLASS", "DRUG_CODE", "GENOTYPE")))) %>%
    dplyr::summarise(nummuts1_ = n()) %>% dplyr::ungroup() 
  
  
  df_gf_4_1[["nummuts2_"]] <- base::lengths(str_extract_all(df_gf_4_1$GENOTYPE, "\\+"))
  df_gf_4_1 <- df_gf_4_1 %>% dplyr::filter(nummuts2_ == nummuts1_ + 1) %>% dplyr::select(-nummuts2_, -nummuts1_)
  
  #
  # Add GSS score in for multiple GENOTYP.
  #
  df_gf_4_1_1 <- dplyr::inner_join(df_gf_4_1, df_gss %>% dplyr::select(GENOTYPE, CLASS, DRUG_CODE, GSS) %>% distinct(), by=c("GENOTYPE", "CLASS", "DRUG_CODE"))
  
  #
  # If Wild-type (REFRES==AAS), assigne GSS to 0 so that negative GSS will not be used.
  # Get maximum GSS at each CODON per drug in each by-group
  #
  df_gf_4_2 <- df_gf_4 %>% dplyr::filter(! str_detect(GENOTYPE, "\\+")) %>% 
    dplyr::mutate(GSS=if_else(GSS<=0 & REFRES == AAS & ! is.na(AAS), 0, GSS)) %>%
    dplyr::select(all_of(c(byvars, "PFGENRI", "GENROICD", "GENROI", "CLASS", "DRUG_CODE", "CODON", "GSS"))) %>%
    dplyr::group_by(dplyr::across(all_of(c(byvars, "PFGENRI", "GENROICD", "GENROI", "CLASS", "DRUG_CODE", "CODON")))) %>%
    dplyr::arrange(GSS, .by_group = TRUE) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() 
  
  df_gf_all_1 <- dplyr::left_join(df_gf_all, df_gf_4_1_1, by=c(byvars, "GENROICD", "GENROI", "CLASS", "DRUG_CODE")) %>%
    dplyr::mutate(GSS=if_else(is.na(GENOTYPE), NA, GSS))
  df_gf_all_2 <- dplyr::left_join(df_gf_all, df_gf_4_2, by=c(byvars, "GENROICD", "GENROI", "CLASS", "DRUG_CODE")) %>%
    dplyr::mutate(GSS=if_else(is.na(CODON), NA, GSS))
  
  df_gf_all_3 <- dplyr::bind_rows(df_gf_all_1, df_gf_all_2) %>% dplyr::mutate(GSS=if_else(is.na(GSS), 0, GSS))
  df_gf_5 <- df_gf_all_3 %>% 
    dplyr::group_by(dplyr::across(all_of(c(byvars, "GENROICD", "GENROI", "CLASS", "DRUG_CODE")))) %>%
    dplyr::summarise(AVAL=sum(GSS)) %>% dplyr::ungroup() %>% 
    dplyr::mutate(AVAL=if_else(AVAL < 0, 0, AVAL))
  
  if (! is.null(artlist)) {
    df_gf_5_1 <- df_gf_5 %>% 
      dplyr::filter(DRUG_CODE %in% toupper(!! artlist)) %>%
      dplyr::group_by(dplyr::across(all_of(c(byvars)))) %>%
      dplyr::summarise(AVAL=sum(AVAL)) %>% dplyr::ungroup() %>% 
      dplyr::mutate(DRUG_CODE="SGSS")    
    df_gf_5 <- dplyr::bind_rows(df_gf_5, df_gf_5_1)
  }

  df_gf_5[["DRUG"]] <- as.character(sapply(df_gf_5$DRUG_CODE, function(x) drugnamelist[[x]]))

  df_out <- df_gf_5 %>% 
    dplyr::rename(
      GSS = AVAL,
      DRUGCD = DRUG_CODE
    ) %>%
    dplyr::mutate(
      ANL01FL="Y",
      DRUG=if_else(DRUG=="NULL", NA, DRUG),
      ONDRGFL = if_else(toupper(DRUGCD) %in% toupper(!! artlist), "Y", NA_character_),
      PARCAT1 = !! rlang::sym(str_catvar),
      PARCAT2 = "Stanford Genotypic Susceptibility Score",  
      PARAMCD = if_else(DRUGCD == "SGSS", "T_SGSSRG", paste0("SGSS", DRUGCD)),
      PARAM = if_else(DRUGCD == "SGSS", "Total Stanford Genotypic Susceptibility Score of ART Drugs in Regimen", 
                      paste0("Stanford Genotypic Susceptibility Score for ", DRUG)),
      DRUGCLS = if_else(stringr::str_detect(CLASS, "CAPSI"), "CAPSID", CLASS),
      AVAL = case_when(
        is.na(GSS) ~ GSS,
        GSS < 9 ~ 1,
        GSS < 15 ~ 0.75,
        GSS < 30 ~ 0.5,
        GSS < 60 ~ 0.25,
        GSS >= 60 ~ 0,
        TRUE ~ NA_real_
      ),
      AVALCAT1 = case_when(
        AVAL == 1 ~ "Susceptible",
        AVAL == 0.75 ~ "Potential low-level resistance",
        AVAL == 0.5 ~ "Low-level resistance",
        AVAL == 0.25 ~ "Intermediate resistance",
        AVAL == 0 ~ "High-level resistance",
        TRUE ~ NA_character_
      ),    
      AVALCAT2 = case_when(
        AVAL == 0 ~ "0",
        AVAL > 0 & AVAL <= 1 ~ ">0 to 1",
        AVAL > 1 & AVAL <= 2 ~ ">1 to 2",
        AVAL > 2 ~ ">2",
        TRUE ~ NA_character_
      ),
      AVALCA2N = case_when(
        AVAL == 0 ~ 0,
        AVAL > 0 & AVAL <= 1 ~ 1,
        AVAL > 1 & AVAL <= 2 ~ 2,
        AVAL > 2 ~ 3,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(-GSS, -CLASS)
  
  df_out <- ru_labels(df_out, base::labels(df_gf))
  df_out <- ru_labels(df_out, base::labels(df_gss_2))
  df_out <- ru_labels(df_out, newvarlabels)

  print(paste0("RU_HIVRES_GSS: ", "End of RU_HIVRES_GSS"))
  return(as.data.frame(df.out))
}

# Example sequence input (Reverse Transcriptase mutations)
# mutation_seq <- "M184V K103N Y181C"

# Derive resistance score for Reverse Transcriptase gene
# resistance_score <- derive_resistance_score(seq = mutation_seq, gene = "RT")

# Print results
# print(resistance_score)


# artlist <- "some,list,of,values"  # Example macro variable equivalent
# artclasses <- "INSTI+EI+NRTI"  # Example macro variable equivalent

# 
# library(httr)
# library(jsonlite)
# library(dplyr)
# 
# # Function to get HIV Resistance Score from Stanford HIVdb API
# derive_resistance_score <- function(seq, gene = "RT") {
#   # Stanford API endpoint
#   url <- "https://hivdb.stanford.edu/api/hivdb/interpretation/"
#   
#   # Prepare the request body
#   body <- list(
#     seq = seq,
#     gene = gene,
#     version = "9.0"  # API version (latest as of 2025)
#   )
#   
#   # Send POST request to the API
#   response <- POST(url, body = body, encode = "json")
#   
#   # Check if request was successful
#   if (response$status_code == 200) {
#     content <- content(response, as = "parsed", type = "application/json")
#     
#     # Extract drug resistance interpretations
#     interpretations <- content$HIVdbResults$drugResistance
#     resistance_scores <- interpretations %>%
#       purrr::map_dfr(function(drug) {
#         data.frame(
#           Drug = drug$drug,
#           Score = drug$score,
#           Interpretation = drug$interpretation
#         )
#       })
#     
#     return(resistance_scores)
#   } else {
#     stop("Failed to get resistance score. Check input or API availability.")
#   }


# dsetingf    =sdtmdata$pf() %>% dplyr::filter(PFTESTCD == "AA")
# dsetinsuppgf=sdtmdata$supppf()
# dsetingss   =rfmtdata$gss2024()
# gregionvar  ="PFGENRI"
# drugnamelist =NULL
# artlist     =c("CAB", "RPV")
# insertion   =c("INSERTION", "INS", "+")
# deletion    =c("DELETION", "DEL", "-")
# byvars      =c("STUDYID", "USUBJID", "VISITNUM", "VISIT", "PFDTC")
# 
