#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_hivres_ias.R
# Purpose: derive HIV resistance based on US-IAS HIV resistance list
# Date:    02/26/2025
#========================================================================= 

ru_hivres_ias <- function (
    dsetingf      =sdtmdata$gf() %>% dplyr::filter(GFTESTCD == "AA"),              
    dsetinsuppgf  =sdtmdata$supppf(),
    dsetinias     =rfmtdata$usias22(),
    bydrugnameyn  =FALSE,
    gregionvar    ="PFGENRI",
    insertion     =c("INSERTION", "INS", "+"),
    deletion      =c("DELETION", "DEL", "-"),
    prespecini    =c("H51Y", "T66A/I/K", "L74M", "E92Q/V/G", "Q95K", "T97A", "G118R", "F121Y", "E138A/K/D", 
                     "G140A/C/R/S", "Y143C/H/R/K/S/G/A", "P145S", "Q146P", "S147G", "Q148H/K/R", "V151I/L/A", 
                     "S153F/Y", "N155H/S/T", "E157Q", "G163R/K", "S230R", "R263K", "L68V/I", "L74I", 
                     "E138T", "V151I", "G193E")
    ) {
  
  print(paste0("RU_HIVRESIAS: ", "Start of RU_HIVRES_IAS"))
  
  newvarlabels=list("MCRIT1"="Analysis Multi-Response Criterion 1", "MCRIT1ML"="Multi-Response Criterion 1 Evaluation",
    "MCRIT2"="Analysis Multi-Response Criterion 2", "MCRIT2ML"="Multi-Response Criterion 2 Evaluation",
    "MCRIT3"="Analysis Multi-Response Criterion 3", "MCRIT3ML"="Multi-Response Criterion 3 Evaluation",
    "GENROI"="Genetic Region of Interest", "GENROICD"="Genetic Region of Interest",
    "PARCAT1"=" Parameter Category 1", "PARCAT2"=" Parameter Category 2", "PARAMCD"="Parameter", "PARAM"="Parameter", 
    "ANL01FL"="Analysis Flag 01", "ANL02FL"="Analysis Flag 02", "DTYPE"="Derivation Type", "AVALC"="Analysis Value (C)", 
    "AVAL"="Analysis Value")
  
  df_gf <- ru_addsupp(dsetingf, dsetinsuppgf)
  str_domain <- as.character(base::unique(df_gf[["DOMAIN"]]))
  resultcvar <- paste0(str_domain, "STRESC")
  str_catvar <- paste0(str_domain, "CAT")
  
  df_gf <- df_gf %>% dplyr::mutate(seqias__=dplyr::row_number())
  df_gf <- df_gf %>% dplyr::filter(! (!! rlang::sym(resultcvar) %in% c("ND", "NA", "NR", "")) & ! is.na(!! rlang::sym(resultcvar))) 
  df_gf_1 <- df_gf %>% dplyr::select(all_of(c("seqias__", resultcvar, gregionvar)))
 

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
 

  df_ias <- ru_hivres_splitrules(dsetingf=dsetinias, resultcvar="GENOTYPE", insertion=insertion, deletion=deletion)
  
  if (! is.null(prespecini)) {
    df_specifi <- NULL
    for (i in 1:length(prespecini)) {
      df_specifi_1 <- c("CLASS"="INSTI", SUBCLASS_="PRESP", GENOTYPE=prespecini[i])
      if (is.null(df_specifi)) df_specifi <- df_specifi_1 else df_specifi <- rbind(df_specifi, df_specifi_1)
    }
    df_specifi <- as.data.frame(df_specifi)
    names(df_specifi$CLASS) <- NULL
    names(df_specifi$SUBCLASS_) <- NULL
    
    df_specifi_2 <- ru_hivres_splitrules(dsetingf=df_specifi, resultcvar="GENOTYPE", insertion=insertion, deletion=deletion) %>%
      dplyr::select(-GENOTYPE)
    
    df_ias <- dplyr::full_join(x=df_ias, y=df_specifi_2, by=c("CLASS", "GENLOC", "REFRES", "AAS", "RESCAT", "MUTATION", "CODON", "AASSEQ")) %>% 
      dplyr::mutate(
        SUBCLASS=if_else(is.na(SUBCLASS_) | SUBCLASS_=="", SUBCLASS, SUBCLASS_)
      ) %>% dplyr::select(-SUBCLASS_)
    
    df_ias_chk <- df_ias %>% dplyr::filter(CLASS == "INSTI" & toupper(SUBCLASS)=="MAJOR") 
    if (nrow(df_ias_chk) > 0) {
      print(paste0("RTE", "ERROR: List of Pre-Specified Mutations which Are Not in IAS Dictionary"))
      print(df_ias_chk)
      print("End of Print --------------------------------------------------------------------")
    }
  } 
  
  df_ias_2 <- df_ias %>% 
    dplyr::mutate(        
      GENROICD=case_when(
        CLASS == "INSTI"  ~ "IN",
        CLASS == "PI"     ~ "PR",
        CLASS == "CAPSID" ~ "FI",
        CLASS == "NRTI"   ~ "RT",
        CLASS == "NNRTI"  ~ "RT",
        TRUE ~ NA
      ),
      DRUG_CODE = case_when(
        stringr::str_detect(toupper(DRUG_NAME), "INSERTION") ~ "ALLRT",
        stringr::str_detect(toupper(DRUG_NAME), "COMPLEX")  ~ "RTNT",
        stringr::str_detect(toupper(DRUG_NAME), "THYMIDINE") ~ "TAM",
        TRUE ~ DRUG_CODE
      ),
      # DRUG_CODE="NSP", DRUG_NAME="Not Specified", 
      AASEQ_=if_else(AAS == REFRES & ! is.na(REFRES) & REFRES != "", -1, 0)
    )
  
  df_ias_rule <- df_ias_2 %>% 
    dplyr::filter(toupper(SUBCLASS) %in% c("MAJOR", "PRESP")) %>%
    dplyr::group_by(GENROICD, CODON, AAS) %>% 
    dplyr::arrange(desc(SUBCLASS), AASEQ_, .by_group = TRUE) %>%
    dplyr::slice_head(n=1) %>% dplyr::ungroup() %>%
    dplyr::select(GENROICD, CODON, AAS) %>%
    dplyr::group_by(GENROICD, CODON) %>% 
    tidyr::pivot_wider(names_from = AAS, values_from = AAS, names_prefix = "WASS") %>%
    tidyr::unite(col="IASRULES", starts_with("WASS"), sep="/", na.rm = TRUE) %>% 
    dplyr::mutate(IASRULES=paste0(CODON, IASRULES))
  
  df_ias_3 <- dplyr::full_join(df_ias_2, df_ias_rule) %>%
    group_by(GENROICD, CODON, AAS) %>%
    dplyr::arrange(AAS, RESCAT, SUBCLASS, .by_group = TRUE) %>% 
    dplyr::slice_head(n=1) %>% dplyr::ungroup() %>%
    dplyr::select(GENROICD, CODON, AAS, RESCAT, SUBCLASS, IASRULES, CLASS) 
    
  df_gf_3 <- dplyr::left_join(df_gf_2, df_ias_3, by=c("GENROICD", "CODON", "AAS", "RESCAT")) %>%
    dplyr::mutate(
      SUBCLASS_=if_else(toupper(SUBCLASS) %in% c("MAJOR", "PRESP"), TRUE, FALSE),
      MCRIT1=if_else(SUBCLASS_, "Any Mutation", NA),
      MCRIT2=if_else(SUBCLASS_ & toupper(SUBCLASS) == "PRESP", "Specified Mutation", 
          if_else(SUBCLASS_ & toupper(SUBCLASS) == "MAJOR", "Major Mutation", NA)),
      MCRIT3=if_else(SUBCLASS_ & toupper(SUBCLASS) == "PRESP", "Specified Mutation Class", 
          if_else(SUBCLASS_ & toupper(SUBCLASS) == "MAJOR", "Major Mutation Class", NA)),     
      MCRIT3ML=case_when(
        SUBCLASS_ & toupper(CLASS) == "NRTI" ~ "NRTI",
        SUBCLASS_ & toupper(CLASS) == "NNRTI" ~ "NNRTI",
        SUBCLASS_ & toupper(CLASS) == "PR" ~ "PI",
        SUBCLASS_ & toupper(CLASS) == "PI" ~ "PI",
        SUBCLASS_ & toupper(CLASS) == "EI" ~ "FI",
        SUBCLASS_ & toupper(CLASS) == "CAPSID" ~ "FI",
        SUBCLASS_ & toupper(CLASS) == "INSTI" ~ "INSTI",
        SUBCLASS_ ~ "INSTI",
        TRUE ~ NA
      ),
      MCRIT1ML=if_else(SUBCLASS_, IASRULES, NA),
      MCRIT2ML=if_else(SUBCLASS_, MUTATION, NA)
    ) %>% 
    dplyr::mutate(
      GENLOC_mod = case_when(
        str_length(GENLOC) == 1 ~ paste0("00", str_trim(GENLOC)),
        str_length(GENLOC) == 2 ~ paste0("0", str_trim(GENLOC)),
        TRUE ~ str_trim(GENLOC)
      )
    ) %>% 
    dplyr::select(-all_of(c("SUBCLASS", "CLASS", "SUBCLASS_", gregionvar, "IASRULES", "MUTATION")))
  
  df_gf_5_1 <- df_gf_3 %>% dplyr::filter(! is.na(MCRIT2)) %>%
    dplyr::mutate(
      REFRES = if_else(is.na(REFRES), "", REFRES),
      PARAMCD = case_when(
        RESCAT == "DELETION" ~ paste0(GENROICD, REFRES, GENLOC_mod, AAS, if_else(is.na(AASSEQ), "", AASSEQ), "D"),
        TRUE ~ paste0(GENROICD, REFRES, GENLOC_mod, AAS, if_else(is.na(AASSEQ), "", AASSEQ))
      ),
      PARAM = case_when(
        RESCAT == "INSERTION" & is.na(AASSEQ) ~ paste0("Major Mutation at ", GENROICD, " DNA reg. at Codon ", GENLOC, AAS, " Insertion ", AASSEQ),
        RESCAT == "DELETION"  & is.na(AASSEQ) ~ paste0("Major Mutation at ", GENROICD, " DNA reg. at Codon ", GENLOC, AAS, " Deletion ", AASSEQ),
        RESCAT == "INSERTION" ~ paste0("Major Mutation at ", GENROICD, " DNA reg. at Codon ", CODON, AAS, " Insertion "),
        RESCAT == "DELETION"  ~ paste0("Major Mutation at ", GENROICD, " DNA reg. at Codon ", CODON, AAS, " Deletion "),        
        TRUE ~ paste0("Major Mutation at ", GENROICD, " DNA reg. at Codon ", CODON, AAS)
      ),
      AVAL = 1,
      AVALC = MCRIT2ML,
      DTYPE = "DERIVED",
      ANL01FL = "Y",
      ANL02FL = ""
    )
  
  df_gf_5_2 <- df_gf_3 %>%
    dplyr::mutate(       
      REFRES = if_else(is.na(REFRES), "", REFRES),
      PARAMCD = paste0(GENROICD, REFRES, GENLOC_mod, if_else(is.na(AASSEQ), "", AASSEQ)),
      PARAM = case_when(
        RESCAT == "INSERTION" & is.na(AASSEQ) ~ paste0("Substitution in ", GENROICD, " DNA reg. at Codon ", CODON, " Insertion "),
        RESCAT == "DELETION"  & is.na(AASSEQ) ~  paste0("Substitution in ", GENROICD, " DNA reg. at Codon ", CODON, " Deletion"),        
        RESCAT == "INSERTION" ~ paste0("Substitution in ", GENROICD, " DNA reg. at Codon ", CODON, " Insertion ", AASSEQ),
        RESCAT == "DELETION" ~  paste0("Substitution in ", GENROICD, " DNA reg. at Codon ", CODON, " Deletion", AASSEQ),
        TRUE ~                  paste0("Substitution in ", GENROICD, " DNA reg. at Codon ", CODON)
      ),
      AVAL = 1,
      AVALC = !! rlang::sym(resultcvar),
      DTYPE = "",
      ANL01FL = "",
      ANL02FL = "Y"
    )
  
  df_gf_5 <- dplyr::bind_rows(df_gf_5_1, df_gf_5_2) %>% dplyr::select(-all_of(c("GENLOC_mod", resultcvar, "AASSEQ", "REFRES")))
  df_gf_5[["PARAMCD"]] <- stringr::str_replace_all(df_gf_5[["PARAMCD"]], c("_1" = "A", "_2" = "B", "_3" = "C", "_4" = "D"))
    
  df_out <- dplyr::left_join(df_gf, df_gf_5, by=c("seqias__")) %>% 
    dplyr::mutate(
      PARCAT1 = !! rlang::sym(str_catvar),
      PARCAT2 = "Amino Acid",  # Assuming CAT was also set to 'Amino Acid'
    ) %>% dplyr::select(-seqias__)
  
  df_out <- ru_labels(df_out, base::labels(df_gf))
  df_out <- ru_labels(df_out, base::labels(df_ias))
  df_out <- ru_labels(df_out, newvarlabels)
  print(paste0("RU_HIVRESIAS: ", "End of RU_HIVRES_IAS"))
  return(as.data.frame(df_out))
}



