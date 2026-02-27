#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_hivres_ias.R
# Purpose: derive HIV resistance based on US-IAS HIV resistance list
# Date:    02/26/2025
#========================================================================= 

ru_gf_nonaa <- function (
    dsetingf      =sdtmdata$pf() %>% dplyr::filter(PFTESTCD != "AA"),   
    artlist       =c("CAB", "RPV"),
    dsetinsuppgf  =sdtmdata$supppf(),
    gregionvar    ="PFGENRI",
    drugnamevar   ="DRUGNAM",
    drugnamelist  =NULL,
    byvars        =c("STUDYID", "USUBJID", "VISITNUM", "VISIT", "PFDTC", "PFDY")
    ) {
  
  print(paste0("RU_HIVRESIAS: ", "Start of RU_HIVRES_IAS"))
  
  newvarlabels=list("GENROI"="Genetic Region of Interest", "GENROICD"="Genetic Region of Interest",
    "PARCAT1"=" Parameter Category 1", "PARCAT2"=" Parameter Category 2", "PARAMCD"="Parameter Code", "PARAM"="Parameter", 
    "ANL01FL"="Analysis Flag 01", "AVALC"="Analysis Value (C)", "AVAL"="Analysis Value",
    "DTYPE"="Derivation Type", 
    "DRUG"="Drug Name", "DRUGCD"="Drug Name Code", "AVALCAT1"="Analysis Value Category 1", "AVALCAT2"="Analysis Value Category 2", 
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
    "LENACAPAVIR"="LEN",
    "GSK126744"="CAB"
  )
  
  if (is.null(drugnamelist)) {
    drugnamelist <- artfmt
  }
  
  df_gf <- ru_addsupp(dsetingf, dsetinsuppgf)
  str_all_names <- names(df_gf)
  str_domain <- as.character(base::unique(df_gf[["DOMAIN"]]))
  str_strescvar <- paste0(str_domain, "STRESC")
  str_catvar <- paste0(str_domain, "CAT")
  str_testcdvar <- paste0(str_domain, "TESTCD")
  str_testuvar <- paste0(str_domain, "STRESU")
  str_stresnvar <- paste0(str_domain, "STRESN")
  str_testvar <- paste0(str_domain, "TEST")
  
  if (! (str_strescvar %in% str_all_names)) str_strescvar <- paste0(str_domain, "STRES")
  
  # df_gf <- df_gf %>% dplyr::mutate(seqias__=dplyr::row_number())
  df_gf <- df_gf %>% dplyr::filter(! (!! rlang::sym(str_strescvar) %in% c("ND", "NA", "NR", "")) & ! is.na(!! rlang::sym(str_strescvar))) 
  df_gf[["DRUGCD"]] <- as.character(sapply(df_gf[[drugnamevar]], function(x) drugnamelist[[x]]))
  
  df_gf_1 <- df_gf %>% 
    dplyr::mutate(
      ANL01FL="Y",
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
      ),      
      DRUGCD=if_else(DRUGCD=="NULL", NA, DRUGCD),
      testcd__ = !! rlang::sym(str_testcdvar),
      test__ = !! rlang::sym(str_testvar),
      AVAL = !! rlang::sym(str_stresnvar),
      AVALC = !! rlang::sym(str_strescvar),
      DRUG = toupper(!! rlang::sym(drugnamevar)),
      PARCAT1 = !! rlang::sym(str_catvar),
      PARCAT2 = case_when(
        testcd__ == "GSASM" ~ "Genotypic Susceptibility Score (Partial)",
        testcd__ == "PSASM" ~ "Phenotypic Susceptibility Score (Partial)",
        testcd__ == "NETASMT" ~ "Overall Susceptibility Score (Partial)",
        TRUE ~ test__    
      ),
      PARAMCD = case_when(
        testcd__ == "GSASM" ~ paste0("GSSP", DRUGCD),
        testcd__ == "PSASM" ~ paste0("PSSP", DRUGCD),
        testcd__ == "NETASMT" ~ paste0("OSSP", DRUGCD),
        testcd__ == "IC50" ~ paste0("IC50", DRUGCD),
        testcd__ == "IC95" ~ paste0("IC95", DRUGCD),
        testcd__ == "REFIC50" ~ paste0("I50R", DRUGCD),
        testcd__ == "REFIC95" ~ paste0("I95R", DRUGCD),
        testcd__ == "FOLDCHG" ~ paste0("FC", DRUGCD),
        testcd__ == "MAXFLDCH" ~ paste0("MFC", DRUGCD),
        TRUE ~ testcd__
      ),
      PARAM = case_when(
        testcd__ %in% c("GSASM", "PSASM", "NETASMT") ~
          stringr::str_glue("{case_when(
          testcd__ == 'GSASM' ~ 'Genotypic Susceptibility Score (Partial)',
          testcd__ == 'PSASM' ~ 'Phenotypic Susceptibility Score (Partial)',
          testcd__ == 'NETASMT' ~ 'Overall Susceptibility Score (Partial)'
        )} for {stringr::str_to_title(DRUG)}"),
        testcd__ %in% c("IC50", "IC95", "REFIC50", "REFIC95", "FOLDCHG", "MAXFLDCH") ~
          stringr::str_glue("{test__} for {str_to_title(DRUG)}"),
        TRUE ~ test__
      )
    ) %>%
    dplyr::mutate(
      AVAL = case_when(
        testcd__ == "GSASM" & AVALC == "SENSITIVE" ~ 1,
        testcd__ == "GSASM" & AVALC == "RESISTANCE POSSIBLE" ~ 0.5,
        testcd__ == "GSASM" & AVALC == "RESISTANT" ~ 0,
        testcd__ == "PSASM" & AVALC == "SENSITIVE" ~ 1,
        testcd__ == "PSASM" & AVALC == "PARTIALLY SENSITIVE" ~ 0.5,
        testcd__ == "PSASM" & AVALC == "RESISTANT" ~ 0,
        testcd__ == "NETASMT" & AVALC == "SENSITIVE" ~ 1,
        testcd__ == "NETASMT" & AVALC %in% c("PARTIALLY SENSITIVE", "RESISTANT") ~ 0,
        TRUE ~ AVAL
      ),
      AVALCA1N = case_when(
        AVAL == 1 ~ 1,
        AVAL == 0.5 ~ 2,
        AVAL == 0 ~ 3,
        TRUE ~ NA_real_
      )
    ) %>% dplyr::mutate(
      testu__=!! rlang::sym(str_testuvar),
      PARAM = ifelse(!is.na(testu__) & testu__!="", stringr::str_glue("{PARAM} ({testu__})"), PARAM)
    ) %>% dplyr::select(-testu__, -test__, -testcd__, -GENROICD_)
      
  
  # Total Score
  df_gf_2 <- df_gf_1 %>%
    dplyr::filter(stringr::str_sub(PARAMCD, 1, 3) %in% c("OSS", "GSS", "PSS", "SGS") &
             stringr::str_to_upper(DRUGCD) %in% str_to_upper(!! artlist)) %>%
    dplyr::group_by(dplyr::across(all_of(c(byvars, "PARCAT1", "PARCAT2",  str_testcdvar, str_testvar, str_testuvar, "PARAMCD", "PARAM")))) %>%
    dplyr::arrange(AVAL, .by_group = TRUE) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::summarise(AVAL = sum(AVAL, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # Step 4: Add PARAMCD and PARAM based on PARCAT2
  df_gf_3 <- df_gf_2 %>%
    dplyr::mutate(
      DTYPE="Derived",
      PARAMCD = case_when(
        PARCAT2 == "Overall Susceptibility Score (Full)" ~ "T_OSSFRG",
        PARCAT2 == "Overall Susceptibility Score (Partial)" ~ "T_OSSPRG",
        PARCAT2 == "Genotypic Susceptibility Score (Full)" ~ "T_GSSFRG",
        PARCAT2 == "Genotypic Susceptibility Score (Partial)" ~ "T_GSSPRG",
        PARCAT2 == "Phenotypic Susceptibility Score (Full)" ~ "T_PSSFRG",
        PARCAT2 == "Phenotypic Susceptibility Score (Partial)" ~ "T_PSSPRG",
        PARCAT2 == "Stanford Genotypic Susceptibility Score" ~ "T_SGSSRG",
        TRUE ~ NA_character_
      ),
      PARAM = case_when(
        PARCAT2 == "Overall Susceptibility Score (Full)" ~ "Total OSS Full Score of ART Drugs in Regimen",
        PARCAT2 == "Overall Susceptibility Score (Partial)" ~ "Total OSS Partial Score of ART Drugs in Regimen",
        PARCAT2 == "Genotypic Susceptibility Score (Full)" ~ "Total GSS Full Score of ART Drugs in Regimen",
        PARCAT2 == "Genotypic Susceptibility Score (Partial)" ~ "Total GSS Partial Score of ART Drugs in Regimen",
        PARCAT2 == "Phenotypic Susceptibility Score (Full)" ~ "Total PSS Full Score of ART Drugs in Regimen",
        PARCAT2 == "Phenotypic Susceptibility Score (Partial)" ~ "Total PSS Partial Score of ART Drugs in Regimen",
        PARCAT2 == "Stanford Genotypic Susceptibility Score" ~ "Total Stanford Genotypic Susceptibility Score of ART Drugs in Regimen",
        TRUE ~ NA_character_
      ),
      AVALCAT1 = case_when(
        AVAL == 0 ~ "0",
        AVAL > 0 & AVAL <= 1 ~ ">0 to 1",
        AVAL > 1 & AVAL <= 2 ~ ">1 to 2",
        AVAL > 2 ~ ">2",
        TRUE ~ NA_character_
      ),
      AVALCA1N = case_when(
        AVAL == 0 ~ 0,
        AVAL > 0 & AVAL <= 1 ~ 1,
        AVAL > 1 & AVAL <= 2 ~ 2,
        AVAL > 2 ~ 3,
        TRUE ~ NA_real_
      )
    )
  
  df_out <- dplyr::bind_rows(df_gf_1, df_gf_3)
  df_out <- ru_labels(df_out, base::labels(df_gf))
  df_out <- ru_labels(df_out, newvarlabels)
  print(paste0("RU_HIVRESIAS: ", "End of RU_HIVRES_IAS"))
  return(as.data.frame(df_out))
}



