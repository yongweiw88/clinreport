#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_hivres_splitrules.R
# Purpose: Split genotypic mutation results 
# Date:    04/16/2025
#========================================================================= 
#
# library(stringr)
#
ru_hivres_splitrules <- function (
  dsetingf=sdtmdata$gf() %>% dplyr::filter(GFTESTCD == "AA"), 
  resultcvar="GFSTRESC", 
  insertion=c("INSERTION", "INS", "+"),
  deletion=c("DELETION", "DEL", "-")) {
  print(paste0("RU_HIVRESIAS: ", "Start of RU_HIVRES_SPLITMUTATIONS"))
  
  newvarlabels <- list("GENLOC"="Genetic Location", "REFRES"="Reference Result Value", 
                       "AAS"="Amino acid substitution conferring resistance", "RESCAT"="Result Category",
                       "MUTATION"="Mutation", "CODON"="Amino Acid Condon",
                       "AASSEQ"="Amino acid substitution conferring resistance sequence")

  
  df_gf <- dsetingf %>% dplyr::mutate(seqsp__=dplyr::row_number())
  df_gf_1 <- df_gf %>% dplyr::select(all_of(c("seqsp__", resultcvar)))
  
  df_gf_1[["REFRES"]] <- stringr::str_replace(df_gf_1[[resultcvar]], "\\d.*", "")
  vec_genloc <- stringr::str_replace(df_gf_1[[resultcvar]], "/.*", "")
  vec_genloc2 <- stringr::str_extract(vec_genloc, "[0-9_]+")  
  df_gf_1[["GENLOC"]] <- stringr::str_replace(vec_genloc2, "_.*", "")
  vec_mutations <- stringr::str_replace(df_gf_1[[resultcvar]], "^.*\\d+", "")
  df_gf_1[["AAS"]] <- stringr::str_split(vec_mutations, "/")
  
  df_gf_2 <- df_gf_1 %>% 
    tidyr::unnest(AAS, keep_empty = TRUE) %>%
    dplyr::mutate(
      AASSEQ=if_else(stringr::str_detect(AAS, "_"),  stringr::str_sub(AAS, 2, 2), NA_character_),
      AAS=if_else(stringr::str_detect(AAS, "_"),  stringr::str_sub(AAS, 1, 1), AAS),
      RESCAT=if_else(REFRES %in% rlang::syms(insertion), "INSERTION", 
                     if_else(REFRES %in% rlang::syms(deletion), "DELETION", NA_character_)),
      CODON=paste0(REFRES, GENLOC),
      MUTATION=paste0(REFRES, GENLOC, AAS)
    )

  df_out <- dplyr::full_join(x=df_gf, y=df_gf_2) %>% dplyr::select(-seqsp__)
  df_out <- ru_labels(df_out, base::labels(dsetingf))
  df_out <- ru_labels(df_out, newvarlabels)

  print(paste0("RU_HIVRES_SPLITMUTATIONS: ", "End of RU_HIVRES_SPLITMUTATIONS"))
  return(df_out)
}

