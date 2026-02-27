#===================================================================== 
# Author:  Yongwei Wang
# Program: ru_whozscore.R
# Purpose: Calculate WHO/CDC Height/Weight z-score for children.
# Date:    04/06/2025
#===================================================================== 

ru_whozscore <- function (
  dsetin     =base::subset(adamdata$advs(), PARAMCD %in% c("HEIGHT", "WEIGHT")), # Input dataset 
  dsetinref  =rfmtdata$whofazs(), # Input WHO or CDC Z-Score dataset 
  zscoretype =c("WHO", "CDC"),     # WHO or CDC Zscore 
  parambyvars=c("STUDYID", "USUBJID", "AVISITN", "AVISIT"),
  presubset  =NULL,
  brthdtvar  ="BRTHDT",  # Birth date variable 
  sexvar     ="SEX",     # Sex variable 
  adtvar     ="ADT",     # Analysis datae variable. adtvar - brthdtvar + 1 will be the age in calculation 
  avalvar    ="AVAL",    # Analysis result variable. It is required if PARAMCDVAR is given  
  paramcdvar ="PARAMCD", # Variable where type test is saved. If leave it blank, existing variable should be given in HEIGHTARAMCD/WEIGHTPARAMCD/BMIPARAMCD. 
  paramvar   ="PARAM",   # It is only needed if DERIVEBMIYN=Y and PARAMCDVAR is given. 
  heightparamcd="HEIGHT", # Height variable name in CM. If PARAMCDVAR is given, it is a values of PARAMCDVAR. Other wise, it is an existing variable name 
  weightparamcd="WEIGHT", # Weight variable name in KG. If PARAMCDVAR is given, it is a values of PARAMCDVAR. Other wise, it is an existing variable name 
  linearlsmyn="Y",       # If linear imputating L/S/M for month based L/S/M (age>=5 years)  
  lengthposcond="VSPOS=='DECUBITUS'"  # Statement to apply for length instead of height 
) {
  
  if (is.null(lengthposcond)) lengthposcond <- "1==2"
  # Sorting dataset
  
  var_dsetinvars <- names(dsetin)
  parambyvars <- base::intersect(parambyvars, var_dsetinvars)
  
  var_dropvars <- c("AAGE", "AAGEU", "AHEIGHT", "PARAM", "AVAL", "AVALC", "ALOH", "BASE", "BASEC", "CHG", "PCHG", "PARAMLBL", "PARAMN", "PARAMCD")
  var_dropvars <- base::intersect(var_dropvars, var_dsetinvars)
  var_keepvars <- base::setdiff(var_dsetinvars, var_dropvars)
  
  sorted_data <- dsetin %>% dplyr::arrange(!!! rlang::syms(parambyvars), !! rlang::sym(adtvar)) %>% dplyr::mutate(seq__=dplyr::row_number())
  
  # Filtering for height parameter and creating loh_
  height_data <- sorted_data %>%
    dplyr::filter(toupper(!!rlang::sym(paramcdvar)) == toupper(!! heightparamcd) & !! rlang::sym(paramcdvar) != "" & ! is.na(!! rlang::sym(paramcdvar))) %>% 
    dplyr::mutate(loh_ = case_when(
      !! rlang::parse_expr(lengthposcond) ~ "L",
      TRUE ~ "H"
    )) 
  
  # Merging height data back to original dataset
  merged_data <- sorted_data %>%
    dplyr::left_join(height_data %>% dplyr::select(!!! rlang::syms(parambyvars), !! rlang::sym(adtvar), !! rlang::sym(avalvar), loh_) %>%
              dplyr::distinct() %>% dplyr::rename(ht_cm_ = !! rlang::sym(avalvar)),
              by = c(parambyvars, adtvar)) 
  
  # Deriving additional variables
  derived_data <- merged_data %>%
    dplyr::mutate(
      age_day_ = as.numeric(as.Date(!! rlang::sym(adtvar)) - as.Date(!! rlang::sym(brthdtvar))),
      agemos = age_day_ / 30.4375,
      AGEU = case_when(
        is.na(age_day_) ~ NA_character_,
        !! zscoretype[1] == "WHO" & age_day_ >= 1857 ~ "M",
        !! zscoretype[1] == "CDC" ~ "M",
        !is.na(age_day_) ~ "D",
        TRUE ~ NA_character_
      ),
      sexn_ = case_when(
        toupper(substr(!! rlang::sym(sexvar), 1, 1)) == "M" ~ 1,
        toupper(substr(!! rlang::sym(sexvar), 1, 1)) == "F" ~ 2,
        TRUE ~ NA_real_
      ),
      age_ = case_when(
        !! zscoretype[1] == "WHO" & age_day_ >= 1857 ~ floor(agemos),
        !! zscoretype[1] == "CDC" & agemos > 0 & agemos < 0.5  ~ 0,
        !! zscoretype[1] == "CDC" ~ round(agemos+0.5)-0.5,
        TRUE ~ age_day_
      ),
      LOH = case_when(
        age_day_ >= 731 ~ "H",
        age_day_ < 731 & loh_ == "H" ~ "L",
        ! is.na(loh_) ~ loh_,
        is.na(age_day_) & ht_cm_ < 0 ~ NA,
        is.na(age_day_) & ht_cm_ >= 0 & ht_cm_ < 87 ~ "L",
        is.na(age_day_) & ht_cm_ >= 87 ~ "H",
        age_day_ < 731 ~ "L",
        age_day_ >= 731 ~ "H",
        TRUE ~ NA
      ),
      ht_cm_ = case_when(
        is.na(age_day_) ~ ht_cm_,
        age_day_ < 731 & loh_ == "H" & toupper(!! zscoretype[1]) == "WHO" ~ ht_cm_ + 0.7,
        age_day_ >= 731 & loh_ == "L" & toupper(!! zscoretype[1]) == "WHO" ~ ht_cm_ - 0.7,
        TRUE ~ ht_cm_
      )
    )
  
  # Generating final dataset
  dsetin2_1 <- derived_data %>% dplyr::filter(toupper(!! rlang::sym(paramcdvar)) == toupper(!! heightparamcd)) %>% 
    dplyr::mutate(
      TYPE="HEIGHT",
      aval_=ht_cm_,
      subtyp_="FOR AGE",
      aolohref_=age_,
      aolohval_=agemos
    )
  dsetin2_2 <- derived_data %>% dplyr::filter(toupper(!! rlang::sym(paramcdvar)) == toupper(!! weightparamcd)) %>%
    dplyr::mutate(
      TYPE="WEIGHT",
      aval_=!! sym(avalvar),
      subtyp_="FOR AGE",
      aolohref_=age_,
      aolohval_=agemos
    )    
  dsetin2_3 <- derived_data %>% dplyr::filter(toupper(!! rlang::sym(paramcdvar)) == toupper(!! weightparamcd)) %>%
    dplyr::mutate(
      TYPE="WEIGHT",
      aval_=!! sym(avalvar),
      subtyp_="FOR LENGTH/HEIGHT",
      aolohref_=case_when(
        !! zscoretype[1] == "WHO" ~ round(ht_cm_, 1),
        !! zscoretype[1] == "CDC" & ht_cm_ > 0 & ht_cm_ < 0.5 ~ 0,
        !! zscoretype[1] == "CDC" ~ round(ht_cm_ + 0.5, 0) - 0.5,
        TRUE ~ NA_real_
      ),
      aolohval_=ht_cm_,
      AGEU=NA_character_
    )
  dsetin2_4 <- derived_data %>% dplyr::filter(toupper(!! rlang::sym(paramcdvar)) == toupper(!! weightparamcd)) %>%
    dplyr::mutate(
      TYPE="BMI",
      LOH=ifelse(!! zscoretype[1] == "WHO", LOH, NA_character_),
      aval_=!! rlang::sym(avalvar)/((ht_cm_/100)^2),
      subtyp_="FOR AGE",
      aolohref_=age_,
      aolohval_=agemos
    )

  dsetin2 <- rbind(dsetin2_1, dsetin2_2, dsetin2_3, dsetin2_4) %>% 
    dplyr::select(seq__, LOH,aval_, TYPE,  sexn_, AGEU, aolohval_, subtyp_, aolohref_)
  
  names(dsetinref) <- toupper(names(dsetinref))

  # Processing reference dataset
  ref_data <- dsetinref %>%
    mutate(
      subtyp_ = case_when(
        is.na(AGE) & !is.na(LOHVAL) ~ "FOR LENGTH/HEIGHT",
        !is.na(AGE) & is.na(LOHVAL) ~ "FOR AGE",
        TRUE ~ NA_character_
      ),
      AGEU = case_when(
        is.na(AGE) & !is.na(LOHVAL) & !! zscoretype[1] == "WHO" ~ "D",
        TRUE ~ AGEU
      ),
      aolohref_ = case_when(
        is.na(AGE) & !is.na(LOHVAL) ~ LOHVAL,
        !is.na(AGE) & is.na(LOHVAL) ~ AGE,
        TRUE ~ NA_real_
      ),
      AGEU = toupper(AGEU),
      TYPE = toupper(TYPE),
      LOH = toupper(LOH)
    ) %>%
    select(-AGE, -LOHVAL) %>%
    rename(sexn_ = SEX)
  
  # Creating refwaz1 and refwaz2 datasets
  refwaz1 <- ref_data %>% rename(l_ = L, m_ = M, s_ = S)
  refwaz2 <- ref_data %>%
    filter(toupper(AGEU) != "D") %>%
    mutate(aolohref_ = aolohref_ - 1) %>%
    rename(l2_ = L, m2_ = M, s2_ = S)
  
  # Merging input dataset with reference dataset
  waz_data <- dsetin2 %>%
    left_join(refwaz1, by = c("TYPE", "subtyp_", "sexn_", "aolohref_", "LOH", "AGEU")) %>%
    left_join(refwaz2, by = c("TYPE", "subtyp_", "sexn_", "aolohref_", "LOH", "AGEU")) %>%
    mutate(
      AVALC = case_when(
        is.na(l2_) & !is.na(aolohref_) & subtyp_ == "FOR AGE" ~ paste("Out of", !! zscoretype[1], "Z-Score Age Range"),
        is.na(l2_) & !is.na(aolohref_) & subtyp_ == "FOR LENGTH/HEIGHT" & LOH == "L" ~ paste("Out of", !! zscoretype[1], "Z-Score Length Range"),
        is.na(l2_) & !is.na(aolohref_) & subtyp_ == "FOR LENGTH/HEIGHT" & LOH == "H" ~ paste("Out of", !! zscoretype[1], "Z-Score Height Range"),
        is.na(aolohref_) & subtyp_ == "FOR AGE" ~ "Missing Analysis Age",
        is.na(aolohref_) & subtyp_ == "FOR LENGTH/HEIGHT" & LOH == "H" ~ "Missing Analysis Height",
        is.na(aolohref_) & subtyp_ == "FOR LENGTH/HEIGHT" & LOH == "L" ~ "Missing Analysis Length",
        TRUE ~ NA_character_
      )
    )
  
  if (linearlsmyn == "Y") {
    waz_data <- waz_data %>%
      mutate(
        l_=ifelse(! (AGEU =='D') & ! (aolohval_ == aolohref_), (aolohref_ + 1) * l_ - aolohref_ * l2_ + aolohval_ * (l2_ - l_), l_),
        m_=ifelse(! (AGEU =='D') & ! (aolohval_ == aolohref_), (aolohref_ + 1) * m_ - aolohref_ * m2_ + aolohval_ * (m2_ - m_), m_),
        s_=ifelse(! (AGEU =='D') & ! (aolohval_ == aolohref_), (aolohref_ + 1) * s_ - aolohref_ * s2_ + aolohval_ * (s2_ - s_), s_)
      )
  }
  
  # Derive Z score
  if (zscoretype[1] == "WHO") {
    waz_data_2 <- waz_data %>%
      mutate(   
        sd2pos_ =m_ * ((1+2 * l_ * s_)**(1/l_)),
        sd3pos_ =m_ * ((1+3 * l_ * s_)**(1/l_)),
        sd2neg_ =m_ * ((1-2 * l_ * s_)**(1/l_)),
        sd3neg_ =m_ * ((1-3 * l_ * s_)**(1/l_)),
        z_ = ((aval_/m_)**l_ - 1)/(s_ * l_),
        z_ = case_when(
          is.na(z_) ~ NA_real_,
          z_ > 3 ~ 3 + (aval_ -sd3pos_)/(sd3pos_ - sd2pos_),
          z_ <= 3 ~ -3 - (sd3neg_ - aval_)/(sd2neg_ - sd3neg_),
          TRUE ~ z_
        )
      )
  } else {
    waz_data_2 <- waz_data %>%
      mutate( 
        z_=case_when(
          is.na(l_) ~ NA_real_,
          abs(l_) >= 0.01 ~ ((aval_ / m_)**l_ - 1)/(s_ * l_),
          abs(l_) < 0.01 ~ log(aval_ / m_)/ s_,
          TRUE ~ NA_real_
        ),
        sdl=((m_ - m_*(1 - 2 * l_ * s_)**(1 / l_)) / 2),
        sdh=((m_ * (1 + 2 * l_ * s_)**(1 / l_) - m_) / 2),
        z2_=ifelse(aval_ < m_, (aval_ - m_) / sdl, (aval_ - m_) / sdh)
      )   
  }
  
  # Create output dataset
  l_n <- if_else(zscoretype[1] == 'CDC', 2, 1)
  
  output_list <- list()
  for (l_i in 1:l_n) {
    if (l_i == 2) l_var <- "Modified" else l_var <- NULL
    # l_var <- if_else(l_i == 2, 'Modified', NULL)
    l_var2 <- if_else(l_i == 2, paste0(' ', zscoretype[1], 'M'), paste0(' ', zscoretype[1]))
    
    waz_temp <- waz_data_2 %>%
      mutate(
        AVAL = ifelse(!! l_i == 1, z_, z2_),
        PARAMCD = case_when(
          subtyp_ == 'FOR LENGTH/HEIGHT' & LOH == 'H' ~ paste0(l_var2, 'WFHZ'),
          subtyp_ == 'FOR LENGTH/HEIGHT' & LOH == 'L' ~ paste0(l_var2, 'WFLZ'),
          subtyp_ == 'FOR AGE' & TYPE == 'WEIGHT' ~ paste0(l_var2, 'WFAZ'),
          subtyp_ == 'FOR AGE' & TYPE == 'HEIGHT' ~ paste0(l_var2, 'HFAZ'),
          subtyp_ == 'FOR AGE' & TYPE == 'BMI' ~ paste0(l_var2, 'BFAZ'),
          subtyp_ == 'FOR AGE' & TYPE == 'HC' ~ paste0(l_var2, 'HCFAZ'),
          subtyp_ == 'FOR AGE' & TYPE == 'AC' ~ paste0(l_var2, 'ACFAZ'),
          subtyp_ == 'FOR AGE' & TYPE == 'SS' ~ paste0(l_var2, 'SSFAZ'),
          subtyp_ == 'FOR AGE' & TYPE == 'TS' ~ paste0(l_var2, 'TSFAZ'),
          TRUE ~ NA_character_
        ),
        PARAM = case_when(
          subtyp_ == 'FOR LENGTH/HEIGHT' & LOH == 'H' ~ paste(zscoretype[1], !! l_var, ' Weight for Height Z-Score', sep=''),
          subtyp_ == 'FOR LENGTH/HEIGHT' & LOH == 'L' ~ paste(zscoretype[1], !! l_var, ' Weight for Length Z-Score', sep=''),
          subtyp_ == 'FOR AGE' & TYPE == 'WEIGHT' ~ paste(zscoretype[1], !! l_var, ' Weight for Age Z-Score', sep=''),
          subtyp_ == 'FOR AGE' & TYPE == 'HEIGHT' ~ paste(zscoretype[1], !! l_var, ' Height/Length for Age Z-Score', sep=''),
          subtyp_ == 'FOR AGE' & TYPE == 'BMI' ~ paste(zscoretype[1], !! l_var, ' BMI for Age Z-Score', sep=''),
          subtyp_ == 'FOR AGE' & TYPE == 'HC' ~ paste(zscoretype[1], !! l_var, ' Head Circumference for Age Z-Score', sep=''),
          subtyp_ == 'FOR AGE' & TYPE == 'AC' ~ paste(zscoretype[1], !! l_var, ' Arm Circumference for Age Z-Score', sep=''),
          subtyp_ == 'FOR AGE' & TYPE == 'SS' ~ paste(zscoretype[1], !! l_var, ' Subscapular Skinfold for Age Z-Score', sep=''),
          subtyp_ == 'FOR AGE' & TYPE == 'TS' ~ paste(zscoretype[1], !! l_var, ' Triceps Skinfold for Age Z-Score', sep=''),
          TRUE ~ NA_character_
        )
      ) %>%
      mutate(PARAM = trimws(PARAM))
    
    output_list[[l_i]] <- waz_temp
  }
  
  waz_data_3 <- bind_rows(output_list)
  
  dsetout <- sorted_data %>%
    select(-all_of(var_dropvars)) %>%
    left_join(waz_data_3, by = "seq__") %>%
    mutate(
      ALOH = case_when(
        LOH == "H" ~ "Height",
        LOH == "L" ~ "Length",
        TRUE ~ NA_character_
      ),
      AAGE = ifelse(toupper(subtyp_) == "FOR AGE", aolohval_, NA_real_),
      AAGEU = ifelse(toupper(subtyp_) == "FOR AGE", AGEU, NA_character_),
      AHEIGHT = ifelse(toupper(subtyp_) == "FOR LENGTH/HEIGHT", aolohval_, NA_real_)
    ) %>%
    select(all_of(var_keepvars), AAGE, AAGEU, AHEIGHT, PARAMCD, PARAM, AVAL, AVALC, ALOH)
  
  newvarlabels <- list("PARAMCD"="Parameter Code", "PARAM" = "Parameter",  AVAL = "Analysis Value", AVALC = "Analysis Value (C)",
                       "ALOH"="Type of Measurement: Length or Height", "AAGE"="Analysis Age", 
                       "AAGEU"="Analysis Age Unit", "AHEIGHT"="Analysis Height/Length")
  
  dsetout <- ru_labels(dsetout, newvarlabels)
  this_vars <- base::intersect(names(dsetin), names(dsetout))
  if (length(this_vars) > 0 && ! (this_vars[1] == "")) {
    this_labels <- base::lapply(dsetin, base::attr, "label")[this_vars]
    dsetout <- ru_labels(dsetout, this_labels)
  }
  return(dsetout)
}


    
