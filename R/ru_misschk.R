#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_misschk.R
# Purpose: Find variables with all missing values in a data frame
# Date:    08/23/2024
#========================================================================= 

ru_misschk <- function (dsetin) {
  print(paste0("RU_MISSCHK: ", "Start or RU_MISSCHK"))
  
  s.names <- base::names(dsetin)
  n.count <- 0
  n.nrows <- base::nrow(dsetin)
  for (i in 1:length(s.names)) {
    n.n <- sum(is.na(dsetin[s.names[i]]))
    if (n.n == n.nrows) {
      n.count <- n.count + 1
      if (n.count == 1) s.rtn <- s.names[i]
      else s.rtn <- c(s.rtn, s.names[i])
      print(paste0("RTW", "ARNING: RU_MISSCHK: The variable ",  s.names[i], " on the DSETIN dataset (",  
              base::deparse(base::substitute(dsetin)),
              ") contains missing values on all records"))
    }
  }
  print(paste0("Total number of variables with missing values on all records: ", n.count))
}
