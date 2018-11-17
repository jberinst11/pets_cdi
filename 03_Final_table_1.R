#install packages
install.packages("tableone")
install.packages("ReporteRs")
install.packages("magrittr")

#Load package
library(tableone)
library(ReporteRs)
library(magrittr)

#Create a variable list which we want in Table 1
listVars <- c("gender", "age", "race", "antibiotics3mo","ppi","h2ra", "adl_total") 

#Define categorical variables
catVars <- c("gender", "race", "antibiotics3mo","ppi","h2ra", "adl_total")

#table according to CDI_status

#Some discordance between cdi and cdi_status
#0 adl_total does not make
table1 <- CreateTableOne(listVars, mtable5, catVars, strata = c("cdi_status"))
table1

