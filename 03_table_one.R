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

table1 <- CreateTableOne(vars = listVars, data = mtable4, factorVars = catVars)

View(table1)

#table according to CDI_status

#Some discordance between cdi and cdi_status
#0 adl_total does not make
table2 <- CreateTableOne(listVars, mtable4, catVars, strata = c("cdi"))
table2

# Load the packages
library(ReporteRs)
library(magrittr)

docx( ) %>% 
  addFlexTable(table2 %>%
                 FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                           header.text.props = textBold( color = "white" ),
                           add.rownames = TRUE ) %>%
                 setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
  writeDoc(file = "table2.docx")