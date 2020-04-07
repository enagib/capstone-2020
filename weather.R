## libraries 
library("readxl")
install.packages("GSODR")
library(GSODR)
# install.packages("bigrquery")
# install.packages('devtools')
# devtools::install_github("rstats-db/bigrquery")

## read data
station_list <- read_xlsx("station_list.xlsx")
View(station_list)
