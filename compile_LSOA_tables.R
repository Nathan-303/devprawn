year <- 2007

pollutant <- "NOx"

yearanchor <- read.csv("Data/Historic_stats/YearIMDCensus.csv")

sets <- yearanchor %>% dplyr::select(!Year) %>% distinct

for(index in c(1:nrow(sets))){
  IMD <- read.csv()
  ethnicity <- read.csv()
  print(index)
} 
