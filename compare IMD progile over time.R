library(PRAWNS)
setwd("C:/Users/Nathan Gray/Documents/GitHub/PRAWNSdata")
Startup()

years <- c(2004,2007,2010,2015,2019)

for(index in c(1:5)){
  
  index <- 1
  slice <- read.csv(
    file=paste0("data-raw/Historic_stats/",years[index],"_IMD.csv")
  ) %>% mutate(score=IMD.SCORE,
               year=years[index]) %>% 
    dplyr::select(score,year)
  
  amalgm <- slice
  
  index <- 2
  
  slice <- read.csv(
    file=paste0("data-raw/Historic_stats/",years[index],"_IMD.csv")
  ) %>% mutate(score=Value,
               year=years[index]) %>% 
    dplyr::select(score,year)
  
  amalgm <- rbind(amalgm,slice)
  
  index <- 3
  
  slice <- read.csv(
    file=paste0("data-raw/Historic_stats/",years[index],"_IMD.csv")
  ) %>% mutate(score=IMD.SCORE,
               year=years[index]) %>% 
    dplyr::select(score,year)
  
  amalgm <- rbind(amalgm,slice)
  
  
  index <- 4
  
  slice <- read.csv(
    file=paste0("data-raw/Historic_stats/",years[index],"_IMD_scores.csv")
  ) %>% mutate(score=Index.of.Multiple.Deprivation..IMD..Score,
               year=years[index]) %>% 
    dplyr::select(score,year)
  
  amalgm <- rbind(amalgm,slice)
  
  index <- 5
  
  slice <- read.csv(
    file=paste0("data-raw/Historic_stats/",years[index],"_IMD.csv")
  ) %>% mutate(score=Index.of.Multiple.Deprivation..IMD..Score,
               year=years[index]) %>% 
    dplyr::select(score,year)
  
  amalgm <- rbind(amalgm,slice)
  
  amalgm <- amalgm %>% group_by(year)
}

ggplot(data=amalgm,
       aes(x=score,
           colour=as.factor(year)))+
  geom_freqpoly()
