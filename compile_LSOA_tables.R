year <- 2007

pollutant <- "NOx"

yearanchor <- read.csv("Data/Historic_stats/YearIMDCensus.csv",
                       check.names=FALSE)

IMDanchor <- read.csv("Data/Historic_stats/IMD_colnames.csv",
                      check.names=FALSE)

sets <- yearanchor %>% dplyr::select(!Year) %>% distinct()
#all the potential names for the join column
namevector <- tibble(col=c("the moon",
                "geography.code",
                "geography code",
                "SOA",
                "FeatureCode",
                "LSOA CODE",
                "LSOA11CD",
                "LSOA code (2011)",
                "LSOA.CODE",
                "LSOA.code..2011.",
                "LSOA"))
for(index in c(1:nrow(sets))){
  setinfo <- sets %>% slice(index) %>% inner_join(IMDanchor, by=c("IMD"="Year"))
  census <- read.csv(paste0("Data/Historic_stats/",
                         setinfo$Census,
                         "_census.csv")) 
  census <- census %>% 
    #rename the geography column for standardisation
    rename("LSOA"=namevector %>% dplyr::filter(col%in%colnames(census)) %>% 
             as.character())
  IMD <- read.csv(paste0("Data/Historic_stats/",
                         setinfo$IMD,
                         "_IMD.csv")) 
  IMD <- IMD %>% 
    #rename the geography column for standardisation
    rename("LSOA"=namevector %>% dplyr::filter(col%in%colnames(IMD)) %>% 
             as.character())
  
  #Refine it until you have only the LSOA and IMD decile
  if(setinfo$Decile=="Yes"){
    #this if i easy as it's just a rename
    IMD <- IMD %>% rename("IMD"=setinfo$Decile.name)
  }else{
    #calculate decile from IMD rank
    if(setinfo$`Trace ID`=="IMD rank"){
      IMD <- IMD %>% mutate(
        IMD=ntile("RANK OF IMD (where 1 is most deprived)",n=10))
    }else{
      #there's only one potential option left now, so use it by process of elimination
      IMD <- IMD %>% mutate(
        IMDrank=min_rank(desc("Value")))
      
      IMD <- IMD %>% mutate(
        IMD=ntile(IMDrank,n=10))
    }
    
  }
  setchunk <- inner_join(census,IMD,
                         by="LSOA")
  write.csv(x=setchunk,
            file=paste0("Data/Historic_stats/set",
                        as.character(setinfo$Set),
                        ".csv")
            )
} 
