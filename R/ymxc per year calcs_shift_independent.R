library(PRAWNS)
library(PRAWNSdata)
Startup()
setwd("C:/Users/Nathan Gray/Documents/GitHub/Data-analysis-with-PRAWNS-demo")

year <- 2019
pollutant <- "NOx"
#run the data processing if it hasn't already
if(exists(paste0("An expected file from the running of the preprocessing"))==FALSE){
  preprocessing(pollutant)
}

for(year in c(2012:2020)){
  ##read in the relevant csv
  data <- read.csv(file = paste0("Data/Historic_PRAWNS/",
                                 year,
                                 "_",
                                 pollutant,
                                 "_PRAWN_setFgeogs.csv"),
                   check.names = FALSE
  )
  data <- data[-1]
  #read in the relevant matching set
 
    data <- inner_join(data,
                       ListSets[[6]],
                       by="LSOA")
  #At this point you have the pollution and demographic data stitched together, begin refinement
  
  
  long_chunk <- data %>% pivot_longer(
    cols= setdiff(names(data),
                  c("IMD","LSOA","Ethnic group","flat_population","broad_group")),
    names_to = "Emission_source",
    values_to = "emissions")%>% 
    mutate(`Weighted emissions`= emissions*flat_population)
  #Plot a faceted graph
  
  
  models <- long_chunk %>% group_by(`Ethnic group`,Emission_source) %>% 
    do(model = lm(emissions ~ IMD,
                  weights = flat_population,
                  data = .))
  
  extracts <- models %>% mutate(
    intercept=model[1]$coefficients[1],
    multiplier=model[1]$coefficients[2]
  ) %>% dplyr::select(c(`Ethnic group`,intercept,multiplier,Emission_source))
  
  rm(models)
  
  delta <- extracts %>% mutate(IMD1=signif(intercept+(1*multiplier),digits=5),
                               IMD10=signif(intercept+(10*multiplier),digits=5)) %>% 
    mutate(deltabsolute=IMD1-IMD10,
           deltpercentage =((IMD1/IMD10)-1)*100,
           year=year)
  
  
  if(exists("compiled_years")==FALSE){
    compiled_years <- delta
  }else{
    compiled_years <- rbind(compiled_years,delta)
  }
  #close year loop
}
compiled_years <- compiled_years %>% tibble
out <- ggplot(data=compiled_years %>% dplyr::filter(`Ethnic group`%in%c("Minoritised white",
                                                                   "Asian Asian British or Asian Welsh",
                                                                   "Black Black British Black Welsh Caribbean or African",
                                                                   "White English Welsh Scottish Northern Irish or British",
                                                                   "Mixed or Multiple ethnic groups",
                                                                   "Other ethnic group"
))%>% group_by(`Ethnic group`))  +
  aes(x=year,
      y=deltabsolute,
      colour=`Ethnic group`)+
  geom_point()+
  geom_line()+
  facet_wrap(~Emission_source,
             scale="free_y")
  


out

process_graph_saver(plot=out,
                    filename = paste0("Outputs/",
                                      pollutant,
                                      " absolute inequality between 1 and 10 over time_setFgeoms2.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.6)


majority <- compiled_years %>% dplyr::filter(`Ethnic group`=="White English Welsh Scottish Northern Irish or British") %>% 
  rename("maj1"="IMD1",
         "maj10"="IMD10") %>% 
  dplyr::select(c(maj1,maj10,year,Emission_source))

ethnicinequality <- inner_join(compiled_years,majority,
                               by=c("year","Emission_source")) %>% 
  mutate(delta1=(IMD1-maj1),
         delta10=(IMD10-maj10),
         deltper1=((IMD1/maj1)-1)*100,
         deltper10=((IMD10/maj10)-1)*100) %>% 
  
  pivot_longer(cols=c(delta1,delta10,deltper1,deltper10),names_to = "measure",values_to = "delta"
               )

out <- ggplot(data=ethnicinequality %>% dplyr::filter(`Ethnic group`%in%c("Minoritised white",
                                                                        "Asian Asian British or Asian Welsh",
                                                                        "Black Black British Black Welsh Caribbean or African",
                                                                        "White English Welsh Scottish Northern Irish or British",
                                                                        "Mixed or Multiple ethnic groups",
                                                                        "Other ethnic group"
))%>% group_by(`Ethnic group`))  +
  aes(x=year,
      y=delta,
      shape=measure,
      colour=`Ethnic group`)+
  geom_point()+
  geom_line()+
  facet_wrap(~Emission_source,
             scale="free_y")


combo_string <- c("delta1","delta10","deltper1","deltper10")
for(index in c(1:4)){
  targIMD <- combo_string[index]
out2 <- ggplot(data=ethnicinequality %>% dplyr::filter(`Ethnic group`%in%c("Minoritised white",
                                                                          "Asian Asian British or Asian Welsh",
                                                                          "Black Black British Black Welsh Caribbean or African",
                                                                          "White English Welsh Scottish Northern Irish or British",
                                                                          "Mixed or Multiple ethnic groups",
                                                                          "Other ethnic group"
),
measure==targIMD)%>% group_by(`Ethnic group`))  +
  aes(x=year,
      y=delta,
      shape=measure,
      colour=`Ethnic group`)+
  geom_point()+
  geom_line()+
  facet_wrap(~Emission_source,
             scale="free_y")

out2

process_graph_saver(plot=out2,
                    filename = paste0("Outputs/",
                                      pollutant,
                                      " abs inequality",targIMD,"over time_setFgeoms.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.6)
}
