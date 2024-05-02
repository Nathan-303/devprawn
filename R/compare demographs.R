#read in the normal data
for(year in c(2012:2020)){
  ##read in the relevant csv
  data <- read.csv(file = paste0("Data/Historic_PRAWNS/",
                                 year,
                                 "_",
                                 pollutant,
                                 "_PRAWN.csv"),
                   check.names = FALSE
  )
  data <- data[-1]
  #read in the relevant matching set
  if (year==2005){
    data <- inner_join(data,
                       ListSets[[1]],
                       by="LSOA")
  }
  
  if (year%in%c(2006:2008)){
    data <- inner_join(data,
                       ListSets[[2]],
                       by="LSOA")
  }
  
  if (year%in%c(2009:2012)){
    data <- inner_join(data,
                       ListSets[[3]],
                       by="LSOA")
  }
  
  if (year%in%c(2013:2015)){
    data <- inner_join(data,
                       ListSets[[4]],
                       by="LSOA")
  }
  if (year==2016){
    data <- inner_join(data,
                       ListSets[[5]],
                       by="LSOA")
  }
  
  if (year%in%c(2017:2021)){
    data <- inner_join(data,
                       ListSets[[6]],
                       by="LSOA")
  }
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
           year=year) %>% 
    mutate(method="Everything")
  
  
  if(exists("compiled_years")==FALSE){
    compiled_years <- delta
  }else{
    compiled_years <- rbind(compiled_years,delta)
  }
  #close year loop
}
compiled_years <- compiled_years %>% tibble
#read in the data using only the most up to date demography
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
           year=year) %>% 
    mutate(method="SetE")
  
    compiled_years <- rbind(compiled_years,delta)
  #close year loop
}
compiled_years <- compiled_years %>% tibble

out <- ggplot(data=compiled_years %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
       aes(x=year,
           y=deltabsolute,
           shape=method,
           colour=method))+
  geom_point()+
  facet_wrap(~Emission_source,scale="free_y")

process_graph_saver(plot=out,
                    filename = paste0("Outputs/",
                                      pollutant,
                                      " all pop absolute inequality between 1 and 10 over time.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.6)


out <- ggplot(data=compiled_years %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
              aes(x=year,
                  y=deltpercentage,
                  shape=method,
                  colour=method))+
  geom_point()+
  facet_wrap(~Emission_source,scale="free_y")

process_graph_saver(plot=out,
                    filename = paste0("Outputs/",
                                      pollutant,
                                      " all pop percentage inequality between 1 and 10 over time.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.6)
