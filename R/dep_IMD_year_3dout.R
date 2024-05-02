
#run the data processing if it hasn't already
if(exists(paste0("An expected file from the running of the preprocessing"))==FALSE){
preprocessing(pollutant)
}

for(year in c(2012:2020)){
##read in the relevant csv
  data <- read.csv(filename <- paste0("Data/Historic_PRAWNS/",
                              year,
                              "_",
                              pollutant,
                              "_PRAWN.csv")
           )
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

#calculate the weighted emissions for each group
intermediate <- data %>% 
  mutate(`Weighted emissions`= Total*flat_population,
         `Weighted deprivation`=IMD*flat_population)

weighted_data <- intermediate %>%
  
  group_by(`Ethnic group`,broad_group,IMD) %>%
  
  summarise(popsum=sum(flat_population),
            emissions_sum=sum(`Weighted emissions`),
            IMD_sum=sum(`Weighted deprivation`)) %>%
 
  
  mutate(`Weighted emissions`=emissions_sum/popsum,
         `Weighted deprivation`=IMD_sum/popsum) %>%
  #add a column to track the years on compilation
  mutate(year=year) %>% 
  #drop the calc columns
  dplyr::select(-c(popsum,emissions_sum,IMD_sum))

keys <- unique(weighted_data$broad_group)

for(index in 1:length(keys)){
  chunk <- weighted_data %>%
    filter(broad_group==keys[index]) %>%
    ungroup() %>%
    mutate(subgroup=row_number())
  
  if(index==1){
    indexed_data <- chunk
  }else{
    indexed_data <- rbind(indexed_data,chunk)
  }
}

indexed_data <- indexed_data %>% mutate(
  point_shape=case_when(
    subgroup==1~1,
    subgroup==2~16,
    subgroup==3~17,
    subgroup==4~18,
    subgroup==5~4,
    subgroup==6~15,
  ),
  point_size=case_when(
    subgroup==1~2,
    subgroup!=1~1
  )
)
  
if(exists("compiled_years")==FALSE){
  compiled_years <- indexed_data
}else{
  compiled_years <- rbind(compiled_years,indexed_data)
}
#close year loop
}
# 
# out <- compiled_years  %>% dplyr::filter(year<=2015)%>% group_by(IMD,`Ethnic group`) %>% 
#   
# do(p=plot_ly(.,
#              y=~IMD,
#              z=~`Weighted emissions`,
#              x=~year,
#              type = 'scatter3d', mode = 'lines',
#              line = list(shape = 'linear', color = 'rgb(205, 12, 24)', width= 4))) %>%
#   subplot(nrows = 1, shareX = TRUE, shareY = TRUE)
# 
# 
# out

fig <- plot_ly(compiled_years %>% dplyr::filter(`Ethnic group`%in%c("Minoritised white",
                                                                    "Asian Asian British or Asian Welsh",
                                                                    "Black Black British Black Welsh Caribbean or African",
                                                                    "White English Welsh Scottish Northern Irish or British",
                                                                    "Mixed or Multiple ethnic groups",
                                                                    "Other ethnic group"
                                                                    )) %>% group_by(IMD,`Ethnic group`),
               y=~IMD,
               z=~`Weighted emissions`,
               x=~year,
               color=~`Ethnic group`,
               type="scatter3d",
               mode="markers",
               width=1920,
               height=1080#,
               #mode="lines"
               )%>%
  add_trace(y=~IMD,
            z=~`Weighted emissions`,
            x=~year,
            color=~`Ethnic group`,
            type = 'scatter3d', mode = 'lines',
            line = list(shape = 'linear', width= 4)) %>% 
  
  layout(legend=list(
    orientation="h",
    xanchor="center"
  ),
  scene=(list(
    aspectmode="manual",
    aspectratio=list(
    x=1,
    y=5,
    z=2
  ))),
  align="center")
fig$sizingPolicy$padding <- 0
fig
#   facet_wrap(~year)
output
compiled_years %>% dplyr::select()
#identify the years needing renaming
#two carribean spellings
#other ethnic group and any other ethnic grooup