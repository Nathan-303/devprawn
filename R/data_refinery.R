
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
data <- data %>% 
  mutate(`Weighted emissions`= Total*flat_population,
         `Weighted deprivation`=IMD*flat_population)

weighted_data <- data %>%
  
  group_by(`Ethnic group`,broad_group) %>%
  
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
ggplot(compiled_years)+
  aes(x=`Weighted deprivation`,
      y=`Weighted emissions`)+
  
  geom_point()+
  
  facet_wrap(~year)

output <- ggplot(data=compiled_years)+
  
  #Set the standard variables
  aes(x=`Weighted deprivation`,
      y=`Weighted emissions`,
      fill=`Ethnic group`
  )+
  
  # This one is for setting the legend without breaking the plot
  geom_point(alpha=0,stroke=2
  )+
  
  geom_smooth(data=data,
              aes(x=IMD,
                  y=Total),
              inherit.aes = FALSE,
              method="lm",
              formula=y~x,
              se=FALSE)+
  
  #Plot the data with the aesthetics wanted
  geom_point(aes(x=`Weighted deprivation`,
                 y=`Weighted emissions`,
                 colour=broad_group,
                 shape=as.factor(subgroup),
                 size=as.factor(point_size),
                 stroke=2
  ),
  show.legend = FALSE
  )+
  
  #define the scales used for plotting the data, control point size here
  scale_size_manual("the legend",
                    breaks=c(1,2),
                    values=c(4,6))+
  
  scale_colour_manual("the legend",
                      values=c("black","royalblue","olivedrab1","#FB8022FF","deeppink2","blue")
  )+
  
  scale_shape_manual("the legend",
                     values = c(1,16,17,18,4,15))+
  #Trim the axis as the line makes the scale too big
  coord_cartesian(xlim=c(3,6)
  )+
  
  
  
  #Set the display parameters for the legend
  scale_fill_viridis_d("Ethnic\ngroup",guide=guide_legend(override.aes = list(
    # colour=c(rep("black",6),
    #          rep("royalblue",4),
    #          rep("olivedrab1",5),
    #          rep("#FB8022FF",3),
    #          rep("deeppink2",6)),
    alpha=1,
    shape=indexed_data$point_shape,
    size=2),
    ncol=6))+
  
  # geom_smooth(data=refcalc,
  #             inherit.aes = FALSE,
  #             aes(x=IMD,
  #                 y=emissions),
  #             colour="deeppink2",
  #             show.legend = FALSE,
  #             se = FALSE
  # )+
  
  # geom_smooth(data=refcalc,
  #             inherit.aes=FALSE,
  #             method="lm",
  #             formula=y~x,
  #             aes(x=IMD,
  #                 y=emissions),
  #             colour="deeppink2",
  #             show.legend = FALSE,
  #             se = FALSE
  # )+
  # 
  # geom_point(data=refcalc,
  #             inherit.aes = FALSE,
  #             aes(x=IMD,
  #                 y=emissions),
  #             colour="deeppink2",
  #             show.legend = FALSE
  # )+
  
  theme_classic()+
  labs(y=bquote("Average"~.(pollutant)~"emissions/ tonnes "~km^"-2"))+
  theme(legend.position="bottom")+
  expand_limits(y=0)+
  scale_y_continuous(expand = expansion(mult = c(0, .095)))+
  scale_x_continuous(expand=c(0,0))
# +
#   facet_wrap(~year)
output
compiled_years %>% dplyr::select()
#identify the years needing renaming
#two carribean spellings
#other ethnic group and any other ethnic grooup