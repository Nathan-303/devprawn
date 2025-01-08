#' Bind ethnicity data to the prawn and create graphs based on it
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The pollutant being investigated, used in graph titles
#'
#' @param year The year being investigated, used in graph titles
#'
#' @keywords faceted, sources
#'
#' @export
#'
#' @examples
#' cartesian_ethnicity_groups_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019)

facet_RUC_deprivation_emissions_ethnicity <- function(prawn_path,pollutant,year){
active_stack <- read.csv(file=prawn_path,
                         check.names = FALSE,
                         row.names = 1)%>%
  mutate("Rural urban classification"=case_when(
    RUC11=="Rural town and fringe in a sparse setting"~"Rural town and fringe",
    RUC11=="Rural village and dispersed in a sparse setting"~ "Rural village and dispersed",
    RUC11=="Urban city and town in a sparse setting"~"Urban city and town",
    .default = RUC11))
  #removed lengthening, it can be put back in from facet sources if you want


edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=-c(date,geography,`geography code`),
    names_to = "Ethnic group",
    values_to = "flat_population"
  ) %>%

  group_by(`Ethnic group`) %>%

  mutate(groupid=cur_group_id()) %>%

  mutate(`Ethnic group`=str_sub(`Ethnic group`,start=14L))%>% mutate(`Ethnic group`=`Ethnic group` %>% str_replace_all(
    c("Asian, Asian British or Asian Welsh: "="",
      "Black, Black British, Black Welsh, Caribbean or African: "="",
      "Mixed or Multiple ethnic groups: "="",
      "Other ethnic group:"="")
  )
  #close mutate
  ) %>%
  mutate(`Ethnic group`=str_trim(`Ethnic group`,"left")) %>%

  mutate(`Ethnic group`=case_when(
    `Ethnic group`=="Black, Black British, Black Welsh, Caribbean or African"~"Black, Black British, Black\nWelsh, Caribbean or African",
    `Ethnic group`=="White: English, Welsh, Scottish, Northern Irish or British"~"White: English, Welsh, Scottish,\nNorthern Irish or British",
    `Ethnic group`=="Asian, Asian British or Asian Welsh"~"Asian, Asian British\nor Asian Welsh",
    `Ethnic group`=="Mixed or Multiple ethnic groups"~"Mixed or Multiple\nethnic groups",
    `Ethnic group`=="Other Mixed or Multiple ethnic groups"~"Other Mixed or\nMultiple ethnic groups",
    !`Ethnic group`%in%c("Black, Black British, Black Welsh, Caribbean or African",
                         "White: English, Welsh, Scottish, Northern Irish or British",
                         "Asian, Asian British or Asian Welsh",
                         "Mixed or Multiple ethnic groups",
                         "Other Mixed or Multiple ethnic groups"
    )~`Ethnic group`
  ))


intermediate <- edata %>%
  mutate(`Ethnic group`=case_when(
    `Ethnic group`=="White: Irish"~"Minoritised white",
    `Ethnic group`=="White: Gypsy or Irish Traveller"~ "Minoritised white",
    `Ethnic group`=="White: Roma"~"Minoritised white",
    `Ethnic group`=="White: Other White"~"Minoritised white",
    .default = `Ethnic group`)) %>%  dplyr::filter(`Ethnic group`=="Minoritised white")

edata2 <- rbind(edata,intermediate) %>% 
  dplyr::filter(`Ethnic group`%in%c("Black, Black British, Black\nWelsh, Caribbean or African",
                                    "White: English, Welsh, Scottish,\nNorthern Irish or British",
                                    "Asian, Asian British\nor Asian Welsh",
                                    "Mixed or Multiple\nethnic groups",
                                    "Other Mixed or\nMultiple ethnic groups",
                                    "Minoritised white"))

#make the statty gra

weightchunk <- inner_join(active_stack,edata2,by=c("LSOA21CD"="geography code")) %>%
  dplyr::select(LSOA11CD,`Ethnic group`,Total,IMD,flat_population,groupid,RUC11,`Rural urban classification`) %>%
  group_by(`Ethnic group`,IMD,`Rural urban classification`) %>%
  mutate(weighted=Total*flat_population)

plottable <- weightchunk %>%
  summarise(emsum=sum(weighted),popsum=sum(flat_population),id=mean(groupid)) %>%
  mutate(avgems=emsum/popsum) %>%
  # dplyr::filter(!`Ethnic group`%in%c("Black, Black British, Black\nWelsh, Caribbean or African",
  #                                   "White: English, Welsh, Scottish,\nNorthern Irish or British",
  #                                   "Asian, Asian British\nor Asian Welsh",
  #                                   "Mixed or Multiple\nethnic groups",
  #                                   "Other Mixed or\nMultiple ethnic groups",
  #                                   "Minoritised white")) %>% 
  mutate(linetype=case_when(
    `Ethnic group`=="Minoritised white"~"dashed",
    .default="straight"
  ))%>% 
  dplyr::filter(`Rural urban classification`!="mismatch")

tableout <- plottable %>% group_by(`Ethnic group`,`Rural urban classification`,IMD) %>%
  dplyr::summarise(Population=sum(popsum))%>%
  dplyr::filter(`Ethnic group`%in%c("Black, Black British, Black\nWelsh, Caribbean or African",
                                    "White: English, Welsh, Scottish,\nNorthern Irish or British",
                                    "Asian, Asian British\nor Asian Welsh",
                                    "Mixed or Multiple\nethnic groups",
                                    "Other Mixed or\nMultiple ethnic groups",
                                    "Minoritised white"))

for(index in c(1:5)){
  target <- unique(tableout$`Rural urban classification`)[index]
  show_things <- ggplot(data=tableout %>% dplyr::filter(`Rural urban classification`==target))+
    aes(x=IMD,y=Population)+
    geom_col()+
    facet_wrap(~`Ethnic group`,scale="free_y")+
    scale_x_continuous(
      breaks=c(1:10),
      expand = expansion(mult=0,add=0),
      minor_breaks = FALSE)+
  
  process_graph_saver(plot=show_things,
                      filename = paste0("Outputs/Racial inequality/",
                                        make.names(target),
                                        "poopulation.png"),
                      file_format = "agg_png",
                      type = 2,
                      scaling = 0.5
  )
}

group_deprivation <- tableout %>% group_by(IMD,`Ethnic group`) %>% 
  dplyr::summarise(popsum=sum(Population)) %>% inner_join(
    tableout %>% group_by(`Ethnic group`) %>% dplyr::summarise(totalpop=sum(Population))) %>% 
  mutate(`Percentage of population in residence` = popsum*100/totalpop)

percentage_deprivation <- ggplot(data=group_deprivation %>% 
                                   dplyr::filter(!`Ethnic group`%in%c("White","Total: All usual residents")))+
  aes(x=IMD,y=`Percentage of population in residence`)+
  geom_col()+
  facet_wrap(~`Ethnic group`)+
  labs(x="IMD decile where 1 is most deprived")+
  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)

process_graph_saver(plot=percentage_deprivation,
                    filename = paste0("Outputs/Racial inequality/",
                                      "percentage deprivation for groups",
                                      "poopulation.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)


weird_stack <- inner_join(active_stack,edata2,by=c("LSOA21CD"="geography code")) %>%
  dplyr::select(LSOA11CD,`Ethnic group`,Total,IMD,flat_population,groupid,RUC11,`Rural urban classification`) %>%
  dplyr::filter(`Ethnic group`%in%c("Black, Black British, Black\nWelsh, Caribbean or African",
                                    "White: English, Welsh, Scottish,\nNorthern Irish or British",
                                    "Asian, Asian British\nor Asian Welsh",
                                    "Mixed or Multiple\nethnic groups",
                                    "Other Mixed or\nMultiple ethnic groups",
                                    "Minoritised white")) %>%
  mutate(pollutiontile=ntile(x=Total,n=10)) %>% 
  group_by(pollutiontile,`Ethnic group`) %>% 
  dplyr::summarise(polmax=max(Total),
                   polmin=min(Total),
                   popsum=sum(flat_population))

weird_stack_2 <- inner_join(weird_stack,weird_stack %>% 
                              group_by(pollutiontile) %>% 
                              dplyr::summarise(Total_pop=sum(popsum))) %>% 
  mutate(percentage=popsum/Total_pop) %>% 
  mutate(`Ethnic group`= fct_reorder(.f=as.factor(`Ethnic group`),
                                     .x=c(1,2,3,4,1.5,6)))
  
         
axticks <- unique(weird_stack_2$polmax %>% signif(2)) %>% as.numeric()

axticks <- paste0(unique(weird_stack_2$polmin %>% signif(2)),
                   "-",
                   unique(weird_stack_2$polmax %>% signif(2))
                   )

axticks[10] <- paste0(max(unique(weird_stack_2$polmin)%>% signif(2)),"+")


weird_stack_graph <- ggplot(data=weird_stack_2)+
  aes(x=pollutiontile,
      y=percentage*100)+
  geom_col(aes(fill=`Ethnic group`)
                   )+
  scale_x_continuous(breaks=c(1:10),
                     labels=axticks,
                     expand=expansion(0,0),
                     minor_breaks = FALSE)+
  scale_y_continuous(expand=expansion(0,0))+
  labs(x=bquote(.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       y="Percentage of population experiencing residential emissions")+
  scale_fill_manual(values=c("black","#FB8022FF","royalblue","deeppink2","olivedrab1","deeppink2"))
         
weird_stack_graph

process_graph_saver(plot=weird_stack_graph,
                    filename = paste0("Outputs/Racial inequality/",
                                      "percentage experiencing emissions",
                                      "poopulation.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)         
         
output <- ggplot(data=plottable)+
  aes(x=IMD,y=avgems,colour=`Ethnic group`,linetype=linetype)+
  geom_line()+
  facet_wrap(~fct_reorder(`Rural urban classification`,avgems,.desc=TRUE),scale="free_y")+
  scale_colour_manual(values=c("black","royalblue","deeppink2","olivedrab1","#FB8022FF","deeppink2"))+
  scale_linetype_manual(values=c(5,1),)+
  
  guides(linetype="none",
         colour=guide_legend(override.aes = 
                               list(linetype=c(1,1,5,1,1,1)))
         )+
  theme_classic()+
  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+
  scale_y_continuous(expand=expansion(mult=c(0,0.06),add=0), limits = c(0, NA))+
  labs(y=bquote("Average"~.(pollutant)~"emissions/ tonnes "~km^"-2"),
            x="IMD decile where 10 is least deprived")

output


}
