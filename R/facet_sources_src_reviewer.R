#' A function for faceting the sources of a pollutant
#'
#' This function takes a prawns CSV and produces a summary of the geographic
#' areas matching an inputted parameter
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used, should
#' be quoted
#'
#' @param pollutant The name of the pollutant that's being examined, this is
#' used in the graph names, should be a string
#'
#' @param year The year of the data, used in graph labelling
#'
#' @param input_prawn Use to directly input a prawns object
#'
#' @param trim If trim is true then the firlds tot_area and offshore are trimmed for reduncancy
#'
#' @keywords faceted, sources
#'
#' @export
#'
#' @examples
#' facet_sources_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019
#'   )

facet_sources_src <- function(prawn_path="blank",pollutant,year,input_prawn,trim=TRUE){
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
                                      "Minoritised white")) %>% 
    group_by(`Ethnic group`)

  
  working <- inner_join(edata2,active_stack,by=c("geography code"="LSOA21CD"))
  
for(index in c(1:length(unique(working$`Ethnic group`)))){
  for(IMdex in c(1:10)){
  chunk <- working %>% dplyr::filter(`Ethnic group`==unique(edata2$`Ethnic group`)[index],
                                     IMD==IMdex)
  
  boxxymini <- tibble(
    q90=wtd.quantile(x=chunk$Total,
                     weights=chunk$flat_population,
                     probs=c(0.90)),
    q10=wtd.quantile(x=chunk$Total,
                     weights=chunk$flat_population,
                     probs=c(0.10)),
    q1=wtd.quantile(x=chunk$Total,
                    weights=chunk$flat_population,
                    probs=c(0.25)),
    q3=wtd.quantile(x=chunk$Total,
                    weights=chunk$flat_population,
                    probs=c(0.75)),
    med=wtd.quantile(x=chunk$Total,
                     weights=chunk$flat_population,
                     probs=c(0.5)),
    mean=wtd.mean(x=chunk$Total,
                  weights=chunk$flat_population),
    ethnic_group = unique(edata2$`Ethnic group`)[index],
    
    IMD=IMdex
  )
if (index==1&IMdex==1){boxxy <- boxxymini}else{boxxy <- rbind(boxxy,boxxymini)}
  }
}
  

  
  boxxy2 <- boxxy %>% group_by(ethnic_group) %>% 
  pivot_longer(cols=c(q90,q10,q1,q3,med),values_to = "emissions")



long_chunk$ethnic_group <- as.factor(long_chunk$ethnic_group)


output <- ggplot(data=long_chunk
                          )+

  aes(x=IMD,
      y=emissions)+

  facet_wrap(~ethnic_group)+

  geom_boxplot(data=boxxy2,
               inherit.aes=FALSE,
               aes(x=IMD,
                   y=emissions,
                   group=IMD),
               coef=10000000000000000000000000000000000000000000000000000000000000)+
  
  geom_point(data=boxxy,
             aes(y=mean))+


  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+

  labs(x=paste0("IMD decile where 10 is least deprived"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
                title=NULL
  )




 output
 
 process_graph_saver(plot=output,
                     filename = paste0("reviewer stat distribution.png"),
                     file_format = "agg_png",
                     type = 2,
                     scaling = 0.5
 )
}
