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

facet_RUC_ethnicity_deprivation <- function(prawn_path,pollutant,year){
active_stack <- read.csv(file=prawn_path,
                         check.names = FALSE,
                         row.names = 1)
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


weightchunk <- inner_join(active_stack,edata,by=c("LSOA11CD"="geography code")) %>%
  dplyr::select(LSOA11CD,`Ethnic group`,Total,IMD,flat_population,groupid,RUC11) %>%
  group_by(`Ethnic group`,IMD,RUC11) %>%
  mutate(weighted=Total*flat_population)

plottable <- weightchunk %>%
  summarise(emsum=sum(weighted),popsum=sum(flat_population),id=mean(groupid)) %>%
  mutate(avgems=emsum/popsum)



#facet by RUC not ethnicity
plottable2 <- plottable %>% dplyr::filter(`Ethnic group`%in%c("Black, Black British, Black\nWelsh, Caribbean or African",
                                                              "White: English, Welsh, Scottish,\nNorthern Irish or British",
                                                              "Asian, Asian British\nor Asian Welsh",
                                                              "Mixed or Multiple\nethnic groups",
                                                              "Other Mixed or\nMultiple ethnic groups"))
output <- ggplot(data=plottable2)+
  aes(x=IMD,y=avgems,colour=`Ethnic group`)+
  geom_line()+
  facet_wrap(~fct_reorder(RUC11,avgems,.desc=TRUE),scale="free_y")+
  scale_colour_manual(values=c("black","royalblue","olivedrab1","#FB8022FF","deeppink2"))+
  theme_classic()+
  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+
  labs(labs(y=bquote("Average"~.(pollutant)~"emissions/ tonnes "~km^"-2"),
            x="IMD decile where 10 is least deprived"))

output


}
