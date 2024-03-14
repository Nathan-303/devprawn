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

facet_sources_decile_emissions_ethnicity <- function(prawn_path,pollutant,year){
data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  rename(`Asian, Asian British or Asian Welsh`=
           `Ethnic group: Asian, Asian British or Asian Welsh`,
         `Black, Black British,  Black Welsh, Caribbean or African` =
         `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
         `Mixed or Multiple  ethnic groups`=
         `Ethnic group: Mixed or Multiple ethnic groups`,
         `White: English, Welsh, Scottish, Northern Irish or British`=
           `Ethnic group: White: English, Welsh, Scottish, Northern Irish or British`,
         `Other ethnic group`=
         `Ethnic group: Other ethnic group`
         ) %>% 
  mutate(`Minoritised white`=
                    `Ethnic group: White: Irish`+
                    `Ethnic group: White: Gypsy or Irish Traveller`+
                    `Ethnic group: White: Roma`+
                    `Ethnic group: White: Other White`) %>%
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=c(
      `Asian, Asian British or Asian Welsh`,
      `Black, Black British,  Black Welsh, Caribbean or African`,
      `Mixed or Multiple  ethnic groups`,
      `White: English, Welsh, Scottish, Northern Irish or British`,
      `Other ethnic group`,
      `Minoritised white`),
    names_to = "Ethnic group",
    values_to = "flat population"
  ) %>%
  #convert flat population into percentage
  mutate("Percentage"=`flat population`/`Ethnic group: Total: All usual residents`) %>%
    group_by(`Ethnic group`)

end_of_sources <- which(colnames(data)=="LSOA11CD")-1

source_list <- colnames(data)[c(1:end_of_sources)]

source_list_trimmer <- !(source_list %in% c("Offshore","Tot_area"))

source_list <- source_list[source_list_trimmer]

long_chunk <- data %>% rename("Other transport and  mobile machinery"="Other transport and mobile machinery") %>% 
  mutate(
    "Industry and point sources"=`Energy production`+`Industrial combustion`+`Industrial production`+`Point sources`,
    "Other sources"=Solvents+Natural+Agricultural+`Waste treatment and disposal`) %>%
  dplyr::select(!c(`Energy production`,
             `Industrial combustion`,
             `Industrial production`,
             `Point sources`,
             Solvents,
             Natural,
             Agricultural,
             `Waste treatment and disposal`)) %>% 
  pivot_longer(
    cols=c("Domestic combustion",
                "Other transport and  mobile machinery",
                "Road transport",
                "Total",
                "Other sources",
                "Industry and point sources"
    ),
    names_to = "Emission_source",
    values_to = "emissions")
#Plot a faceted graph

foray <- edata %>%
  mutate(Diversity_quintile = ntile(x=Percentage,
                                    n=10)) %>%
  group_by(`Ethnic group`,Diversity_quintile)


plottable <- foray %>% inner_join(
  x=long_chunk,
  y=foray %>% dplyr::select(`geography code`,`Ethnic group`,`flat population`,Percentage),
  by=c("LSOA21CD"="geography code")
) %>%
  filter(!Emission_source%in%c("Waste treatment and disposal",
                               "Energy production",
                               "Natural",
                               "Solvents",
                               "Agricultural")) %>%
  mutate(polpop=`flat population`*emissions) %>% 
  group_by(`Ethnic group`,Emission_source) %>%
  summarise(totalems=sum(polpop),totalpop=sum(`flat population`)) %>% 
  mutate(scalems=totalems/totalpop)


segment <- plottable %>% filter(`Ethnic group`=="White: English, Welsh, Scottish, Northern Irish or British") %>% 
  rename("majority"="scalems") %>% 
  ungroup() %>% 
  dplyr::select(Emission_source,majority) %>% 
  inner_join(plottable,by="Emission_source") %>% 
  mutate(delta=scalems-majority)

percentagegrabber <- inner_join(segment,
                                segment %>% 
                                  dplyr::filter(Emission_source=="Total") %>% 
                                  rename("wholepie"="delta") %>% 
                                  dplyr::select(`Ethnic group`,wholepie),
                                by="Ethnic group") %>% 
  mutate(percentage=signif(delta/wholepie,4))

csvbit <- percentagegrabber %>% dplyr::filter(Emission_source!="Total") %>% 
  dplyr::select(!c(totalems,totalpop,scalems,delta,wholepie,majority)
  ) %>% 
  pivot_wider(names_from = Emission_source,
              values_from = percentage) %>% 
  dplyr::filter(`Ethnic group`!="White: English, Welsh, Scottish, Northern Irish or British")
  
ggplot(data=percentagegrabber %>% filter(Emission_source!="Total"))+
  geom_bar(aes(colour=Emission_source,
               y=percentage))+
  facet_wrap(~`Ethnic group`)

ggplot(data=percentagegrabber %>% filter(Emission_source!="Total",
                                         `Ethnic group`!="White: English, Welsh, Scottish, Northern Irish or British"))+
  
  geom_point(aes(x=`Ethnic group`,
                 y=percentage,
                 shape=Emission_source))
  

write.csv(x=percentagegrabber,
          file=paste0(pollutant,"source sector inequality contributions.csv"))

write.csv(x=csvbit,
          file=paste0(pollutant,"source sector inequality contributions reworked.csv"))

dataout <- inner_join(plottable,
                      plottable %>% 
                        dplyr::filter(`Ethnic group`=="White: English, Welsh, Scottish, Northern Irish or British") %>% 
                        rename("base"="Mean") %>% 
                        ungroup() %>% 
                        dplyr::select(!`Ethnic group`),
           by="Emission_source")



output <- ggplot(data=plottable
)+

  aes(x=Diversity_quintile,
      y=Mean,
      colour=`Ethnic group`,linetype=linetype)+

  geom_line(stat="summary",
            linewidth=0.75,
            fun=mean,
            na.rm=TRUE
  )+


  # geom_smooth(method="lm",
  #             formula=y~x,
  #             se=FALSE,
  #             linewidth=1,
  #             na.rm = TRUE)+


  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+
  
  scale_y_continuous(expand=expansion(mult=c(0,0.06),add=0), limits = c(0, NA))+

  labs(x=paste0("Decile where 10 contains the most people within the group"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       title=NULL
  )+
  theme(legend.position = "right",
        legend.key.width = unit(0.5,"cm"),
        legend.key.height = unit(1.3,"cm"))+
  scale_linetype_manual(values=c(5,1),)+

  scale_colour_manual(values =c("black","royalblue","deeppink2","olivedrab1","#FB8022FF","deeppink2"))+

  guides(fill = guide_legend(byrow = TRUE,
                             ),
         linetype="none",
         colour=guide_legend(override.aes = 
                               list(linetype=c(1,1,5,1,1,1))))+

  facet_wrap(~fct_reorder(Emission_source,Mean,mean,na.rm=TRUE,.desc=TRUE),
             scale="free_y")+
  theme_classic()

output

}
