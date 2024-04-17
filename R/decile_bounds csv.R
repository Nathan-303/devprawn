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
  rename(`Asian, Asian British or\nAsian Welsh`=
           `Ethnic group: Asian, Asian British or Asian Welsh`,
         `Black, Black British, \nBlack Welsh, Caribbean\nor African` =
         `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
         `Mixed or Multiple \nethnic groups`=
         `Ethnic group: Mixed or Multiple ethnic groups`,
         `White`=
           `Ethnic group: White: English, Welsh, Scottish, Northern Irish or British`,
         `Other ethnic\ngroup`=
         `Ethnic group: Other ethnic group`
         ) %>% 
  mutate(`Minoritised white`=
                    `Ethnic group: White: Irish`+
                    `Ethnic group: White: Gypsy or Irish Traveller`+
                    `Ethnic group: White: Roma`+
                    `Ethnic group: White: Other White`
         ) %>%
  dplyr::select(c(
    `Ethnic group: Total: All usual residents`,
    `Asian, Asian British or\nAsian Welsh`,
    `Black, Black British, \nBlack Welsh, Caribbean\nor African`,
    `Mixed or Multiple \nethnic groups`,
    `White`,
    `Other ethnic\ngroup`,
    `Minoritised white`)) %>% 
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=c(
      `Asian, Asian British or\nAsian Welsh`,
      `Black, Black British, \nBlack Welsh, Caribbean\nor African`,
      `Mixed or Multiple \nethnic groups`,
      `White`,
      `Other ethnic\ngroup`,
      `Minoritised white`),
    names_to = "Ethnic group",
    values_to = "flat population"
  ) %>%
  #convert flat population into percentage
  mutate("Percentage"=`flat population`/`Ethnic group: Total: All usual residents`) %>%
    group_by(`Ethnic group`)

output <- edata %>% group_by(`Ethnic group`) %>% 
  summarise(Decile=c(1:10),
            `Lower bound`=quantile(x=Percentage,
                         probs=seq(from=0,
                                   by=0.1,
                                   to=0.9)),
            `Upper bound`=quantile(x=Percentage,
                         probs=seq(from=0.1,
                                   by=0.1,
                                   to=1))
            ) #%>% 
  pivot_wider(id_cols=c(Decile),
              values_from=c(`Lower bound`,`Upper bound`),
              names_from = `Ethnic group`)
  


write.csv(x=output,
          file="Decile_bounds.csv")

}
