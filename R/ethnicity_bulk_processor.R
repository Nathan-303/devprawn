#' Run ethnicity graphs and save results
#' @export
#'
#' @examples
#' cartesian_ethnicity_groups_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019)
ethnicity_bulk_processor <- function(){
library(PRAWNS)
library(devprawn)
Startup()
setwd("C:/Users/Nathan Gray/Documents/GitHub/Data-analysis-with-PRAWNS-demo")
prawn_path <-"NOx_emissions_in_2019_v0.13.3/PRAWN.csv"
#prawn_path <-"PM 2.5_emissions_in_2019_v0.14.2/PRAWN.csv"
pollutant <- "NOx"
#pollutant <- "PM2.5"
year <- 2019

graph_placeholder <- cartesian_deprivation_emissions_ethnicity(prawn_path = prawn_path,
                                                   pollutant= pollutant,
                                                   year=year)

process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/",
                                      pollutant," emissions in ",
                                      year,
                                      "weighted by ethnicity.png"),
                                      file_format = "agg_png",
                                      type = 2,
                                      scaling = 0.5
                    )
##This function doesn't have a single output and is misleadingly named
graph_placeholder <- facet_sources_deprivation_emissions_ethnicity(prawn_path = prawn_path,
                                                                        pollutant= pollutant,
                                                                        year=year)
process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/",
                                      pollutant,
                                      " emissions in ",
                                      year,
                                      "faceted by source, coloured by ethnicity, x deprivation.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)

graph_placeholder <- facet_sources_decile_emissions_ethnicity(prawn_path = prawn_path,
                                                         pollutant= pollutant,
                                                         year=year)
process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/",
                                      pollutant,
                                      " emissions in ",
                                      year,
                                      "faceted by source, coloured by ethnicity, x decile.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)


graph_placeholder <- facet_ethnicity_bar_deprivation(prawn_path = prawn_path,
                                                    pollutant= pollutant,
                                                    year=year)
process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/deprivation distribution for ethnicities.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)

graph_placeholder <- facet_RUC_decile_emissions_ethnicity(prawn_path = prawn_path,
                                                     pollutant= pollutant,
                                                     year=year)
process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/",
                                      pollutant,
                                      " emissions in ",
                                      year,
                                      "faceted by RUC, coloured by ethnicity, x decile.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)

graph_placeholder <- facet_RUC_deprivation_emissions_ethnicity(prawn_path = prawn_path,
                                                          pollutant= pollutant,
                                                          year=year)
process_graph_saver(plot=graph_placeholder,
                    filename = paste0("Outputs/Racial inequality/",
                                      pollutant,
                                      " emissions in ",
                                      year,
                                      "faceted by RUC, coloured by ethnicity, x deprivation.png"),
                    file_format = "agg_png",
                    type = 2,
                    scaling = 0.5
)
}
