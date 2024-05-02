preprocessing <- function(){
library(PRAWNSdata)
library(tidyterra)
pollutant <- "NOx"


#read the table used for assigning years and shapefiles
setlinks <- read.csv(file="Data/Historic_stats/YearIMDCensus.csv")
#loop for the years
for(year in c(2010:2020)){
#select the correct protion of the lookup table
  year <- 2015
  
  chosenset <- setlinks %>% dplyr::filter(Year==year)
  filename <- paste0("Data/Historic_PRAWNS/",
                     year,
                     "_",
                     pollutant,
                     "_PRAWN.csv")
  #
  selected_key <- pollutant_key %>% filter(Pollutant==pollutant)

  shapefile_path <-  paste0("Data/Historic_stats/",
                          as.character(chosenset$LSOAs),
                          "_LSOA")
    # input_prawn <- process_create_set_prawns(
    #   raster_path = ziploc,
    #   pollutant_data_name = selected_key$ID.string,
    #   year = year,
    #   pollutant = pollutant,
    #   output_path = paste0(pollutant," emissions in ",year,"PRAWN.csv"),
    #
    # )
  LSOA_shapefile <- vect(shapefile_path)# %>% bind_spat_cols(ListSets[[4]] %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
                                                                   #by="LSOA")

  binder <- inner_join(tibble(LSOA=LSOA_shapefile$LSOA11CD),
                       ListSets[[4]] %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
                       by="LSOA"
                       )

  filtered_shapefile <- LSOA_shapefile %>% dplyr::filter(LSOA11CD%in%binder$LSOA)%>%
    bind_spat_cols(ListSets[[4]] %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
                   by="LSOA")

  raster_frame <- rast(filtered_shapefile,
                       ncols=900,
                       nrows=1050)

  hmmmmmm <- rasterize(x=filtered_shapefile,
                       y=raster_frame,
                       field="IMD")

  writeRaster(x=hmmmmmm,file="rasterisation_testing.tif",overwrite=TRUE)

  }
}

}
