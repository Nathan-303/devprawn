preprocessing <- function(){
library(PRAWNSdata)
library(tidyterra)
pollutant <- "NOx"


#read the table used for assigning years and shapefiles
setlinks <- read.csv(file="Data/Historic_stats/YearIMDCensus.csv")
#save the rasters in a list, use this as the base
raster_list <- list()
#A vecort with a year from each set used
years <- c(2005,2007,2009,2013,2016,2020)
for(index in c(1:6)){
  year <- years[index]
  chosenset <- setlinks %>% dplyr::filter(Year==year)

  selected_key <- pollutant_key %>% filter(Pollutant==pollutant)

  shapefile_path <-  paste0("Data/Historic_stats/",
                          as.character(chosenset$LSOAs),
                          "_LSOA")

  
  LSOA_shapefile <- vect(shapefile_path)# %>% bind_spat_cols(ListSets[[4]] %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
  #by="LSOA")
  #use the shapefile parameters for 2001
  if(index==1){
    binder <- inner_join(tibble(LSOA=LSOA_shapefile$LSOA01CD),
                         ListSets[[index]] %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
                         by="LSOA"
    )
    
    filtered_shapefile <- LSOA_shapefile %>% dplyr::filter(LSOA01CD%in%binder$LSOA)%>%
      bind_spat_cols(ListSets[[index]] %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
                     by="LSOA")
    
    raster_frame <- rast(filtered_shapefile,
                         ncols=900,
                         nrows=1050)
  }
  #use the shapefile parameters for 2011
  if(index%in%c(2,3,4)){
    binder <- inner_join(tibble(LSOA=LSOA_shapefile$LSOA11CD),
                         ListSets[[index]] %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
                         by="LSOA"
    )
    
    filtered_shapefile <- LSOA_shapefile %>% dplyr::filter(LSOA11CD%in%binder$LSOA)%>%
      bind_spat_cols(ListSets[[index]] %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
                     by="LSOA")
    
    raster_frame <- rast(filtered_shapefile,
                         ncols=900,
                         nrows=1050)
  }
 #use the shapefile parameters for 2021
  if(index%in%c(5,6)){
  binder <- inner_join(tibble(LSOA=LSOA_shapefile$LSOA21CD),
                       ListSets[[index]] %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
                       by="LSOA"
                       )

  filtered_shapefile <- LSOA_shapefile %>% dplyr::filter(LSOA21CD%in%binder$LSOA)%>%
    bind_spat_cols(ListSets[[index]] %>% dplyr::filter(`Ethnic group`=="Total All usual residents"),
                   by="LSOA")

  raster_frame <- rast(filtered_shapefile,
                       ncols=900,
                       nrows=1050)
}
  raster_list[[index]] <- rasterize(x=filtered_shapefile,
                       y=raster_frame,
                       field="IMD") %>% extend(ext(82680, 655660, 5355, 657540))

  terra::set.ext(x=raster_list[[index]],value=c(82680, 655660, 5355, 657540))
  
  crs(raster_list[[index]]) <- "PROJCRS[\"OSGB36 / British National Grid\",\n    BASEGEOGCRS[\"OSGB36\",\n        DATUM[\"Ordnance Survey of Great Britain 1936\",\n            ELLIPSOID[\"Airy 1830\",6377563.396,299.3249646,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4277]],\n    CONVERSION[\"British National Grid\",\n        METHOD[\"Transverse Mercator\",\n            ID[\"EPSG\",9807]],\n        PARAMETER[\"Latitude of natural origin\",49,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",-2,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",0.9996012717,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",400000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",-100000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Engineering survey, topographic mapping.\"],\n        AREA[\"United Kingdom (UK) - offshore to boundary of UKCS within 49°45'N to 61°N and 9°W to 2°E; onshore Great Britain (England, Wales and Scotland). Isle of Man onshore.\"],\n        BBOX[49.75,-9.01,61.01,2.01]],\n    ID[\"EPSG\",27700]]"

  }
}
#plot the changes over time
for(index in c(1:5)){
  placeholder <- raster_list[[index+1]]-raster_list[[index]]
  
  IMDchange <- ggplot()+
    
    geom_spatraster(data=placeholder,
                    aes(fill=IMD))+
    scale_fill_gradient2()
  
  process_graph_saver(plot=IMDchange,
                      filename = paste0("The change in IMD from set ",
                                        letters[index],
                                        " to ",
                                        letters[index+1],
                                        ".png"),file_format = "agg_png",type = 3,scaling = 1)
}

}
