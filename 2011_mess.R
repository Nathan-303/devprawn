#Load in the name lookup
rowNameKey <- read.csv("Data/messy_2011_colnames.csv")
#read in the raw data
OAdata <- read.csv("Data/messy_2011_census.csv") %>% 
  setNames(c("GeographyCode",rowNameKey$ColumnVariableDescription)) %>% 
  dplyr::select(c(1:20))

lookup <- read.csv("Data/LSOAoa.csv") %>% 
  #shed the excess columns
  dplyr::select(OA11CD,LSOA11CD) %>%
  #shed the dupes arisinbg from postcodes being one of the shed columns
  unique()

#join by OA
LSOA_merge <- inner_join(OAdata,lookup,by=c("GeographyCode"="OA11CD")) %>% 
 
  group_by(LSOA11CD) %>%
  #Make LSOA values by summing the component OAs, new names reflect the syntax of 2021
  summarise(
    `White: English, Welsh, Scottish,Northern Irish or British`= sum(`White: English/Welsh/Scottish/Northern Irish/British`),   
    `White: Irish` = sum(`White: Irish`),                                           
    `White: Gypsy or Irish Traveller`= sum(`White: Gypsy or Irish Traveller`),                      
    `White: Other White`= sum(`White: Other White`),                                     
    `White and Black Caribbean`= sum(`Mixed/multiple ethnic group: White and Black Caribbean`), 
    `White and Black African`= sum(`Mixed/multiple ethnic group: White and Black African`),  
    `White and Asian`= sum(`Mixed/multiple ethnic group: White and Asian`),           
    `Other Mixed`= sum(`Mixed/multiple ethnic group: Other Mixed`),               
    `Indian`= sum(`Asian/Asian British: Indian`),                            
    Pakistani= sum(`Asian/Asian British: Pakistani`),                         
    Bangladeshi= sum(`Asian/Asian British: Bangladeshi`),                       
    Chinese= sum(`Asian/Asian British: Chinese`),                           
    `Other Asian`= sum(`Asian/Asian British: Other Asian`),                       
    African= sum(`Black/African/Caribbean/Black British: African`),       
    Carribean= sum(`Black/African/Caribbean/Black British: Caribbean`),       
    `Other Black`= sum(`Black/African/Caribbean/Black British: Other Black`),     
    Arab= sum(`Other ethnic group: Arab`),                               
    `Any other ethnic group`= sum(`Other ethnic group: Any other ethnic group`)           
  )
    
  