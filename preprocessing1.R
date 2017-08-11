

#############impurt librarys##################

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(lattice)
library(survival)
library(Formula)
library(colorspace)
library(grid)
library(VIM)
library(Hmisc) 


############# Importing Data #################
properties <- fread('properties_2016.csv', na.strings = "")
train <- fread('train_2016_v2.csv', na.strings = "")

############# rename columns #################
properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26,
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet,
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt,
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr,
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag,
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

train <- train %>% rename(
  id_parcel = parcelid)


############ this is the raw training data without preprocessing ###############
train_data <- train %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate) %>%
  inner_join(properties, by="id_parcel") 


############ droping column and imputation ########################
col_droping = c() ## adding columns to drop here


###############impute number colnum 46 --colnum 58################


properties<-properties[,-c('censustractandblock','area_patio',
                 'area_shed','tax_delinquency','tax_delinquency_year')]

#flag_fireplace
properties$flag_fireplace=ifelse(properties$flag_fireplace=='true',2,properties$flag_fireplace)
properties$flag_fireplace=ifelse(!is.na(properties$num_fireplace),1,properties$flag_fireplace)
properties$flag_fireplace=ifelse(is.na(properties$flag_fireplace),0,properties$flag_fireplace)

properties$flag_fireplace=as.factor(properties$flag_fireplace)

unique(properties$flag_fireplace)

#drop number of fireplace
properties<-properties[,-c('num_fireplace')]

#num_story
properties$num_story=ifelse(is.na(properties$num_story),0,properties$num_story)
properties$num_story=ifelse(properties$num_story %in% 4:41,'other',properties$num_story)
unique(properties$num_story)

properties$num_story=as.factor(properties$num_story)

#build_year
set.seed(0)
imputedyear = Hmisc::impute(properties$build_year, "random")
properties$build_year=imputedyear

#tax (4 cols)
properties$tax_building=ifelse(is.na(properties$tax_building) & !is.na(properties$tax_land),0,
                            properties$tax_building)
set.seed(1)
imputedtaxt=Hmisc::impute(properties$tax_total, mean)
properties$tax_total=imputedtaxt
set.seed(2)
imputedtl=Hmisc::impute(properties$tax_land,mean)
properties$tax_land=imputedtl


properties$tax_building=ifelse(is.na(properties$tax_building),
                          properties$tax_total-properties$tax_land,properties$tax_building)


set.seed(3)
imputedtp=Hmisc::impute(properties$tax_property,mean)
properties$tax_property=imputedtp

properties$tax_year=ifelse(is.na(properties$tax_year),0,1)
properties$tax_year=as.factor(properties$tax_year)


