setwd("~/liyad/Documents/Zillow-Price")
#############impurt librarys##################

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(Hmisc)
library(VIM)
library(mice)
library(caret)


############# Importing Data #################
properties <- fread('properties_2016.csv', na.strings = "")
train <- fread('train_2016_v2.csv', na.strings = "")
train <- train %>% rename(
  id_parcel = parcelid)

train <- train %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate)
train <- train %>%
  filter(logerror %between% c(quantile(train$logerror, .005),
                              quantile(train$logerror, .995)))


properties$latitude = properties$latitude/1e6
properties$longitude = properties$longitude/1e6




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


properties[is.na(properties$aircon),'aircon']=-1
properties[is.na(properties$heating),'heating'] = -1
properties[is.na(properties$zoning_landuse),'zoning_landuse'] = -1
properties[is.na(properties$region_county),'region_county'] = -1
properties[is.na(properties$region_zip),'region_zip'] = -1
properties[is.na(properties$num_75_bath),'num_75_bath'] = -1
properties[is.na(properties$flag_fireplace),'flag_fireplace'] = -1
properties[is.na(properties$num_pool),'num_pool'] = -1
properties[is.na(properties$pooltypeid7),'pooltypeid7'] = -1
properties[is.na(properties$num_story),'num_story'] = -1

properties[is.na(properties)] = -999


saveRDS(properties, file="PropertiesClnedRnd2_full.rds")


properties.test = properties[,-c("architectural_style", "area_basement", "framing", "num_bathroom_calc",
                                 "deck", "area_firstfloor_finished","area_live_finished","area_liveperi_finished",  "area_total_finished",
                                 "area_unknown", "area_base", "fips", "num_fireplace", "num_bath", "num_garage", "flag_tub",
                                 "area_pool", "pooltypeid10", "pooltypeid2", "zoning_landuse_county", "zoning_property", "rawcensustractandblock",
                                 "region_neighbor", "story", "material", "area_patio", "area_shed", "tax_building", 
                                 "tax_year", "tax_land", "tax_property", "tax_delinquency", "tax_delinquency_year","censustractandblock","region_city"
)]

write.csv(properties.test, "test3.csv")
test3 = read.csv("test3.csv")

factor_cols <- c("aircon",
                 "heating",
                 "zoning_landuse",
                 "region_county",
                 # "region_zip",
                 "num_75_bath",
                 "flag_fireplace",
                 "num_pool",
                 "pooltypeid7",
                 "num_story"
)

test3 <- test3 %>% select(-X)
test3 <- test3 %>% 
  mutate_at(.funs = as.factor, 
            .vars = intersect(names(test3), factor_cols))

properties_train_working = merge(test3, train, by="id_parcel",all.y=TRUE)

saveRDS(test3, file="PropertiesClnedRnd2.rds")
saveRDS(properties_train_working, file = "TrainClnedRnd2.rds")











