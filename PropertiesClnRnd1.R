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

#train <- train %>% rename(
 # id_parcel = parcelid)


############ this is the raw training data without preprocessing ###############
train_data <- train %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate) %>%
  inner_join(properties, by="id_parcel") 


############ droping column and imputation ########################
col_droping = c("architectural_style", "area_basement", "framing", "num_bathroom_calc",
                "deck", "area_firstfloor_finished","area_live_finished","area_liveperi_finished",  "area_total_finished",
                "area_unknown", "area_base", "fips", "num_fireplace", "num_bath", "num_garage", "flag_tub",
                "area_pool", "pooltypeid10", "pooltypeid2", "zoning_landuse_county", "zoning_property", "rawcensustractandblock",
                "region_neighbor", "region_zip", "story", "material", "area_patio", "area_shed", "tax_building", 
                "tax_year", "tax_land", "tax_property", "tax_delinquency", "tax_delinquency_year","censustractandblock") ## adding columns to drop here




############ Imputation ########################
properties$aircon=ifelse(is.na(properties$aircon),5,properties$aircon)

vec_quality = sample(properties$quality[properties$quality %in% c(1,4,7,10)],sum(is.na(properties$quality)))
properties$quality[is.na(properties$quality)] = vec_quality

set.seed(0)
imputed.1 = Hmisc::impute(properties$area_total_calc, mean)
properties$area_total_calc = imputed.1

set.seed(0)
imputed.2 = impute(properties$area_garage, "random")
properties$area_garage = imputed.2

properties$heating = ifelse(is.na(properties$heating),13, properties$heating)

set.seed(0)
imputed.3 = impute(properties$area_lot, "random")
properties$area_lot = imputed.3

properties$num_pool = ifelse(is.na(properties$num_pool), 0, 1)

properties$pooltypeid7 = ifelse(is.na(properties$pooltypeid7), 0, 1)

##leave region_city for now, come back later



properties$num_75_bath = ifelse(is.na(properties$num_75_bath), 0, 1)





#######
#flag_fireplace
properties$flag_fireplace=ifelse(properties$flag_fireplace=='true',2,properties$flag_fireplace)
properties$flag_fireplace=ifelse(!is.na(properties$num_fireplace),1,properties$flag_fireplace)
properties$flag_fireplace=ifelse(is.na(properties$flag_fireplace),0,properties$flag_fireplace)
properties$flag_fireplace=as.factor(properties$flag_fireplace)

unique(properties$flag_fireplace)

#drop number of fireplace
#properties<-properties[,-c('num_fireplace')]

#num_story
properties$num_story=ifelse(is.na(properties$num_story),0,properties$num_story)
properties$num_story=ifelse(properties$num_story %in% 4:41,4,properties$num_story)
unique(properties$num_story)

properties$num_story=as.factor(properties$num_story)

#build_year
set.seed(0)
imputedyear = Hmisc::impute(properties$build_year, "random")
properties$build_year=imputedyear

#tax (4 cols)
set.seed(1)
imputedtaxt=Hmisc::impute(properties$tax_total, mean)
properties$tax_total=imputedtaxt





vec_unit = sample(properties$num_unit[properties$num_unit %in% c(1,2,3,4)],sum(is.na(properties$num_unit)))
properties$num_unit[is.na(properties$num_unit)] = vec_unit


##longitude, latitude
set.seed(0)
imputed.long = Hmisc::impute(properties$longitude, "random")
properties$longitude=imputed.long

set.seed(0)
imputed.lat = Hmisc::impute(properties$latitude, "random")
properties$latitude=imputed.lat

set.seed(0)
imputed.county = Hmisc::impute(properties$region_county, "random")
properties$region_county=imputed.county

set.seed(0)
imputed.zip = Hmisc::impute(properties$region_zip, "random")
properties$region_zip=imputed.zip



vec_bedroom = sample(properties$num_bedroom[properties$num_bedroom %in% c(3,4,2,5)],sum(is.na(properties$num_bedroom)))
properties$num_bedroom[is.na(properties$num_bedroom)] = vec_bedroom


vec_zoning_landuse = sample(properties$zoning_landuse[properties$zoning_landuse %in% c(261,246,266,269)],sum(is.na(properties$zoning_landuse)))
properties$zoning_landuse[is.na(properties$zoning_landuse)] = vec_zoning_landuse

num_bathroom = sample(properties$num_bathroom[properties$num_bathroom%in% c(1.00,2.00,3.00,2.50)],sum(is.na(properties$num_bathroom)))
properties$num_bathroom[is.na(properties$num_bathroom)] = num_bathroom


properties$num_room=ifelse(is.na(properties$num_room),0,properties$num_room)


         ############ droping column and imputation ########################
#properties.cln = properties[,-which(names(properties) %in% col_droping)]

properties.test = properties[,-c("architectural_style", "area_basement", "framing", "num_bathroom_calc",
                               "deck", "area_firstfloor_finished","area_live_finished","area_liveperi_finished",  "area_total_finished",
                               "area_unknown", "area_base", "fips", "num_fireplace", "num_bath", "num_garage", "flag_tub",
                               "area_pool", "pooltypeid10", "pooltypeid2", "zoning_landuse_county", "zoning_property", "rawcensustractandblock",
                               "region_neighbor", "story", "material", "area_patio", "area_shed", "tax_building", 
                               "tax_year", "tax_land", "tax_property", "tax_delinquency", "tax_delinquency_year","censustractandblock","region_city"
  )]


####Remove region_city for now, come back later
#properties.cln = properties[,-which(names(properties) %in% c("region_city", "year"))]






write.csv(properties.test, "test3.csv")
test3 = read.csv("test3.csv")

factor_cols <- c("aircon",
                 "heating",
                 "zoning_landuse",
                 "region_county",
                 "region_zip",
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

saveRDS(test3, file="PropertiesClned.rds")
saveRDS(properties_train_working, file = "TrainClned.rds")

##readRDS()




