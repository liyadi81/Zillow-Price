#############impurt librarys##################

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(Hmisc)
library(VIM)
library(mice)


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
col_droping = c("architectural_style", "area_basement", "framing", "num_bathroom_calc",
                "deck", "area_firstfloor_finished","area_live_finished","area_liveperi_finished",  "area_total_finished",
                "area_unknown", "area_base", "fips", "num_fireplace", "num_bath", "num_garage", "flag_tub",
                "area_pool", "pooltypeid10", "pooltypeid2", "zoning_landuse_county", "zoning_property", "rawcensustractandblock",
                "region_neighbor", "region_zip", "story", "material", "area_patio", "area_shed", "tax_building", 
                "tax_year", "tax_land", "tax_property", "tax_delinquency", "tax_delinquency_year","censustractandblock") ## adding columns to drop here




############ Imputation ########################
train_data$aircon=ifelse(is.na(train_data$aircon),5,train_data$aircon)

vec_quality = sample(train_data$quality[train_data$quality %in% c(1,4,7,10)],sum(is.na(train_data$quality)))
train_data$quality[is.na(train_data$quality)] = vec_quality

set.seed(0)
imputed.1 = impute(train_data$area_total_calc, "mean")
train_data$area_total_calc = imputed.1

set.seed(0)
imputed.2 = impute(train_data$area_garage, "random")
train_data$area_garage = imputed.2

train_data$heating = ifelse(is.na(train_data$heating),13, train_data$heating)

set.seed(0)
imputed.3 = impute(train_data$area_lot, "random")
train_data$area_lot = imputed.3

train_data$num_pool = ifelse(is.na(train_data$num_pool), 0, 1)

train_data$pooltypeid7 = ifelse(is.na(train_data$pooltypeid7), 0, 1)

##leave region_city for now, come back later



train_data$num_75_bath = ifelse(is.na(train_data$num_75_bath), 0, 1)





#######
#flag_fireplace
train_data$flag_fireplace=ifelse(train_data$flag_fireplace=='true',2,train_data$flag_fireplace)
train_data$flag_fireplace=ifelse(!is.na(train_data$num_fireplace),1,train_data$flag_fireplace)
train_data$flag_fireplace=ifelse(is.na(train_data$flag_fireplace),0,train_data$flag_fireplace)
train_data$flag_fireplace=as.factor(train_data$flag_fireplace)

unique(train_data$flag_fireplace)

#drop number of fireplace
#train_data<-train_data[,-c('num_fireplace')]

#num_story
train_data$num_story=ifelse(is.na(train_data$num_story),0,train_data$num_story)
train_data$num_story=ifelse(train_data$num_story %in% 4:41,'other',train_data$num_story)
unique(train_data$num_story)

train_data$num_story=as.factor(train_data$num_story)

#build_year
set.seed(0)
imputedyear = Hmisc::impute(train_data$build_year, "random")
train_data$build_year=imputedyear

#tax (4 cols)
set.seed(1)
imputedtaxt=Hmisc::impute(train_data$tax_total, mean)
train_data$tax_total=imputedtaxt





vec_unit = sample(train_data$num_unit[train_data$num_unit %in% c(1,2,3,4)],sum(is.na(train_data$num_unit)))
train_data$num_unit[is.na(train_data$num_unit)] = vec_unit






############ droping column and imputation ########################
train_data = train_data[,-which(names(train_data) %in% col_droping)]



####Remove region_city for now, come back later
train_data = train_data[,-which(names(train_data) %in% c("region_city", "year"))]

write.csv(train_data, "train_data_v1.csv")





























