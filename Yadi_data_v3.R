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
properties <- properties %>% dplyr::rename(
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

train <- train %>% dplyr::rename(id_parcel = parcelid)


############ this is the raw training data without preprocessing ###############
train_data <- train %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate) %>%
  inner_join(properties, by="id_parcel") 


############ droping column and imputation ########################

################# code for inspecting NAs ############################# 
# unique(train_data$censustractandblock)
# range(train_data$censustractandblock,na.rm = T)
# str(train_data)
# 
# View(train_data %>% select(area_live_finished, area_total_calc))
# 
# View(na.omit(train_data %>% select(region_county, num_story)) %>% 
#   group_by(region_county, num_story) %>% 
#   dplyr::summarise(count = n()) %>% 
#   arrange(desc(count)))



############ Imputation ########################
## addNA for categorical features with NA
col_addna = c(  
  "aircon", 
  "flag_tub", "heating", "num_pool",
  "pooltypeid7"
  
)

train_data <- train_data %>% 
  mutate_at(.funs = addNA, 
            .vars = intersect(names(train_data), col_addna))

## NA -> 0 for numeric features with NA
impute0 = c(   
  "quality", 
  "num_garage", # missing value form one county (3101)
  "area_total_calc", 
  "area_lot",
  "num_75_bath",
  "num_unit", # missing value from two county (1286, 2061)
  "build_year",
  "num_story", # missing value from one county (3101)
  "tax_total"
)

train_data <- train_data %>% 
  mutate_at(.funs = function(x) ifelse(is.na(x), 0, x),
            .vars = intersect(names(train_data), impute0))

## NA -> 0, otherwise -> 1 for numeric features with high % NA
dummyfy = c(
  "area_firstfloor_finished",
  "area_unknown"
  )

train_data <- train_data %>% 
  mutate_at(.funs = function(x) ifelse(is.na(x), 0, 1),
            .vars = intersect(names(train_data), dummyfy))
train_data <- train_data %>% 
  mutate_at(.funs = as.factor,
            .vars = intersect(names(train_data), dummyfy))

## as.factor for zoning_landuse and region_county
donothing = c(  # no missing value
  "logerror",
  "id_parcel",
  "month", 
  "num_bathroom", 
  "num_bedroom",
  "zoning_landuse", # as.factor
  "region_county", # as.factor
  "num_room"
)
train_data[, "zoning_landuse"] = as.factor(train_data[, "zoning_landuse"])
train_data[, "region_county"] = as.factor(train_data[, "region_county"])

## fireplace
special = c("num_fireplace","flag_fireplace",
            "region_zip" # impute from lat. and long.
)
train_data$flag_fireplace=ifelse(train_data$flag_fireplace=='true',
                                 2,
                                 train_data$flag_fireplace)
train_data$flag_fireplace=ifelse(!is.na(train_data$num_fireplace),
                                 1,
                                 train_data$flag_fireplace)
train_data$flag_fireplace=ifelse(is.na(train_data$flag_fireplace),
                                 0,
                                 train_data$flag_fireplace)
train_data$flag_fireplace=as.factor(train_data$flag_fireplace)
unique(train_data$flag_fireplace)

################# zip code ##################
# missing_zip = train_data[is.na(train_data$region_zip),] %>% 
#   select(latitude, longitude) %>% 
#   group_by(latitude, longitude) %>% 
#   dplyr::summarise()
# write.csv(missing_zip, "missing_zip_train.csv", row.names = FALSE)

zip = fread("missing_zip_train.csv")

for (i in 1:nrow(zip)) {
  train_data[train_data$latitude == zip[i, latitude] & 
               train_data$longitude == zip[i, longitude] &
               is.na(train_data$region_zip), "region_zip"] = zip[i, zip]
}




############ droping column ########################
col_droping = c(
  "year",  # zero variance, all 2016
  "architectural_style", # 99.7% missing
  "area_basement", # 99.95% missing
  "framing", # 99.98% missing
  "num_bathroom_calc",  # correlated to num_bathroom
  "deck", # 99.3% missing
  "area_live_finished", # the same as area_total_calc
  "area_liveperi_finished", #99.96% missing
  "area_total_finished", # 96.1% missing
  "area_base", # 99.5% missing
  "fips", # same as county ID
  "num_bath", # correlated to num_bathroom
  "area_garage", # has the same missing part as num_garage, keeping one
  "latitude", # not included for this round
  "longitude", # not included for this round
  "area_pool",  # high % missing, whether missing or not is correlated to num_pool
  "pooltypeid10", # 98.7% missing
  "pooltypeid2", # 98.7% missing
  "zoning_landuse_county", # categorical feature with large levels
  "zoning_property", # categorical feature with large levels
  "rawcensustractandblock", # ID numbers
  "region_city", # related to region_zip
  "region_neighbor", # too much missing, related to region_zip
  "story",  # 99.95% missing
  "material", # 99.7% missing
  "area_patio", # 97.1% missing
  "area_shed", # 99.9% missing
  "tax_building", # related to tax_total
  "tax_year", # zero variance, all 2015
  "tax_land", # related to tax_total
  "tax_property", # related to tax_total
  "tax_delinquency", # 98.0% missing
  "tax_delinquency_year", # same missing rows as tax_delinquency
  "censustractandblock", # too many unique ID numbers
  "num_fireplace"
) 

train_data3 = train_data[,-which(names(train_data) %in% col_droping)]









