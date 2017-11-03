######################### data imputation-Round 1 #########################
###########################################################################

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(Hmisc)
library(VIM)
library(mice)


properties <- fread('properties_2016.csv', na.strings = "")
train <- fread('train_2016_v2.csv', na.strings = "")

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

train_data <- train %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate) %>%
  inner_join(properties, by="id_parcel") 


col_droping = c("architectural_style", "area_basement", "framing", "num_bathroom_calc",
                "deck", "area_firstfloor_finished","area_live_finished","area_liveperi_finished",  "area_total_finished",
                "area_unknown", "area_base", "fips", "num_fireplace", "num_bath", "num_garage", "flag_tub",
                "area_pool", "pooltypeid10", "pooltypeid2", "zoning_landuse_county", "zoning_property", "rawcensustractandblock",
                "region_neighbor", "region_zip", "story", "material", "area_patio", "area_shed", "tax_building", 
                "tax_year", "tax_land", "tax_property", "tax_delinquency", "tax_delinquency_year","censustractandblock") ## adding columns to drop here




train_data$aircon=ifelse(is.na(train_data$aircon),5,train_data$aircon)

vec_quality = sample(train_data$quality[train_data$quality %in% c(1,4,7,10)],sum(is.na(train_data$quality)))
train_data$quality[is.na(train_data$quality)] = vec_quality

set.seed(0)
imputed.1 = Hmisc::impute(train_data$area_total_calc, mean)
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

train_data$num_75_bath = ifelse(is.na(train_data$num_75_bath), 0, 1)

train_data$flag_fireplace=ifelse(train_data$flag_fireplace=='true',2,train_data$flag_fireplace)
train_data$flag_fireplace=ifelse(!is.na(train_data$num_fireplace),1,train_data$flag_fireplace)
train_data$flag_fireplace=ifelse(is.na(train_data$flag_fireplace),0,train_data$flag_fireplace)
train_data$flag_fireplace=as.factor(train_data$flag_fireplace)

unique(train_data$flag_fireplace)

train_data$num_story=ifelse(is.na(train_data$num_story),0,train_data$num_story)
train_data$num_story=ifelse(train_data$num_story %in% 4:41,'other',train_data$num_story)
unique(train_data$num_story)

train_data$num_story=as.factor(train_data$num_story)

set.seed(0)
imputedyear = Hmisc::impute(train_data$build_year, "random")
train_data$build_year=imputedyear

set.seed(1)
imputedtaxt=Hmisc::impute(train_data$tax_total, mean)
train_data$tax_total=imputedtaxt

vec_unit = sample(train_data$num_unit[train_data$num_unit %in% c(1,2,3,4)],sum(is.na(train_data$num_unit)))
train_data$num_unit[is.na(train_data$num_unit)] = vec_unit

train_data = train_data[,-which(names(train_data) %in% col_droping)]

train_data = train_data[,-which(names(train_data) %in% c("region_city", "year"))]

write.csv(train_data, "train_data_v1.csv")

######################### data imputation-Round 2 #########################
###########################################################################
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


########################## machine learning ###############################
###########################################################################

###################### Janet gbm ####################

library(caret)
library(doMC)
library(parallel)
library(data.table)
library(dplyr)


train.scale = as.data.frame(scale(traindata))
train.scale$year=1
train.scale$tax_year=1
View(traindata %>% select(tax_delinquency,year,tax_year))
View(summary(train.scale))
View(sapply(train.scale, sd))

propertyclean<-readRDS('PropertiesClnedRnd2.rds')
propertyclean$censustractandblock=as.numeric(propertyclean$censustractandblock)
propertyclean[is.na(propertyclean)] = -1

trainclean <- fread('train_2016_v2.csv', na.strings = "")
trainclean <- trainclean %>% dplyr::rename(
  id_parcel = parcelid)

trainclean <- trainclean %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate)

trainclean <- trainclean %>%
  filter(logerror %between% c(quantile(trainclean$logerror, .005),
                              quantile(trainclean$logerror, .995)))

traindata <- trainclean %>% 
  inner_join(propertyclean, by="id_parcel") 





sapply(train.scale,function(x) sum(is.na(x)))


traindata$flag_tub=ifelse(traindata$flag_tub=='true',1,-1)
traindata$flag_fireplace=ifelse(traindata$flag_fireplace=='true',1,-1)
traindata$tax_delinquency=ifelse(traindata$tax_delinquency=='Y',1,-999)
traindata$zoning_landuse_county=as.factor(traindata$zoning_landuse_county)
levels(traindata$zoning_landuse_county)=c(-999,1:77)
traindata$zoning_property=as.factor(traindata$zoning_property)
levels(traindata$zoning_property)=c(-999,1:89372)

traindata<-traindata %>%
  mutate_at(.funs=as.numeric,
            .cols=intersect(names(traindata),c(1:61)))

traindata[traindata==-1]=-999



set.seed(100)
trainIndex<-createDataPartition(traindata$logerror,
                                p=.75,
                                list=FALSE,
                                times=1)


subTrain<-traindata[trainIndex,-1]
subTest<-traindata[-trainIndex,-1]

maeSummary<-function(data,lev=NULL,model=NULL){
  mae_score<-sum(abs(data$obs-data$pred))/nrow(data)
  names(mae_score)<-'MAE'
  mae_score
}



fitBest <- trainControl(method = "none",
                        summaryFunction = maeSummary)

gbmGridlogoutlier <- expand.grid(interaction.depth = c(20), 
                       n.trees = c(2000), 
                       shrinkage = c(0.003),
                       n.minobsinnode = c(30))

gbmFitscale1 <- train(logerror ~ .,
                data = subTrain, 
                method = "gbm", 
                preProcess = c("center", "scale"),
                metric = "MAE",
                maximize = FALSE,
                tuneGrid = gbmGridlogoutlier,
                trControl = fitBest,
                verbose = TRUE)


results<-data.frame(obs=subTest$logerror,
                    pred=predict(gbmFitscale1,newdata=subTest))


maeSummary(results)

propertyclean$flag_tub=ifelse(propertyclean$flag_tub=='true',1,-1)
propertyclean$flag_fireplace=ifelse(propertyclean$flag_fireplace=='true',1,-1)
propertyclean$tax_delinquency=ifelse(propertyclean$tax_delinquency=='Y',1,-999)
propertyclean$zoning_landuse_county=as.factor(propertyclean$zoning_landuse_county)
levels(propertyclean$zoning_landuse_county)=c(-999,1:240)
propertyclean$zoning_property=as.factor(propertyclean$zoning_property)
levels(propertyclean$zoning_property)=c(-999,1:5638)

propertyclean<-propertyclean %>%
  mutate_at(.funs=as.numeric,
            .cols=intersect(names(propertyclean),c(1:58)))

propertyclean[propertyclean==-1]=-999


saveRDS(propertyclean, file="numeric-propertyclean.rds")


propertyclean.scale = as.data.frame(scale(propertyclean))
propertyclean.scale$id_parcel=propertyclean$id_parcel
saveRDS(propertyclean.scale,file="scale-numeric-propertyclean.rds")


test2<-readRDS('scale-numeric-propertyclean.rds')
test3<-readRDS('numeric-propertyclean.rds')

#####scale from property to training
propertyscale=test2


trainoriginal <- fread('train_2016_v2.csv', na.strings = "")
trainoriginal <- trainoriginal %>% dplyr::rename(
  id_parcel = parcelid)

trainoriginal <- trainoriginal %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate)

trainoriginal <- trainoriginal %>%
  filter(logerror %between% c(quantile(trainoriginal$logerror, .005),
                              quantile(trainoriginal$logerror, .995)))

trainwithscale <- trainoriginal %>% 
  inner_join(propertyscale, by="id_parcel") 

######model try
set.seed(100)
trainIndex<-createDataPartition(trainwithscale$logerror,
                                p=.75,
                                list=FALSE,
                                times=1)


subTrainscale<-trainwithscale[trainIndex,-1]
subTestscale<-trainwithscale[-trainIndex,-1]

maeSummary<-function(data,lev=NULL,model=NULL){
  mae_score<-sum(abs(data$obs-data$pred))/nrow(data)
  names(mae_score)<-'MAE'
  mae_score
}

rdmSearch<-trainControl(method='cv',
                        number=2,
                        verboseIter=TRUE)


fitBest <- trainControl(method = "none",
                        summaryFunction = maeSummary)

gbmGridscale <- expand.grid(interaction.depth = c(20), 
                                 n.trees = c(1200,1400,1600,1800,2000), 
                                 shrinkage = c(0.003),
                                 n.minobsinnode = c(30))

gbmFitscaleallchoose <- train(logerror ~ .,
                      data = subTrainscale, 
                      method = "gbm", 
                      preProcess = c("center", "scale"),
                      metric = "MAE",
                      maximize = FALSE,
                      tuneGrid = gbmGridscale,
                      trControl = rdmSearch,
                      verbose = TRUE)
saveRDS(gbmFitscaleallchoose, file="gbmFitscaleallchoose.rds")
plot(gbmFitscaleallchoose)
saveRDS(gbmFitscaleall, file="gbmFitscaleall.rds")

varImp(gbmFitscaleall)
results<-data.frame(obs=subTestscale$logerror,
                    pred=predict(gbmFitscaleall,newdata=subTestscale))


gbmGridscale
maeSummary(results)
gbmImp <- varImp(gbmFitscaleall, scale = FALSE)
gbmImp$importance
plot(gbmImp,top=30)


propertyscale$year=2016

makePrediction <- function(model, newdata, months, labels) {
  predictions <- newdata[,"id_parcel", drop=FALSE]
  for(i in 1:length(months)) {
    newdata$month <- months[i]
    predictions[, labels[i]] <- predict(model, newdata = newdata)
    print(i)
  }
  write.csv(x = predictions, file = "submissionthird.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(gbmFitscaleall,newdata=propertyscale,months = c(10, 11, 12, 22), 
               labels = c("201610", "201611", "201612", "201710"))

submissionforth<-fread("submissionthird.csv")
submissionforth[109885,1]='12000000'
submissionforth[412490,1]='14000000'
submissionforth[701406,1]='11000000'
submissionforth[2879372,1]='13000000'
submissionforth$V6=submissionforth$V5
submissionforth$V7=submissionforth$V5
colnames(submissionforth)=c('ParcelId',"201610", "201611", "201612", "201710", "201711", "201712")
submissionforth=submissionforth[-1,]


write.csv(x = submissionforth, file = "submissionforth.csv", 
          quote = FALSE, row.names = FALSE)



















