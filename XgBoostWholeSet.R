library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(Hmisc)
library(VIM)
library(mice)
library(caret)

setwd("/Users/wei/Desktop/MLProject")
prop <- fread("/Users/wei/Desktop/MLProject/properties_2016.csv", stringsAsFactors = FALSE)

train <- fread("/Users/wei/Desktop/MLProject/train_2016_v2.csv", stringsAsFactors = FALSE)

train <- train %>% rename(
  id_parcel = parcelid)
prop$latitude = prop$latitude/1e6
prop$longitude = prop$longitude/1e6


