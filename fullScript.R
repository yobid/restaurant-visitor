library(dplyr)
require(lubridate)
library(caret)
library(ModelMetrics)
library(ggplot2)
library(gridExtra)

airReserve <- read.csv(file="air_reserve.csv")
airInfo <- read.csv(file="air_store_info.csv")
airVisit <- read.csv(file="air_visit_data.csv")
date <- read.csv(file="date_info.csv")

#feature engineering
#Date features engineering
airReserve$visit_datetime <- ymd_hms(airReserve$visit_datetime)
airReserve$visit_date <- date(airReserve$visit_datetime)
#Summarise visitors by date
airReserve <- select(airReserve, -(reserve_datetime))
airReserve <- aggregate(airReserve$reserve_visitors, 
                        by=list(airReserve$air_store_id, airReserve$visit_date), 
                        FUN=sum)
names(airReserve) <- c("air_store_id", "visit_date", "reserve_visitors")
#create weekday variable
airReserve$wday <- weekdays(airReserve$visit_date)
#Merge datasets
df <- merge(airReserve, airInfo)
df <- merge(df, airVisit, all.x=TRUE)
#Remove Nas
airTraining <- df[!is.na(df$visitors),] 
#air area name is correlated with longitude and latitude so we will remove it
airTraining <- select(airTraining, -air_area_name)
airTraining <- arrange(airTraining, by=visit_date)

#train&test
set.seed(12345)
inTrain <- createDataPartition(airTraining$visitors, p=0.7, list=FALSE)
airTrain <- airTraining[inTrain, ]
airTest <- airTraining[-inTrain, ]

#fit model
airTrainA <- select(airTrain, -c(air_store_id, wday))
controls <- trainControl(method="repeatedcv", number=10, repeats=3)
airfit <- train(visitors~., data=airTrainA, method="rf", metric="RMSE", 
                trainControl=controls)

#predict and evaluate
airTestA <- select(airTest, -c(air_store_id, visitors, wday))
airPred <- predict(airfit, airTestA)
accu <- rmsle(airTest$visitors,airPred)
print(accu)
