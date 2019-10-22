Data_Train <- read_excel("C:/Users/user/Desktop/r/Data_Train.xlsx")
str(Data_Train)
Data_Train$Stops=ifelse(Data_Train$Total_Stops=="non-stop",0,ifelse(Data_Train$Total_Stops=="2 stops",2,ifelse(Data_Train$Total_Stops=="1 stop",1,ifelse(Data_Train$Total_Stops=="3 stops",3,ifelse(Data_Train$Total_Stops=="4 stops",4," ")))))
flight=Data_Train
flight$Date_of_Journey=as.Date(flight$Date_of_Journey)
flight$ar_time=unlist(sapply(strsplit(flight$Arrival_Time," "), function(x) { return(x[1]) }))
flight=na.omit(flight)
library(splitstackshape)
d=cSplit(flight, "Route", "→" )


?strptime
d$new_air=as.numeric(factor(d$Airline))
d$new_source=as.numeric(factor(d$Source))
d$new_destination=as.numeric(factor(d$Destination))
d$info=as.numeric(factor(d$Additional_Info))
d$Route_1=as.numeric(factor(d$Route_1))
d$Route_2=as.numeric(factor(d$Route_2))
d$Route_3=as.numeric(factor(d$Route_3))
d$Route_4=as.numeric(factor(d$Route_4))
d$Route_5=as.numeric(factor(d$Route_5))
d$Route_6=as.numeric(factor(d$Route_6))
d$Stops=as.numeric(d$Stops)
str(d)
demo=d[,-c(1,2,3,4,5,6,8,9,18)]
demo=cSplit(demo,"Duration"," ")
demo$Duration_1= as.numeric(gsub("[^[:digit:]]"," ",demo$Duration_1))
demo$Duration_2=as.numeric( gsub("[^[:digit:]]"," ",demo$Duration_2))
demo[is.na(demo)]=0
demo$new_duration=(demo$Duration_1*60)+demo$Duration_2
demo=demo[,-c(3,11,10,13,14)]
str(demo)
library(data.table)
library(chron)
#demo$Duration_minuate=as.numeric(demo$Duration)
#demo$Duration_minuate=as.numeric(lubridate::period(demo$Duration), "minutes")

sapply(demo,function(x) length(unique(x[!is.na(x)])))

a <- sample(nrow(demo),nrow(demo)*0.7)
train <- demo[a,]
test <- demo[-a,]
model1=lm(Price~.,data=demo)
summary(model1)
library(randomForest)
#model2=glm(Price ~ Stops+Route_1+Route_2+Route_3+Route_4+Route_5+new_air+new_source+new_destination+info+ ,data = demo)
mod2=randomForest(Price~ Stops+Route_1+Route_2+Route_3+Route_4+Route_5+new_air+info+new_duration,data = demo,ntree=300,mtry=9)
lm_prediction <- predict(mod2,type="response", test)
summary(model2)
pred <- predict(mod2,test)
SSE <- sum((test$Price - lm_prediction) ^ 2)
SST <- sum((train$Price - mean(train$Price)) ^ 2)
rsquare <-( 1 - (SSE/SST))
print(rsquare)



