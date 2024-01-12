#Importing data and required libraries
da = read.csv("data.csv",header=T)
newda = read.csv("testdata_Ex2.csv")

library(fields)
#Reshuffling the data
set.seed(123)
reshuffle.id = sample(1:dim(da)[1],size=dim(da)[1])
reshuffled.data = da[reshuffle.id,] 

#Doing 9-fold cross validation (k=9, groups of 43) for each of our proposed models
foldnum = 9
fold_size = length(da$X)/9

NRMSE_p = vector(length=foldnum)


for (k in seq(1,foldnum)){
  
  test.id = (k-1)*fold_size + (1:fold_size)
  train.data = reshuffled.data[-test.id,]
  test.data = reshuffled.data[test.id,]
  
  
  
  #Method 2: Parametric MLR approach that is piecewise, separated based on wind speed = 15 w/predictors of temp and speed
  
  train.data1 = train.data[which(train.data$Speed_ms <= 1.5),]
  train.data2 = train.data[which(train.data$Speed_ms > 1.5),]
  test.data1 = test.data[which(test.data$Speed_ms <= 1.5),]
  test.data2 = test.data[which(test.data$Speed_ms > 1.5),]
  
  lmod1 = lm(Power_Watt~Speed_ms + Temperature_C,data=train.data1)
  lmod2 = lm(Power_Watt~Speed_ms + Temperature_C,data=train.data2)
  
  pred1 = predict(lmod1,test.data1)
  pred2 = predict(lmod2,test.data2)
  
  diffs1 = (pred1 - test.data1$Power_Watt)^2
  diffs2 = (pred2 - test.data2$Power_Watt)^2
  diffsum = c(diffs1,diffs2)
  NRMSE_p[k] = 100*(sqrt(mean(diffsum))/(max(train.data$Power_Watt)-min(train.data$Power_Watt)))
}


NRMSE_piecewise = mean(NRMSE_p)

