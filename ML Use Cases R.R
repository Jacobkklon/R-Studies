#Defining the data, putting into a dataframe:

fur = c(-1.056,1.12,-1.045,.952,.993)
temp = c(-1,-1,.667,.667,-.167)
lay = c(1004,1636,852,1506,1555)

mydata = data.frame(fur,temp,lay) #Row index corresponds to obs num

colnames(mydata) = c("x1","x2","y") #x1 is fur, x2 is temp

#Performing Leave One Out CV via looping through the data
SE = vector(length=dim(mydata)[1])
for (i in seq(1,dim(mydata)[1])){
  test.loo = mydata[i,]
  train.loo = mydata[-i,]
  
  lin_mod.loo = lm(y~x1 + x2, data = train.loo) #Making multiple LR
  loo_prediction = predict(object=lin_mod.loo,test.loo) #prediction
  
  #Calculate square error and put into SE vector
  SE[i] = (loo_prediction - test.loo$y)^2
  
  
}
MSE = mean(SE) #Finding MSE by taking mean of sq error

#Training the model using all of the data (mydata)
full_linmod = lm(y~x1 + x2,data=mydata)
params = full_linmod$coefficients #Estimated values of model parameters\

#Using model to predict y at x1 = x2 = 1
predict_df = data.frame(1,1)
colnames(predict_df) = c("x1","x2")
prediction_of_y = predict(object=full_linmod,predict_df)#Final value of prediction

#Training the new quadratic polynomial regression
newdata = data.frame(fur,temp,temp^2,lay) #No column for beta0, it's inherent to MLR
colnames(newdata) = c("x1","x2","x2_2","y")

quad_linmod = lm(y~x1 + x2 + x2_2,data=newdata)#Training model w/all the data

new_params = quad_linmod$coefficients #Reporting new parameters

#Doing new prediction for x1 = x2 = 1
newpredict_df = data.frame(1,1,1^2)
colnames(newpredict_df) = c("x1","x2","x2_2")
newpred_of_y = predict(object=quad_linmod,newpredict_df) #New prediction of y

#Answering part 5 of the assignment:
  #Leave one out cross validation is a suitable CV approach
  #for this experiment because the given data set is small
  #and easy to loop through (only a few data points).
