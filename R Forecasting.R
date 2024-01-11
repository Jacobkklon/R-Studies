#Setting seed and simulating normal error
set.seed(123)
err = rnorm(1000,0,1)

#Generating 1000 observations from model 1 -> this is an AR(1) model
y = vector(length=1000)
y[1] = 0
for (i in seq(2,1000)){
  y[i] = 0.85*y[i-1]+err[i]
  
  
}

#Simulating 1000 observations from model 2 -> this is an AR(2) model
y2 = vector(length=1000)
y2[1] = 0
y2[2] = 0
for (i in seq(3,1000)){
  
  y2[i] = -0.01 + 0.5*y2[i-1] + 0.2*y2[i-2] + err[i]
  

}

#Plotting the simulated data from both models versus time:
#Please note that if you want to effectively analyze this data, it's better to 
#break it into multiple graphs for limited ranges of x so that trends can be more
#clearly identified
x = seq(1,1000)
plot(x,y,type="l",col="green",lwd=1.5,panel.first=grid(),xlab="Time",
     ylab = "y value",cex.lab=1.25,cex.axis=1.25, ylim = c(-1,1))
lines(x,y2,col="black",lwd=1.5)

#Using first 975 obs in y2 to train a ts model

y2 = ts(y2)
train_y2 = y2[seq(1,975)]
test_y2 = y2[-seq(1,975)]
library(forecast)
fmod = auto.arima(train_y2)

#Reporting the parameters (the model is estimated as AR(2))
params = fmod$coef
param_diff = abs(params - c(0.5,0.2)) #Comparing the parameters by taking absolute difference
#We can see that the estimated parameters are pretty accurate since the differences are small

#Simulating 1000 observations with estimated parameters
set.seed(123)
y2_obs = vector(length=1000) #Pre-allocating

y2_obs[1] = 0
y2_obs[2] = 0 
for (i in seq(3,1000)){
  
  y2_obs[i] = params[1]*y2_obs[i-1] + params[2]*y2_obs[i-2] + err[i]
  
  
}

plot(x,y2,type="l",col="black",lwd=1.5,panel.first=grid(),xlab="Time",
     ylab = "y value",cex.lab=1.25,cex.axis=1.25, ylim = c(-1,1))
lines(x,y2_obs,col="orange",lwd=1.5)
#The estimated model seems to be doing a fairly good job accurately replicating the data obtained from the underlying equation
#This is seen especially clear when splitting data into pieces by doing something like xlim = c(0,200)

#Producing forecasts 25 time units in advance (this was done previously but is shown by itself here)
y2_forecasted = forecast(fmod,h=25)
forecasted_values = y2_forecasted$mean

#Creating two vectors x1 and x2
set.seed(123)
x1 = vector(length=1000)
x2= x1
err_new = rnorm(1000,0,0.01)
for (i in seq(1,1000)){
  
  x1[i] = 0.75*y2[i]+err_new[i]
  x2[i] = -0.06*y2[i] + err_new[i]
  
}

#Training time series models to predict y2, using x1 and x2
indexer = seq(1,975)
x1_train = x1[indexer]
x1_test = x1[-indexer]
x2_train = x2[indexer]
x2_test = x2[-indexer]
fmod_x1 = auto.arima(train_y2,xreg=x1_train)
fmod_x2 = auto.arima(train_y2,xreg=x2_train)

#Using each model to produce forecasts with a horizon of 25 time units

y2_forecasted_x1 = forecast(fmod_x1,h=25,xreg=x1_test)
forecasted_values_x1 = y2_forecasted_x1$mean

y2_forecasted_x2 = forecast(fmod_x2,h=25,xreg=x2_test)
forecasted_values_x2 = y2_forecasted_x2$mean

#Plotting the last 50 obs from y2 and then all forecasts and the naive forecast
naive_forecasted = rep(y2[975],25)

plot(x[951:1000],y2[951:1000],type="l",col="black",lwd=1.5,panel.first=grid(),xlab="Time",
     ylab = "y value",cex.lab=1.25,cex.axis=1.25)
lines(x[976:1000],forecasted_values,col="orange",lwd=1.5) #regular y2 forecast
lines(x[976:1000],forecasted_values_x1,col="red",lwd=1.5) #x1 assisted forecast
lines(x[976:1000],forecasted_values_x2,col="green",lwd=1.5) #x2 assisted forecast
lines(x[976:1000],naive_forecasted,col="purple",lwd=1.5) #naive forecast

#Reporting NRMSE of the four sets of forecasts and commenting on results
NRMSE_reg = 100*(sqrt(mean((forecasted_values - test_y2)^2))/(max(train_y2)-min(train_y2)))
NRMSE_x1 = 100*(sqrt(mean((forecasted_values_x1 - test_y2)^2))/(max(train_y2)-min(train_y2)))
NRMSE_x2 = 100*(sqrt(mean((forecasted_values_x2 - test_y2)^2))/(max(train_y2)-min(train_y2)))
NRMSE_naive = 100*(sqrt(mean((naive_forecasted - test_y2)^2))/(max(train_y2)-min(train_y2)))

NRMSE_reg
NRMSE_x1
NRMSE_x2
NRMSE_naive

cor(x1,y2)
cor(x2,y2)
# From the results, it is quite clear that the AR forecast with the exogeneous variable of x1 performed the best.
# In comparison, using x2 performed pretty well, and the other two forecasts were significantly worse, with naive being
# the absolute worst. From this it is clear that exogenous variables, when they have a clear relation to the output, are a great
# tool for improving the accuracy of your forecast. From the correlations shown, we can see that x1 and x2 are VERY related to
# y2 and that subsequently gives a huge additional piece of information that helps improve the forecast.
