#Reading in data and setting seed
da = read.csv("Energydata2_withposition.csv") #Reading in data
SEN = vector(length = length(da$Scaled_Energy))
da = data.frame(da, SEN) #Adding SEN to the dataframe ahead of time
set.seed(123)

#Identifying indexes for each train, validation, test -> note that we shuffle them
train.id = sample(x=1:length(da$Scaled_Energy),size=0.6*length(da$Scaled_Energy))
rem.id = seq(1,length(da$Scaled_Energy),1)[-train.id] #Finding remaining indices
val.id = sample(rem.id,size=0.20*dim(da)[1]) #sampling validation
test.id = seq(1,length(da$Scaled_Energy),1)[-c(train.id,val.id)]

#Before splitting up data, we'll calculate SENs 
library(rdist)
mat1 = as.matrix(da$Position)
mat2 = mat1
distances = cdist(mat1,mat2) #Element 1,2 refers to distance between position 1 and 2
index = vector(length=20)
for (i in 1:length(da$Scaled_Energy)){
  
  dists = data.frame(distances[i,],seq(1,920))
  colnames(dists) = c("X","Index")
  for (j in 1:20){
    mini = min(dists$X)
    for (k in 1:length(da$Defect)){
      if (dists$X[k] == mini){
        index[j] = dists$Index[k]
        dists$X[k] = 1000
        
      }
      
      
    }
    
    
  }
  
  
  closest_20_energies = da$Scaled_Energy[index]
  
  da$SEN[i] = sum(closest_20_energies)/20

  
}
  




#Splitting into identifiable data for train, val, test
train.data = da[train.id,]
val.data = da[val.id,]
test.data = da[test.id,]

#######################################
#Training models:
#Model 1: Logit regression using only atomic energy
library(stats)
logitmod = glm(Defect~Scaled_Energy,family="binomial",data=train.data)
logitmod$coefficients #beta_0 , beta_1

#Model 2: Logit regression using atomic energy and positions
logitmod2 = glm(Defect~Scaled_Energy + Position,family="binomial",data=train.data)

#Model 3: SVM using atomic energy and positions (RBF kernel)
library(e1071)
Y = as.factor(da$Defect)
df = data.frame(da$Scaled_Energy,da$Position,da$SEN,Y)

kernel_grid = seq(from=0.1,to=5,by=.01)
acc.vec = vector(length = length(kernel_grid))
for(i in 1:length(acc.vec)){
  smod = svm(Y~da.Scaled_Energy + da.Position,data=df[train.id,],kernel="radial",gamma=kernel_grid[i])
  pred.tr = predict(smod,df[train.id,])
  acc.vec[i] = 100*length(which(pred.tr == df[train.id,]$Y))/length(df[train.id,]$Y)
  cat(i,"\n") #print index of the loop
}
plot(kernel_grid,acc.vec,type="l") #Plotting to see if it makes sense
sel.par1 = kernel_grid[which(acc.vec == max(acc.vec))][10] #Selected kernel value

#Model 4: SVM using atomic energy, positions, and spacial energy

kernel_grid = seq(from=0.1,to=5,by=.01)
acc.vec = vector(length = length(kernel_grid))
for(i in 1:length(acc.vec)){
  smod = svm(Y~da.Scaled_Energy + da.Position + da.SEN,data=df[train.id,],kernel="radial",gamma=kernel_grid[i])
  pred.tr = predict(smod,df[train.id,])
  acc.vec[i] = 100*length(which(pred.tr == df[train.id,]$Y))/length(df[train.id,]$Y)
  cat(i,"\n") #print index of the loop
}
plot(kernel_grid,acc.vec,type="l") #Plotting to see if it makes sense
sel.par2 = kernel_grid[which(acc.vec == max(acc.vec))][10] #Selected kernel value

##########################################################
#Validation and testing of Logistic Regression Methods
##########################################################
#Validation for model 1 (validation and plotting):
val.preds = predict(logitmod,val.data,type="response") #probability predictions
thred.vec = seq(0.05,.95,length.out=100)
acc = TPR = FPR = vector(length = length(thred.vec))
for(i in 1:length(thred.vec)){
  v.pred = vector(length = length(val.preds))
  for(j in 1:length(val.preds)){
    if(val.preds[j] >= thred.vec[i]){
      v.pred[j] = 1
    }else{v.pred[j] = 0}
  }
  acc[i] = 100*length(which(v.pred == val.data$Defect))/length(val.data$Defect)
  TPR[i] = length(which(v.pred == 1 & val.data$Defect == 1))/length(which(val.data$Defect == 1)) ##TP/P
  FPR[i] = 1 - ( (length(which(v.pred == 0 & val.data$Defect == 0)))/(length(which(val.data$Defect == 0)))) #1 - TN/N
}
head(acc)
plot(thred.vec,acc,type="b",panel.first=grid(),xlab="Threshold",
     ylab="Validation Accuracy") #This is the required plot for model 1

best.th1 = thred.vec[which(acc == max(acc))] #The best threshold value


##Obtain predictions on the test set for model 1:

logitmod.fin = glm(Defect~Scaled_Energy,family="binomial",data=rbind(train.data,val.data))
test.preds = predict(logitmod.fin,test.data,type="response")
t.pred = vector(length = length(test.preds))
for(jj in 1:length(test.preds)){
  if(test.preds[jj] >= best.th1){
    t.pred[jj] = 1
  }else{t.pred[jj] = 0}
}
head(t.pred)
testacc1 = 100*length(which(t.pred == test.data$Defect))/length(test.data$Defect)

#Validation and plotting for model 2:

val.preds = predict(logitmod2,val.data,type="response") #probability predictions
thred.vec = seq(0.05,.95,length.out=100)
acc = TPR = FPR = vector(length = length(thred.vec))
for(i in 1:length(thred.vec)){
  v.pred = vector(length = length(val.preds))
  for(j in 1:length(val.preds)){
    if(val.preds[j] >= thred.vec[i]){
      v.pred[j] = 1
    }else{v.pred[j] = 0}
  }
  acc[i] = 100*length(which(v.pred == val.data$Defect))/length(val.data$Defect)
  TPR[i] = length(which(v.pred == 1 & val.data$Defect == 1))/length(which(val.data$Defect == 1)) ##TP/P
  FPR[i] = 1 - ( (length(which(v.pred == 0 & val.data$Defect == 0)))/(length(which(val.data$Defect == 0)))) #1 - TN/N
}
head(acc)
plot(thred.vec,acc,type="b",panel.first=grid(),xlab="Threshold",
     ylab="Validation Accuracy") #This is the required plot for model 2

best.th2 = thred.vec[which(acc == max(acc))][2] #The best threshold value

##Obtain predictions on the test set for model 2:

logitmod2.fin = glm(Defect~Scaled_Energy + Position,family="binomial",data=rbind(train.data,val.data))
test.preds = predict(logitmod2.fin,test.data,type="response")
t.pred = vector(length = length(test.preds))
for(jj in 1:length(test.preds)){
  if(test.preds[jj] >= best.th2){
    t.pred[jj] = 1
  }else{t.pred[jj] = 0}
}
head(t.pred)
testacc2 = 100*length(which(t.pred == test.data$Defect))/length(test.data$Defect)

##########################################################
#Validation and testing of SVM Methods
##########################################################
#Validation and plotting for model 3:
val = df[val.id,]
cost_grid = seq(from=0.1,to=5,by=.01)
acc.vec = TPR = TNR = vector(length = length(cost_grid))
for(i in 1:length(acc.vec)){
  smod = svm(Y~da.Scaled_Energy + da.Position,data=df[val.id,],kernel="radial",gamma=sel.par1,cost=cost_grid[i])
  pred.val = predict(smod,val)
  acc.vec[i] = 100*length(which(pred.val == val$Y))/length(val$Y)
  cat(i,"\n") #print index of the loop
}
plot(cost_grid,acc.vec,type="l",panel.first=grid(),xlab="Cost",ylab="Validation Accuracy",lwd=3)
sel.par.cost1 = cost_grid[which(acc.vec == max(acc.vec))][100]

##Obtain predictions on the test set for model 3:
fintr = rbind(df[train.id,],val)
test = df[test.id,]
smod.fin = svm(Y~da.Scaled_Energy + da.Position,data=fintr,kernel="radial",gamma=sel.par1,cost=sel.par.cost1)
pred.ts = predict(smod.fin,test)

acc.fin3 = 100*length(which(pred.ts == test$Y))/length(test$Y)

#Validation and plotting for model 4:

val = df[val.id,]
cost_grid = seq(from=0.1,to=5,by=.01)
acc.vec = TPR = TNR = vector(length = length(cost_grid))
for(i in 1:length(acc.vec)){
  smod = svm(Y~da.Scaled_Energy + da.Position +da.SEN,data=df[val.id,],kernel="radial",gamma=sel.par2,cost=cost_grid[i])
  pred.val = predict(smod,val)
  acc.vec[i] = 100*length(which(pred.val == val$Y))/length(val$Y)
  cat(i,"\n") #print index of the loop
}
plot(cost_grid,acc.vec,type="l",panel.first=grid(),xlab="Cost",ylab="Validation Accuracy",lwd=3)
sel.par.cost2 = cost_grid[which(acc.vec == max(acc.vec))][100]

##Obtain predictions on the test set for model 4:
fintr = rbind(df[train.id,],val)
test = df[test.id,]
smod.fin = svm(Y~da.Scaled_Energy + da.Position + da.SEN,data=fintr,kernel="radial",gamma=sel.par2,cost=sel.par.cost2)
pred.ts = predict(smod.fin,test)

acc.fin4 = 100*length(which(pred.ts == test$Y))/length(test$Y)

############################################################
############################################################
#Comparison of testing accuracies:
testacc1 #Method 1
testacc2 #Method 2
acc.fin3 #Method 3
acc.fin4 #Method 4

#Summary of findings:
#From the code above, it is clear that method 4 has the best accuracy. Though all the methods are good.
#Thus, we'd recommend to use method 4 for the classification in this case, though further evaluation
#is required via things like FPR, TPR, and confusion matrix in order to ensure that it's a good model
