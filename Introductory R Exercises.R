#Exercise 1:


set.seed(123) #Setting seed to 123
normsamp = rnorm(10000,0,1) #Random sample from standard normal

#Reporting mean and variance
mean(normsamp)
var(normsamp) 

#Drawing histogram of sample
hist(normsamp,col="yellow",xlab="Normal Samples",cex.lab=1.25,
     cex.axis=1.25,main="Histogram of Normal Samples",prob = T)

#Adding density estimate
lines(density(normsamp),lwd=2,col = "red")

#Adding two random samples and finding correlation
doublesamp = normsamp + runif(10000,1,2)
cor(doublesamp,normsamp)

#Making boxplots to compare, we do this by putting the samples into a dataframe
box_df = data.frame(doublesamp,normsamp)
boxplot(box_df,xlab="Random Samples")
#normsamp is the normal sample, doublesamp is the normal sample + uniform sample

#Exercise 2:

#Creating vector
xx = seq(0,1,.01)

#Set seed and creating random sample added to xx as vector yy
set.seed(123)
yy = xx + rnorm(length(xx),0,.2)

#Plotting xx vs yy and adding additional lines on the graph 
plot(xx,yy,pch=20,xlab="Input",ylab="Output",cex.lab=1.5,
     cex.axis=1.5,main="xx variation graphs")
lines(xx,xx,col="red",lwd=2)
lines(xx,1.25*xx^2,col='blue',lwd=2)

#Making a legend to add to the graph, note that since the data doesn't have an
#associated line, it was just given no line in the legend
par(cex=.5)
legend("topleft",legend=c("Data","Linear","Nonlinear"),col=
         c("black","red","blue"),lty=c(0,1,1),pch=c(20,20,20),cex=1.25)

#Exercise 3:

#Function definition:
outlier_finder = function(data){
  #Finding minimum and maximum in data (note that I understand that this can
  #be done with a loop but minimum and maximum seems much easier when outlier
  #is defined this way)
  mini = min(data)
  maxi = max(data)
  #Finding median
  median = median(data)
  #If statement to identify the outlier by comparing abs values with median
  if(abs(mini-median) > abs(maxi-median)){
    return(mini)
  }
  else if (abs(mini-median) < abs(maxi-median)){
    return(maxi)
  }
  else{
    return(c(mini,maxi)) #In this case, we have 2 outliers 
  }

  
}

#Testing outlier_finder function
our_outlier = outlier_finder(c(0.1,0.5,0.7,0.8,1.2,1.35,5.0,10.0))
our_outlier

#Exercise 4:

#Creating evenly spaced vectors with 10 elements between 0 and 1
x1 = seq(0,1,length.out=10)
x2 = seq(0,1,length.out=10)

#Finding all possible combinations of vectors with expand.grid()
expand.grid(x = x1,y = x2)

#Interactive 3D plot of x1,x2,and z via plot_ly, must make df of inputs first ERROR
library(plotly)
z = x1^2 + 0.1*x1 #Making z
xyz = data.frame(x1,x2,z)
colnames(xyz) = c("x","y","z") #setting column names
plot_ly(xyz,x =~x,y=~y,z=~z, type="scatter3d",mode="markers")

#Summing elements in x1 for which z>=1.00
x1sum = 0 #Defining the variable outside the loop
for (i in 1:length(z)){
  if (z[i]>=1){
    x1sum = x1sum + x1[i]
  }
}

#Creating factor variable based on z condition, using a similar loop 
zz.bin = vector(length=length(z)) #Creating zz.bin as a vector with length of z
for (i in 1:length(z)){
  if (z[i]>=0.50){
    zz.bin[i] = 0
  }
  else{
    zz.bin[i] = 1
  }
}
zz.bin = factor(zz.bin)
