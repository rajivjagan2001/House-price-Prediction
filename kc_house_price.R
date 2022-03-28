                   #KING COUNTRY HOUSE PRICE PREDICTION

#SETTING THE DIRECTORY INTO THE CURRENT WORKING DIRECTORY

#LOADING THE DATA SET

data<-read.csv("kc_house_data.csv")
head(data)
summary(data) #minimum,maximun,mean,median values of all the features
str(data)  #seeing the type of all features

#WE FIND THAT DATE FEATURE IS NOT IN A PROPER FORMAT , SO WE TRANSFORM INTO THE CORRECT FORMET

data$Date<-as.Date(data$date,"%Y%m%dT000000")
head(data)
data<-data[,-2]  #removing the old date column
head(data)

#ANALYSING THE FEATURES

hist(data$grade)  #90 percent of the house have grade above 5
hist(data$waterfront)
sum(data$waterfront==1) #only 163 of the houses have a waterfront
hist(data$condition)
sum(data$condition>=3) #21411 houses have condition rate greater than or equal to 3

hist(data$view)
sum(data$view!=0)  #only 2124 ot of 21613 houses have a view in their house

hist(data$grade)
sum(data$grade>=6)  #21338 houses have a grade greater than or equal to 6

hist(data$floors)
hist(data$bedrooms)
hist(data$bathrooms)

#REMOVING THE OUTLIERS OF ALL THE FEATURES BY USING SCATTER PLOT VISUALIZATION
#-----------------------------------------------------------------------------
library(ggplot2)

#bathrooms
plot(data$bathrooms)
sum(data$bathrooms>=6) #nearly 16 observations are considered as outliers
bad<-data$bathrooms<6
sum(bad==FALSE)
data<-data[bad,]
nrow(data)

#bathrooms

plot(data$bedrooms)
sum(data$bedrooms>7.5)
bad<-data$bedrooms<=7.5
sum(bad==FALSE)
data<-data[bad,]
nrow(data)

#price

plot(data$price)
bad<-data$price<3.5e+06
sum(bad==FALSE)
data<-data[bad,]
nrow(data)

#Floors

plot(data$floors)
sum(data$floors>3)
bad<-data$floors<=3
sum(bad==FALSE)
data<-data[bad,]
nrow(data)

#grade

plot(data$grade)
sum(data$grade>12 | data$grade<4)
bad<-data$grade<=12 & data$grade>=4
sum(bad==FALSE)
data<-data[bad,]
nrow(data)

#Square feet living

plot(data$sqft_living)
sum(data$sqft_living>6000)
bad<-data$sqft_living<=6000
sum(bad==FALSE)
data<-data[bad,]
nrow(data)

#Square feet lot

plot(data$sqft_lot)
sum(data$sqft_lot>750000)
bad<-data$sqft_lot<=750000
sum(bad==FALSE)
data<-data[bad,]
nrow(data)

#Square feet above

plot(data$sqft_above)
sum(data$sqft_above>5500)
sum(data$sqft_above>5200)
bad<-data$sqft_above<5200
sum(bad==FALSE)
data<-data[bad,]
nrow(data)

#Square feet basement

plot(data$sqft_basement)
sum(data$sqft_basement>2100)
sum(data$sqft_basement>2150)
bad<-data$sqft_basement<=2150
sum(bad==FALSE)
data<-data[bad,]
nrow(data)

#Square feet living of neighbourhood of 15 houses

plot(data$sqft_living15)
sum(data$sqft_living15>5000)
sum(data$sqft_living15>4900)
bad<-data$sqft_living15<=4900
sum(bad==FALSE)
data<-data[bad,]
nrow(data)
 
#Square feet lot of neighbourhood of 15 houses

plot(data$sqft_lot15)
sum(data$sqft_lot15>3e+05)
sum(data$sqft_lot15>=2.5e+05)
bad<-data$sqft_lot15<=2.5e+05
sum(bad==FALSE)
data<-data[bad,]
nrow(data)

#NOW WE HAVE A TOTAL OF 21388 OBSERVATIONS THAT IS WE HAVE WOMITED 225 OBSERVATIONS WHICH IS NEARLY 1%

#SPLITING THE DATASET INTO TRAINING AND TESTING DATASETS IN THE FORM OF 80-20 SPLIT THAT IS 80% OF THE DATA FOR TRAINING AND 20% FOR TESTING

set.seed(2)
id<-sample(2,nrow(data),prob=c(0.8,0.2),replace=TRUE)
head(id)
sum(id==1)
sum(id==2)
train<-data[id==1,]
test<-data[id==2,]
dim(train)
dim(test)
summary(train)
summary(test)

#Spliting the independent and dependent variables seperately

train_inputs<-train[,-2]
train_output<-train[,2]
test_inputs<-test[,-2]
test_output<-test[,2]

#visualizing heatmap to see the correlation between the variables

library(ggcorrplot)

date<-train_inputs[,20]
head(date)
train_inputs<-train_inputs[,-20] #date is not a numeric vector 
cor_mat<-cor(train_inputs)
cor_mat
d<-dim(cor_mat)
l1<-d[1]
l2<-d[2]

ggcorrplot(cor_mat)  #heatmap of correlation matrix

#removing the features that are highly correlated that is above the value of 0.85 and less than 1

for(i in 1:l1)
{
for(j in 1:l2)
{
if(abs(cor_mat[i,j])>=0.85 &abs(cor_mat[i,j])<1)
{
print(i)
print("\n")
}
}
}

#From the above we infer that the sqft_living and sqft_above are highly correlated variables but sqft_living is a very important feature so we cannot remove it. we here exclude sqft_above

new_train_inputs<-train_inputs[,-11]
head(new_train_inputs)

#BUILDING MODELS

train_feat<-new_train_inputs
train_feat$price<-train_output
test_date<-test_inputs$Date
test_inputs<-test_inputs[,-20]
test_inputs<-test_inputs[,-11]
head(test_inputs)


#simple linear regression

model1<-lm(price~.,data=train_feat)
summary(model1)
result1<-predict(model1,newdata=test_inputs)
head(result1)
head(test_output)
sse1<-sum((result1-test_output)^2)
sst1<-sum((test_output-mean(test_output))^2)
r_Square1<-1-sse1/sst1
r_Square1  #the r-square value is 0.692


#support vector regression

library(e1071)
model2<-svm(price~.,data=train_feat)
result2<-predict(model2,newdata=test_inputs)
head(test_output)
head(result2)
sse2<-sum((result2-test_output)^2)
sse2
sst2<-sum((test_output-mean(test_output))^2)
sst2
r_Square2<-1-sse2/sst2
r_Square2  #the r-square value is 0.8275

#Random Forest Algorithm for regression

library(randomForest)
rfNews()
model3<-randomForest(price~.,data=train_feat)
result3<-predict(model3,newdata=test_inputs)
head(result3)
head(test_output)
sse3<-sum((result3-test_output)^2)
sst3<-sum((test_output-mean(test_output))^2)
r_Square3<-1-sse3/sst3
r_Square3  #the r-square value is 0.8755


