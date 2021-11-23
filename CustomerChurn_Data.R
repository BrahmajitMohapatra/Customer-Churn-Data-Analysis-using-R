cuschurn <- read.csv("E:/Backup.exe/7th sem/R predictive/predict-churn-py-main/predict-churn-py-main/customer_churn_data.csv")

summary(cuschurn)
unique(cuschurn$gender)
unique(cuschurn$PaymentMethod)

mean(cuschurn$MonthlyCharges)
median(cuschurn$MonthlyCharges)
range(cuschurn$MonthlyCharges)

sum(is.na(cuschurn))
cuschurn<-na.omit(cuschurn)

sum(is.na(cuschurn))


#Extracting 'InternetService' column
cuschurn$InternetService -> cusIntService


#Extracting Paper less billing
cuschurn$PaperlessBilling -> cusPLBilling

#Extracting Streaming TV
cuschurn$StreamingTV -> cusSTV

#Extracting 3,6 and 9th coloumns
cuschurn[,c(3,6,9)] -> cusRandCol

#Getting the count of customers whose 'InternetService' is 'DSL'
#for loop
count=0
for(i in 1:nrow(cuschurn)){
  if(cuschurn$InternetService[i]=="DSL"){
    count=count+1
  }
}

count


#Finding the no. of customers whose tenure is exactly '2' months
#while loop
count=0
i=1
while(i<=nrow(cuschurn)){
  if(cuschurn$tenure[i]==2){
    count=count+1
  }
  i=i+1
}

count


#Customer whose Internet Service is "DSL"

library(dplyr)
cuschurn %>% filter(InternetService=="DSL")->cusDSL

#Customer whose contract type is 'Month-to-month'
cuschurn %>% filter(Contract=="Month-to-month")->cusMonth

#Male senior citizens whose Payment method is Electronic check
#AND '&'
cuschurn%>%filter(gender=="Male" & SeniorCitizen==1 & PaymentMethod=="Electronic check")->cusElectronic

#All those customers whose tenure is greater than 70 months or their total charges should be greater than 8000
#OR '|'
cuschurn%>%filter(tenure>70 | TotalCharges>8000)->cusTotalTenure

#Count of different levels from the 'Churn' column
cuschurn%>%count(Churn)



#Data Visualization

#Barplot for the "PhoneService" coulmn

#GrammerOfGraphics ~ ggplot2
library(ggplot2)
ggplot(data=cuschurn,aes(x=PhoneService))+geom_bar()

#Fill the color to 'blue'
ggplot(data=cuschurn,aes(x=PhoneService))+
  geom_bar(fill="blue")

ggplot(data=cuschurn,aes(x=PhoneService))+
  geom_bar(fill="blue",col="red")


#Barplot for the 'InternerService' column
ggplot(data=cuschurn,aes(x=InternetService))+geom_bar()

ggplot(data=cuschurn,aes(x=InternetService,fill=InternetService))+geom_bar()



#Scatter plot between 'TotalCharges' & 'tenure'
ggplot(data=cuschurn,aes(x=tenure,y=TotalCharges))+
  geom_point()


#Assigning the color to 'wheat3'
ggplot(data=cuschurn,aes(x=tenure,y=TotalCharges))+
  geom_point(col="wheat3")

#Mapping 'PaymentMethod' to col aesthetic
ggplot(data=cuschurn,aes(x=tenure,y=TotalCharges,col=PaymentMethod))+
  geom_point()

#Mapping 'gender' to col aesthetic
ggplot(data=cuschurn,aes(x=tenure,y=TotalCharges,col=gender))+
  geom_point()

#Mapping 'SeniorCitizen' to col aesthetic
ggplot(data=cuschurn,aes(x=tenure,y=TotalCharges,col=SeniorCitizen))+
  geom_point()


#geom_boxplot()
#Boxplot between 'tenure' & 'Partner'

ggplot(data=cuschurn,aes(x=Partner,y=tenure))+geom_boxplot()

ggplot(data=cuschurn,aes(x=Partner,y=tenure))+geom_boxplot(fill="violet",col="snow3")


#Predective Analysis

#Build Linear Regression
library(caTools)
sample.split(cuschurn$tenure,SplitRatio=0.70)->split_tag
subset(cuschurn, split_tag==T)->train
subset(cuschurn, split_tag==F)->test

lm(tenure~Contract, data=train)->model1 #dependent~independent

predict(model1,newdata = test)->predicted_value

head(predicted_value)

cbind(Actual=test$tenure, Predicted=predicted_value)->final_data

as.data.frame(final_data)->final_data

head(final_data)

final_data$Actual - final_data$Predicted ->error
head(error)

cbind(final_data,error)->final_data

head(final_data)

rmse<-sqrt(mean(final_data$error)^2)
rmse

#(rmse:Root Mean Sq Error ~ comparative type of error evaluation)

#Multi Linear Regression
library(caTools)
sample.split(cuschurn$tenure,SplitRatio=0.75)->split_model
subset(cuschurn, split_model==T)->train
subset(cuschurn, split_model==F)->test

lm(tenure~Dependents+MultipleLines+OnlineBackup+DeviceProtection, data=train)->modMLinear

predict(modMLinear,newdata=test)->predictedMLinear

head(predictedMLinear)

cbind(Actual=test$tenure,Predicted=predictedMLinear)->final_data

as.data.frame(final_data)->final_data

head(final_data)

final_data$Actual - final_data$Predicted -> error
head(error)

cbind(final_data,error)->final_data

head(final_data)

rmse_ml<-sqrt(mean((final_data$error)^2))

rmse
rmse_ml

#Lower the RMSE value, better the model performance.

#rmse<rmse_ml

#rmse is the best model (Linear Regression)




