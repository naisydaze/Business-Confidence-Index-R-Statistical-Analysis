#Installing prerequisite packages
packages <- c( "caTools", "ggplot2", "data.table", "car", "rpart", "rpart.plot")
install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(install)

library(data.table)
library(caTools)
library(ggplot2)
library(car)
library(rpart)
library(rpart.plot)

#set working directory if necessary
options(scipen=999)
setwd()
bci.after <- fread('BCI_after.csv')
bci.subset <- bci.after

#create nominal BCI
for (i in 1:length(bci.subset$Value)){
  if(bci.subset$Value[i] < 100){
    bci.subset$BCI.category[i]<- "pessimism"
  }
  else {
    bci.subset$BCI.category[i]<-"optimism"
  }
}
#===============================================================================
##----------------------- DATA EXPLORATION -------------------------------------
#===============================================================================

#total_cases
ggplot(bci.after, aes(total_cases,Value))+geom_jitter()
ggplot(bci.after, aes(total_cases,Value))+geom_point(color = 'red')+
  labs( x = "total_cases", y = "Business Confidence Index",
        title ="Relationship Between BCI and Total Cases")

#only plot total cases less than 10000000
ggplot(bci.after[bci.after$total_cases<10000000], aes(total_cases,Value))+geom_point(color = 'red')+
  labs( x = "total_cases", y = "Business Confidence Index",
        title ="Relationship Between BCI and Total Cases",
        caption = "Filtered Data: Total Cases < 10 Million")


#total_deaths
ggplot(bci.after, aes(total_deaths,Value))+geom_point(color = 'deepskyblue4')+
  labs( x = "total_cases", y = "Business Confidence Index",
        title ="Relationship Between BCI and Total Death")


#Stringency Index 
ggplot(bci.subset, aes(BCI.category,stringency_index, color = BCI.category)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=1) + 
  labs( x = "Business Confidence Index",
        title ="Relationship Between BCI and Stringency Index")


#people_fully_vaccinated
ggplot(bci.after, aes(people_fully_vaccinated,Value))+ geom_smooth(method = lm)+
  labs( y = "Business Confidence Index",
        title ="Relationship Between people_fully_vaccinated and BCI")


#inflation rate 
ggplot(data=bci.subset, aes(x=inflation_rate, y=Value)) + geom_point(color="dark red") + 
  labs(title="BCI vs Inflation rate", x="Inflation rate", y="BCI")+ 
  geom_smooth()


#unemployment rate 
ggplot(data=bci.subset, aes(x=unemployment_rate, y=Value)) + 
  geom_point(color="dark blue") + labs(title="BCI vs Unemployment rate", x="Unemployment rate", y="BCI") + 
  geom_smooth()


#long term interest rates 
ggplot(data=bci.subset, aes(x=long_term_interest_rates, y=Value)) + 
  geom_point(color="maroon") + 
  labs(title="BCI vs Long-term interest rates", x="Long-term interest rates", y="BCI") + 
  geom_smooth()


#international liquidity 
require(scales)
ggplot(data=bci.subset, aes(x=International_Liquidity, y=Value)) + 
  geom_point(color="dark green") + 
  labs(title="BCI vs International liquidity", x="International liquidity", y="BCI") + 
  scale_x_continuous(labels = comma)


#relative consumer price indices 
ggplot(data=bci.subset, aes(x=relative_consumer_price_indices, y=Value)) + 
  geom_point(color="dark orange") + 
  labs(title="BCI vs Relative consumer price indices", x="Relative consumer price indices", y="BCI")


#===============================================================================
##-------------- DATA CLEANING PART 1: CREATE CATEGORICAL BCI ------------------
#===============================================================================

#factor BCI
bci.subset$BCI.category<- factor(bci.subset$BCI.category)

#Check baseline of BCI
levels(bci.subset$BCI.category) #baseline is optimism

#Re-level BCI
bci.subset$BCI.category <- relevel(bci.subset$BCI.category,"pessimism")
levels(bci.subset$BCI.category) #baseline has been updated to pessimism

#Remove unnecessary columns 
bci.subset$BCI<- bci.subset$Value
bci.subset2 <- subset(bci.subset,select =  -c(LOCATION:Value))

#create another subset 3 for CART model
bci.subset3 <- bci.subset2

#check categorical Y: BCI
table(bci.subset2$BCI.category)

#check continuous Y: BCI
summary(bci.subset2$BCI)

#===============================================================================
#----------------- DATA CLEANING PART 2: HANDLING OF NA ------------------------
#===============================================================================

#data cleaning to remove the NAs
summary(bci.subset2)
sum(is.na(bci.subset2)) #total 10 NAs

which(is.na(bci.subset2$inflation_rate)) #20  26  54  74 112
bci.subset2[c(20,26,54,74,112)] #Missing inflation_rate for these 5 rows

#replace the NA with the previous month data
bci.subset2$inflation_rate[20] <- bci.subset2$inflation_rate[19]
bci.subset2$inflation_rate[26] <- bci.subset2$inflation_rate[25]
bci.subset2$inflation_rate[54] <- bci.subset2$inflation_rate[53]
bci.subset2$inflation_rate[74] <- bci.subset2$inflation_rate[73]
bci.subset2$inflation_rate[112] <- bci.subset2$inflation_rate[111]

which(is.na(bci.subset2$unemployment_rate)) #row 111 112
bci.subset2$unemployment_rate[111] <- bci.subset2$unemployment_rate[110]
bci.subset2$unemployment_rate[112] <- bci.subset2$unemployment_rate[111]

which(is.na(bci.subset2$International_Liquidity)) #row 148 168
bci.subset2$International_Liquidity[148] <- bci.subset2$International_Liquidity[147]
bci.subset2$International_Liquidity[168] <- bci.subset2$International_Liquidity[167]

sum(is.na(bci.subset2)) #left with 1NA

which(is.na(bci.subset2$people_fully_vaccinated)) #row 19
bci.subset2[19] # country is USA, time is 2021/07
#delete the row 19 with NA
bci.subset2 <- bci.subset2[-c(19),]

summary(bci.subset2)

#===============================================================================
##--------------- LINEAR REGRESSION MODEL --------------------------------------
#===============================================================================

#Perform Linear regression (Full Model with all variables)
linreg1 <- lm(BCI ~ . -BCI.category, data = bci.subset2)
summary(linreg1)

#Adjust the margin and create a matrix of 2 x 2 plots
par(mar=c(2,2,2,1),mfrow = c(2,2))  
plot(linreg1)
par(mfrow = c(1,1))  

#Check Multicollinearity
vif(linreg1) #total_Cases and total_deaths vif > 10 

#Check the RMSE of the linear regression model
RMSE.linreg1 <- sqrt(mean(residuals(linreg1)^2))
RMSE.linreg1

#Check the min and max absolute error 
summary(abs(residuals(linreg1))) 

##---------- LINEAR REGRESSION MODEL: plot the intercept against BCI -----------
#Obtain the Y intercept from the linreg Model
linreg1_summary <- summary(linreg1)
intercept <- linreg1_summary$coefficient[1,1] 
bci.intercept.residuals <- bci.subset2$BCI - intercept
bci.intercept.residuals

#Check the RSME using the Y intercept
RMSE.intercept <- sqrt(mean(bci.intercept.residuals)^2) 
RMSE.intercept  #lower than RMSE of linear regression model

#===============================================================================
##---------------- LOGISTIC REGRESSION MODEL  ----------------------------------
#===============================================================================

#Check baseline level of BCI
levels(bci.subset2$BCI.category)

# Perform Binary Class Logistic Regression (Full Model with all variables)
logreg <- glm(BCI.category ~ . -BCI, family = binomial, data = bci.subset2)
summary(logreg)

#Perform backward elimination
logreg_AIC <- step(logreg)

#Check Multicollinearity
vif(logreg_AIC)

#Check odds ratio and odds ratio interval
OR <- exp(coef(logreg_AIC))
OR
OR.CI <- exp(confint(logreg_AIC))
OR.CI #inflation_rate is not significant

#reperform Binary Class Logistic Regression (with selected variables)
lgreg2 <- glm(BCI.category ~ total_cases+stringency_index
              +people_fully_vaccinated+long_term_interest_rates, 
              family = binomial, data = bci.subset2)

# Check variable importance
summary(lgreg2)

#Check Multicollinearity
vif(lgreg2)

# Check Odds Ratio Confidence Interval
OR.CI <- exp(confint(lgreg2))
OR.CI

#Output the logistic function prob from the final model
prob <- predict(lgreg2, type = 'response')
threshold1 <- 0.5
logreg.predict <- ifelse(prob > threshold1, "optimism","pessimism")
logreg.predict <- relevel(factor(logreg.predict),"pessimism")

#Create Confusion Matrix (Observed V.S Predicted)
CM1.logreg <- table(observed=bci.subset2$BCI.category, predicted=logreg.predict)
CM1.logreg
round(prop.table(CM1.logreg),3)

#Check the accuracy of logreg Model
mean(bci.subset2$BCI.category==logreg.predict)

#Identify TP, FP, FN, TN
TN = CM1.logreg[1,1]
TP = CM1.logreg[2,2]
FP = CM1.logreg[1,2]
FN = CM1.logreg[2,1]

#Create a table for easy comparison: Recall, Precision, Specificity, Accuracy
table.logreg <- data.frame('table' = 1:5)

rownames(table.logreg) <- c('Accuracy','TP Rate','FN Rate', 'TN Rate', 'FP Rate')

table.logreg[1,1] <- round(mean(bci.subset2$BCI.category==logreg.predict),2)
table.logreg[2,1] <- round(TP/(TP+FN),2)
table.logreg[3,1] <- round(FN/(TP+FN),2)
table.logreg[4,1] <- round(TN/(TN+FP),2)
table.logreg[5,1] <- round(FP/(TN+FP),2)
table.logreg

View(table.logreg)

#===============================================================================
##-------------------- CART (CONTINOUS BCI) ------------------------------------
#===============================================================================

set.seed(2004)
summary(bci.subset3)

#CART Model
cart1 <- rpart(BCI ~.-BCI.category, data = bci.subset3, method = 'anova', 
               control = rpart.control(minsplit = 2, cp = 0))

#print out the pruning sequence and 10-fold CV errors, as a table
printcp(cart1)

#display the pruning sequence and 10-fold CV errors, as a chart
plotcp(cart1)

#print the maximal tree onto the console
print(cart1)

##Identify the CV error cap
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CV error cap in maximal tree cart1
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split
cp1.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)
cp1.opt

#prune the tree based on the optimal cp
cart2 <- prune(cart1, cp = cp1.opt)
printcp(cart2, digits = 3)
print(cart2)
rpart.plot(cart2, cex=0.55, nn = T, main = "Optimal Tree in BCI_AfterCovid")

#Check the variable importance
cart2$variable.importance
scaledVarImpt<-round((cart2$variable.importance/sum(cart2$variable.importance))*100)
scaledVarImpt
summary(cart2)
#people_fully_vaccinated,long_term_interest_rates,stringency_index, total_cases,total_deaths

#evaluate the model 
BCI.predict <- predict(cart2, data = bci.subset3)
predict.error <- bci.subset3$BCI - BCI.predict

#check the RMSE of the linear regression model
RMSE.cart <- sqrt(mean(predict.error^2))
RMSE.cart

#Check the min and max absolute error 
summary(abs(predict.error))

#===============================================================================
##--------------- CART (CATEGORICAL BCI) ---------------------------------------
#===============================================================================

summary(bci.subset3)
table(bci.subset3$BCI.category)

#Check the BCI.category baseline
levels(bci.subset3$BCI.category) 

# For randomisation in 10-fold CV
set.seed(2004)

cart_cat_1 <- rpart(BCI.category ~ .-BCI, data = bci.subset3, method = 'class',
                    control = rpart.control(minsplit = 2, cp = 0))

# print out the pruning sequence and 10-fold CV errors, as a table.
printcp(cart_cat_1)

# Display the pruning sequence and 10-fold CV errors, as a chart.
plotcp(cart_cat_1)

# print the maximal tree onto the console.
print(cart_cat_1)

#Identify the CV error cap
CVerror.cap <- cart_cat_1$cptable[which.min(cart_cat_1$cptable[,"xerror"]), "xerror"] + cart_cat_1$cptable[which.min(cart_cat_1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CV error cap in maximal tree cart1.
i <- 1; j<- 4
while (cart_cat_1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split
cp2.opt = ifelse(i > 1, sqrt(cart_cat_1$cptable[i,1] * cart_cat_1$cptable[i-1,1]), 1)

#prune the tree based on the optimal cp
cart_cat_2 <- prune(cart_cat_1, cp = cp2.opt)
printcp(cart_cat_2, digits = 3)
print(cart_cat_2)
rpart.plot(cart_cat_2, cex=0.55, nn = T, main = "Optimal Tree for categorical BCI_AfterCovid")

#check the variable importance
cart_cat_2$variable.importance
scaledVarImpt<-round((cart_cat_2$variable.importance/sum(cart_cat_2$variable.importance))*100)
scaledVarImpt
summary(cart_cat_2)

#evaluate the model 
prob <- predict(cart_cat_2, type = 'class')
CM2.cart <- table(observed = prob, predicted = bci.subset3$BCI.category)
CM2.cart

# Overall Accuracy
mean(prob == bci.subset3$BCI.category)

#Create a table for easy comparison: Recall, Precision, Specificity, Accuracy
TN = CM2.cart[1,1]
TP = CM2.cart[2,2]
FP = CM2.cart[1,2]
FN = CM2.cart[2,1]

table.cart <- data.frame('table' = 1:5)
rownames(table.cart) <- c('Accuracy','TP Rate','FN Rate', 'TN Rate', 'FP Rate')

table.cart[1,1] <- round(mean(bci.subset3$BCI.category==prob),2)
table.cart[2,1] <- round(TP/(TP+FN),2)
table.cart[3,1] <- round(FN/(TP+FN),2)
table.cart[4,1] <- round(TN/(TN+FP),2)
table.cart[5,1] <- round(FP/(TN+FP),2)
table.cart

