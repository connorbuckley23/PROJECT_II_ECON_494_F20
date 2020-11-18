install.packages("readxl") #installs package that allows us to import Excel files into RStudio
library("readxl") #loads library that allows us to import Excel files into RStudio

getwd() #file is in the working directory
my_data<-read_excel("Data_for_RStudio_ECON_494.xlsx") #Imports Excel file data into RStudio
View(my_data) #view data in spreadsheet format
dim(my_data) #check the dimensions of the data frame

###############################################################################
###############################################################################

##combine complete data set into subset solely on percentage change in stocks month to month##
a1<-cbind(my_data[,3])
b1<-cbind(a1,my_data[,5])
c1<-cbind(b1,my_data[,7])
d1<-cbind(c1,my_data[,9])
e1<-cbind(d1,my_data[,11])
f1<-cbind(e1,my_data[,13])
g1<-cbind(f1,my_data[,15])
h1<-cbind(g1,my_data[,17])
i1<-cbind(h1,my_data[,19])
j1<-cbind(i1,my_data[,21])
k1<-cbind(j1,my_data[,23])

my_StockChange<-cbind(k1,my_data[,25]) #new data set solely on percentage change in stock price month to month
dim(my_StockChange) #check the dimensions of the data frame
head(my_StockChange) #check the first 6 observations to ensure data is structured correctly
tail(my_StockChange) #check the last 7 observations to ensure data is structured correctly

###############################################################################
###############################################################################

##Load necessary libraries for regression and partitioning of data##
library(ggplot2) #Loads the ggplot2 library 
library(plyr) #for ddply()
library(tseries) #loads library needed to run the J-B test

###############################################################################
###############################################################################

##Build a linear regression model with the strongly correlated stocks##

## FIRST STRONG RELATIONSHIP BETWEEN SP500 AND JPM ##

STRONG1<-lm(`SP500 Month Change` ~ `JPM Month Change`, my_StockChange) #BUILDS MODEL OBJECT USING lm()
summary(STRONG1) #GENERATES SUMMARY


STRONG1$coefficients #RETURNS BETA ESTIMATES
STRONG1$residuals #RETURNS RESIDUALS
STRONG1$fitted.values #RETURNS FITTED VALUES


hist(STRONG1$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(STRONG1$residuals) #UTILIZE J-B TEST FOR NORMLAITY

## UTILIZE GGPLOT2 TO CREATE VISUALIZATION ##
ggplot(my_StockChange, aes(x = `SP500 Month Change`, y = `JPM Month Change`)) + 
  geom_point() +
  geom_smooth(method ='lm')

###############################################################################
##############################################################################

## SECOND STRONG RELATIONSHIP BETWEEN SP500 AND AAPL ##

STRONG2<-lm(`AAPL Month Change` ~ `SP500 Month Change`, my_StockChange) #BUILDS MODEL OBJECT USING lm()
summary(STRONG2) #GENERATES SUMMARY 


STRONG2$coefficients #RETURNS BETA ESTIMATES
STRONG2$residuals #RETURNS RESIDUALS
STRONG2$fitted.values #RETURNS FITTED VALUES


hist(STRONG2$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(STRONG5$residuals) #UTILIZE J-B TEST FOR NORMLAITY 

## UTILIZE GGPLOT2 TO CREATE VISUALIZATION ##
ggplot(my_StockChange, aes(x = `AAPL Month Change`, y = `SP500 Month Change`)) + 
  geom_point() +
  geom_smooth(method ='lm')

###############################################################################
##############################################################################

## THIRD STRONG RELATIONSHIP BETWEEN SP500 AND UNP ##

STRONG3<-lm(`SP500 Month Change` ~ `UNP Month Change`, my_StockChange) #BUILD THE MODEL OBJECT USING lm()
summary(STRONG3) #GENERATES SUMMARY


STRONG3$coefficients #RETURNS BETA ESTIMATES
STRONG3$residuals #RETURNS RESIDUALS
STRONG3$fitted.values #RETURNS FITTED (PREDICTED) VALUES


hist(STRONG3$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(STRONG3$residuals) #UTILIZE J-B TEST FOR NORMLAITY

## UTILIZE GGPLOT2 TO CREATE VISUALIZATION ##
ggplot(my_StockChange, aes(x = `SP500 Month Change`, y = `UNP Month Change`)) + 
  geom_point() +
  geom_smooth(method ='lm')

##############################################################################
##############################################################################

##Build a linear regression model with the weakly correlated stocks##

## FIRST WEAK RELATIONSHIP BETWEEN AMZN AND XOM ##

WEAK1<-lm(`AMZN Month Change` ~ `XOM Month Change`, my_StockChange) #BUILD THE MODEL OBJECT USING lm()
summary(WEAK2) #GENERATES SUMMARY


WEAK1$coefficients #RETURNS BETA ESTIMATES
WEAK1$residuals #RETURNS RESIDUALS
WEAK1$fitted.values #RETURNS FITTED VALUES


hist(WEAK1$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(WEAK1$residuals) #UTILIZE J-B TEST FOR NORMLAITY

## UTILIZE GGPLOT2 TO CREATE VISUALIZATION ##
ggplot(my_StockChange, aes(x = `AMZN Month Change`, y = `XOM Month Change`)) + 
  geom_point() +
  geom_smooth(method ='lm')

###############################################################################
###############################################################################

## SECOND WEAK RELATIONSHIP BETWEEN AAPL AND XOM ##

WEAK2<-lm(`XOM Month Change` ~ `AAPL Month Change`, my_StockChange) #BUILD THE MODEL OBJECT USING lm()
summary(WEAK2) #GENERATES SUMMARY


WEAK2$coefficients #RETURNS BETA ESTIMATES
WEAK2$residuals #RETURNS RESIDUALS
WEAK2$fitted.values #RETURNS FITTED VALUES


hist(WEAK2$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(WEAK2$residuals) #UTILIZE J-B TEST FOR NORMLAITY

## UTILIZE GGPLOT2 TO CREATE VISUALIZATION ##
ggplot(my_StockChange, aes(x = `XOM Month Change`, y = `AAPL Month Change`)) + 
  geom_point() +
  geom_smooth(method ='lm')

###############################################################################
###############################################################################

## LET'S LOOK INTO THE SP500'S MONTH CHANGE EFFECT ON JPM'S MONTH CHANGE ##

my_StockChange$SP500change2<-my_StockChange$`SP500 Month Change`^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
my_StockChange$SP500change3<-my_StockChange$`SP500 Month Change`^3 #CUBIC TRANSFORMATION (3rd ORDER)
my_StockChange$SP500change4<-my_StockChange$`SP500 Month Change`^4 #QUARTIC TRANSFORMATION (4th ORDER)


p<-.7 #fraction of sample to be used for training

obs_count<-dim(my_StockChange)[1] #number of observations (rows) in the dataframe

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size

set.seed(1234)#set the seed to make your partition reproducible

train_ind <- sample(obs_count, size = training_size) #create a vector with the shuffled row numbers of the original dataset

Training <- my_StockChange[train_ind, ] #pulls random rows for training
Testing <- my_StockChange[-train_ind, ] #pulls random rows for testing

#CHECK THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)


#PLOT THE TRAINING AND TESTING PARTITIONS
plot(`JPM Month Change`  ~ `SP500 Month Change`, my_StockChange, xlim=c(-.2,.2), ylim=c(-.2,.2)) #PLOT ENTIRE DATASET
plot(`JPM Month Change`  ~ `SP500 Month Change`, Training, xlim=c(-.2,.2), ylim=c(-.2,.2), col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(`JPM Month Change`  ~ `SP500 Month Change`, Testing, xlim=c(-.2,.2), ylim=c(-.2,.2),  col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Training$`JPM Month Change`, Training$`SP500 Month Change`, col='blue') #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$`JPM Month Change`, Testing$`SP500 Month Change`, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION


## 1ST MODEL ##


A1 <- lm(`JPM Month Change` ~ `SP500 Month Change`, Training)
summary(A1) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_1_IN <- predict(A1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(A1$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(A1, Testing) #generate predictions on the (out-of-sample) testing data

RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$`JPM Month Change`)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$`JPM Month Change`)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(A1, list(`SP500 Month Change`=x_grid))
plot(Training$`JPM Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`JPM Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)

hist(A1$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(A1$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR 2ND MODEL ##


A2 <- lm(`JPM Month Change` ~ `SP500 Month Change` + `SP500change2`, Training)
summary(A2) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_2_IN <- predict(A2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(A2$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(A2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$`JPM Month Change`)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$`JPM Month Change`)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(A2, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2))
plot(Training$`JPM Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`JPM Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)


hist(A2$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(A2$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR 3RD MODEL ##


#BUILD THE CUBIC MODEL FROM THE TRAINING DATA
A3 <- lm(`JPM Month Change` ~ `SP500 Month Change` + `SP500change2` + `SP500change3`, Training)
summary(A3) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_3_IN <- predict(A3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(A3$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(A3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$`JPM Month Change`)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$`JPM Month Change`)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(A3, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3))
plot(Training$`JPM Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`JPM Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)

hist(A3$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(A3$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR THE 4TH MODEL##


#BUILD THE QUARTIC MODEL FROM THE TRAINING DATA
A4 <- lm(`JPM Month Change` ~ `SP500 Month Change` + `SP500change2` + `SP500change3` + SP500change4, Training)
summary(A4) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_4_IN <- predict(A4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(A4$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(A4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$`JPM Month Change`)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$`JPM Month Change`)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(A4, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3, SP500change4=x_grid^4))
plot(Training$`JPM Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`JPM Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)

hist(A4$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(A4$residuals) #UTILIZE J-B TEST FOR NORMLAITY

######################################
######### MODEL COMPARISON ###########
######################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_IN #MODEL WITH LINEAR, QUADRATIC, CUBIC, AND QUARTIC TERM

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_OUT #MODEL WITH LINEAR, QUADRATIC, CUBIC, AND QUARTIC TERM

#CREATE VECTOR WITH EACH RESULTING IN-SAMPLE ERROR
c(RMSE_1_IN,RMSE_2_IN,RMSE_3_IN,RMSE_4_IN)

#CREATE VECTOR WITH EACH RESULTING OUT-OF-SAMPLE ERROR
c(RMSE_1_OUT,RMSE_2_OUT,RMSE_3_OUT,RMSE_4_OUT)


########################################################
##   PLOT THE REGRESSION MODELS AGAINST ONE ANOTHER   ##
########################################################

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$`JPM Month Change` ~ Training$`SP500 Month Change`, col='blue')
predictions_1 <- predict(A1, list(`SP500 Month Change`=x_grid))
predictions_2 <- predict(A2, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2))
predictions_3 <- predict(A3, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3))
predictions_4 <- predict(A4, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3, SP500change4=x_grid^4))
lines(x_grid, predictions_1, col='red', lwd=3) #PLOTS A1
lines(x_grid, predictions_2, col='orange', lwd=3) #PLOTS A2
lines(x_grid, predictions_3, col='yellow', lwd=3) #PLOTS A3
lines(x_grid, predictions_4, col='green', lwd=3) #PLOTS A4
points(Testing$`JPM Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)


################################################################################
################################################################################

## LET'S LOOK INTO THE SP500'S MONTH CHANGE EFFECT ON AAPL'S MONTH CHANGE ##

#PLOT THE TRAINING AND TESTING PARTITIONS
plot(`AAPL Month Change`  ~ `SP500 Month Change`, my_StockChange, xlim=c(-.2,.2), ylim=c(-.2,.2)) #PLOT ENTIRE DATASET
plot(`AAPL Month Change`  ~ `SP500 Month Change`, Training, xlim=c(-.2,.2), ylim=c(-.2,.2), col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(`AAPL Month Change`  ~ `SP500 Month Change`, Testing, xlim=c(-.2,.2), ylim=c(-.2,.2),  col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Training$`AAPL Month Change`, Training$`SP500 Month Change`, col='blue') #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$`AAPL Month Change`, Testing$`SP500 Month Change`, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

my_StockChange$SP500change2<-my_StockChange$`SP500 Month Change`^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
my_StockChange$SP500change3<-my_StockChange$`SP500 Month Change`^3 #CUBIC TRANSFORMATION (3rd ORDER)
my_StockChange$SP500change4<-my_StockChange$`SP500 Month Change`^4 #QUARTIC TRANSFORMATION (4th ORDER)


p<-.7 #fraction of sample to be used for training


obs_count<-dim(my_StockChange)[1] #number of observations (rows) in the dataframe

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size

set.seed(1234) #set the seed to make your partition reproducible

train_ind <- sample(obs_count, size = training_size) #create a vector with the shuffled row numbers of the original dataset

Training <- my_StockChange[train_ind, ] #pulls random rows for training
Testing <- my_StockChange[-train_ind, ] #pulls random rows for testing

#CHECK THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)


## 1ST MODEL ##


B1 <- lm(`AAPL Month Change` ~ `SP500 Month Change`, Training)
summary(B1) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_1_IN <- predict(B1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(B1$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(B1, Testing) #generate predictions on the (out-of-sample) testing data

RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$`AAPL Month Change`)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$`AAPL Month Change`)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(B1, list(`SP500 Month Change`=x_grid))
plot(Training$`AAPL Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AAPL Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)

hist(B1$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(B1$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR 2ND MODEL ##


B2 <- lm(`AAPL Month Change` ~ `SP500 Month Change` + `SP500change2`, Training)
summary(B2) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_2_IN <- predict(B2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(B2$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(B2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$`AAPL Month Change`)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$`AAPL Month Change`)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(B2, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2))
plot(Training$`AAPL Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AAPL Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)

hist(B2$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(B2$residuals) #UTILIZE J-B TEST FOR NORMLAITY

## TIME FOR 3RD MODEL ##

#BUILD THE CUBIC MODEL FROM THE TRAINING DATA
B3 <- lm(`AAPL Month Change` ~ `SP500 Month Change` + `SP500change2` + `SP500change3`, Training)
summary(B3) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_3_IN <- predict(B3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(B3$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(B3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$`AAPL Month Change`)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$`AAPL Month Change`)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(B3, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3))
plot(Training$`AAPL Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AAPL Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)

hist(B3$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(B3$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR THE 4TH MODEL ##


#BUILD THE QUARTIC MODEL FROM THE TRAINING DATA
B4 <- lm(`AAPL Month Change` ~ `SP500 Month Change` + `SP500change2` + `SP500change3` + SP500change4, Training)
summary(B4) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_4_IN <- predict(B4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(B4$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(B4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$`AAPL Month Change`)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$`AAPL Month Change`)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(B4, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3, SP500change4=x_grid^4))
plot(Training$`AAPL Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AAPL Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)

hist(B4$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(B4$residuals) #UTILIZE J-B TEST FOR NORMLAITY

######################################
########   MODEL COMPARISON  #########
######################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_IN #MODEL WITH LINEAR, QUADRATIC, CUBIC, AND QUARTIC TERM

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_OUT #MODEL WITH LINEAR, QUADRATIC, CUBIC, AND QUARTIC TERM

#CREATE VECTOR WITH EACH RESULTING IN-SAMPLE ERROR
c(RMSE_1_IN,RMSE_2_IN,RMSE_3_IN,RMSE_4_IN)

#CREATE VECTOR WITH EACH RESULTING OUT-OF-SAMPLE ERROR
c(RMSE_1_OUT,RMSE_2_OUT,RMSE_3_OUT,RMSE_4_OUT)

########################################################
###  PLOT THE REGRESSION MODELS AGAINST ONE ANOTHER  ###
########################################################

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$`AAPL Month Change` ~ Training$`SP500 Month Change`, col='blue')
predictions_1 <- predict(B1, list(`SP500 Month Change`=x_grid))
predictions_2 <- predict(B2, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2))
predictions_3 <- predict(B3, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3))
predictions_4 <- predict(B4, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3, SP500change4=x_grid^4))
lines(x_grid, predictions_1, col='red', lwd=3) #PLOTS B1
lines(x_grid, predictions_2, col='orange', lwd=3) #PLOTS B2
lines(x_grid, predictions_3, col='yellow', lwd=3) #PLOTS B3
lines(x_grid, predictions_4, col='green', lwd=3) #PLOTS B4
points(Testing$`AAPL Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)


################################################################################
################################################################################

## LET'S LOOK INTO THE SP500'S MONTH CHANGE EFFECT ON UNP'S MONTH CHANGE ##

#PLOTTING THE TRAINING AND TESTING PARTITIONS
plot(`UNP Month Change`  ~ `SP500 Month Change`, my_StockChange, xlim=c(-.3,.5), ylim=c(-.5,.4)) #PLOT ENTIRE DATASET
plot(`UNP Month Change`  ~ `SP500 Month Change`, Training, xlim=c(-.3,.5), ylim=c(-.5,.4), col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(`UNP Month Change`  ~ `SP500 Month Change`, Testing, xlim=c(-.3,.5), ylim=c(-.5,.4),  col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Training$`UNP Month Change`, Training$`SP500 Month Change`, col='blue') #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$`UNP Month Change`, Testing$`SP500 Month Change`, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

my_StockChange$SP500change2<-my_StockChange$`SP500 Month Change`^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
my_StockChange$SP500change3<-my_StockChange$`SP500 Month Change`^3 #CUBIC TRANSFORMATION (3rd ORDER)
my_StockChange$SP500change4<-my_StockChange$`SP500 Month Change`^4 #QUARTIC TRANSFORMATION (4th ORDER)



p<-.7 #fraction of sample to be used for training


obs_count<-dim(my_StockChange)[1] #number of observations (rows) in the dataframe

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size

set.seed(1234) #set the seed to make your partition reproducible

train_ind <- sample(obs_count, size = training_size) #create a vector with the shuffled row numbers of the original dataset

Training <- my_StockChange[train_ind, ] #pulls random rows for training
Testing <- my_StockChange[-train_ind, ] #pulls random rows for testing

#CHECK THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)


C1 <- lm(`UNP Month Change` ~ `SP500 Month Change`, Training)
summary(C1) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_1_IN <- predict(C1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(C1$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(C1, Testing) #generate predictions on the (out-of-sample) testing data

RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$`UNP Month Change`)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$`UNP Month Change`)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(C1, list(`SP500 Month Change`=x_grid))
plot(Training$`UNP Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`UNP Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)

hist(C1$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(C1$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR 2ND MODEL ##


C2 <- lm(`UNP Month Change` ~ `SP500 Month Change` + `SP500change2`, Training)
summary(C2) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_2_IN <- predict(C2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(C2$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(C2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$`UNP Month Change`)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$`UNP Month Change`)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(C2, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2))
plot(Training$`UNP Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`UNP Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)

hist(C2$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(C2$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR THE 3RD MODEL ##


#BUILD THE CUBIC MODEL FROM THE TRAINING DATA
C3 <- lm(`UNP Month Change` ~ `SP500 Month Change` + `SP500change2` + `SP500change3`, Training)
summary(C3) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_3_IN <- predict(C3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(C3$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(C3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$`UNP Month Change`)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$`UNP Month Change`)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(C3, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3))
plot(Training$`UNP Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`UNP Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)


hist(C3$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(C3$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR THE 4TH MODEL ##


#BUILD THE QUARTIC MODEL FROM THE TRAINING DATA
C4 <- lm(`UNP Month Change` ~ `SP500 Month Change` + `SP500change2` + `SP500change3` + SP500change4, Training)
summary(C4) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_4_IN <- predict(C4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(C4$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(C4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$`UNP Month Change`)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$`UNP Month Change`)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(C4, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3, SP500change4=x_grid^4))
plot(Training$`UNP Month Change` ~ Training$`SP500 Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`UNP Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)

hist(C4$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(C4$residuals) #UTILIZE J-B TEST FOR NORMLAITY

######################################
#########  MODEL COMPARISON  #########
######################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_IN #MODEL WITH LINEAR, QUADRATIC, CUBIC, AND QUARTIC TERM

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_OUT #MODEL WITH LINEAR, QUADRATIC, CUBIC, AND QUARTIC TERM

#CREATE VECTOR WITH EACH RESULTING IN-SAMPLE ERROR
c(RMSE_1_IN,RMSE_2_IN,RMSE_3_IN,RMSE_4_IN)

#CREATE VECTOR WITH EACH RESULTING OUT-OF-SAMPLE ERROR
c(RMSE_1_OUT,RMSE_2_OUT,RMSE_3_OUT,RMSE_4_OUT)

########################################################
###  PLOT THE REGRESSION MODELS AGAINST ONE ANOTHER  ###
########################################################

x_grid <- seq(-.15,.15,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$`UNP Month Change` ~ Training$`SP500 Month Change`, col='blue')
predictions_1 <- predict(B1, list(`SP500 Month Change`=x_grid))
predictions_2 <- predict(B2, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2))
predictions_3 <- predict(B3, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3))
predictions_4 <- predict(B4, list(`SP500 Month Change`=x_grid, SP500change2=x_grid^2, SP500change3=x_grid^3, SP500change4=x_grid^4))
lines(x_grid, predictions_1, col='red', lwd=3) #PLOTS B1
lines(x_grid, predictions_2, col='orange', lwd=3) #PLOTS B2
lines(x_grid, predictions_3, col='yellow', lwd=3) #PLOTS B3
lines(x_grid, predictions_4, col='green', lwd=3) #PLOTS B4
points(Testing$`UNP Month Change` ~ Testing$`SP500 Month Change`, col='red', pch=3)


################################################################################
################################################################################

## TIME FOR WEAK RELATED STOCKS ##

## LET'S LOOK INTO THE XOM'S MONTH CHANGE EFFECT ON AMZN'S MONTH CHANGE ##

#PLOT THE TRAINING AND TESTING PARTITIONS
plot(`AMZN Month Change`  ~ `XOM Month Change`, my_StockChange, xlim=c(-.2,.2), ylim=c(-.2,.2)) #PLOT ENTIRE DATASET
plot(`AMZN Month Change`  ~ `XOM Month Change`, Training, xlim=c(-.2,.2), ylim=c(-.2,.2), col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(`AMZN Month Change`  ~ `XOM Month Change`, Testing, xlim=c(-.2,.2), ylim=c(-.2,.2),  col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Training$`AMZN Month Change`, Training$`XOM Month Change`, col='blue') #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$`AMZN Month Change`, Testing$`XOM Month Change`, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

my_StockChange$XOMchange2<-my_StockChange$`XOM Month Change`^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
my_StockChange$XOMchange3<-my_StockChange$`XOM Month Change`^3 #CUBIC TRANSFORMATION (3rd ORDER)
my_StockChange$XOMchange4<-my_StockChange$`XOM Month Change`^4 #QUARTIC TRANSFORMATION (4th ORDER)



p<-.7 #fraction of sample to be used for training


obs_count<-dim(my_StockChange)[1] #number of observations (rows) in the dataframe

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size

set.seed(1234) #set the seed to make your partition reproducible

train_ind <- sample(obs_count, size = training_size) #create a vector with the shuffled row numbers of the original dataset

Training <- my_StockChange[train_ind, ] #pulls random rows for training
Testing <- my_StockChange[-train_ind, ] #pulls random rows for testing

#CHECK THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)


D1 <- lm(`AMZN Month Change` ~ `XOM Month Change`, Training)
summary(D1) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_1_IN <- predict(D1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(D1$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(D1, Testing) #generate predictions on the (out-of-sample) testing data

RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$`AMZN Month Change`)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$`AMZN Month Change`)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.3,.3,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(D1, list(`XOM Month Change`=x_grid))
plot(Training$`AMZN Month Change` ~ Training$`XOM Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AMZN Month Change` ~ Testing$`XOM Month Change`, col='red', pch=3)

hist(D1$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(D1$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR THE 2ND MODEL ##


D2 <- lm(`AMZN Month Change` ~ `XOM Month Change` + `XOMchange2`, Training)
summary(D2) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_2_IN <- predict(D2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(D2$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(D2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$`AMZN Month Change`)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$`AMZN Month Change`)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.3,.3,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(D2, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2))
plot(Training$`AMZN Month Change` ~ Training$`XOM Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AMZN Month Change` ~ Testing$`XOM Month Change`, col='red', pch=3)

hist(D2$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(D2$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR THE 3RD MODEL ##


#BUILD THE CUBIC MODEL FROM THE TRAINING DATA
D3 <- lm(`AMZN Month Change` ~ `XOM Month Change` + `XOMchange2` + `XOMchange3`, Training)
summary(D3) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_3_IN <- predict(D3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(D3$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(D3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$`AMZN Month Change`)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$`AMZN Month Change`)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.3,.3,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(D3, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2, XOMchange3=x_grid^3))
plot(Training$`AMZN Month Change` ~ Training$`XOM Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AMZN Month Change` ~ Testing$`XOM Month Change`, col='red', pch=3)

hist(D3$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(D3$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR THE 4TH MODEL ##


#BUILD THE QUARTIC MODEL FROM THE TRAINING DATA
D4 <- lm(`AMZN Month Change` ~ `XOM Month Change` + `XOMchange2` + `XOMchange3` + `XOMchange4`, Training)
summary(D4) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_4_IN <- predict(D4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(D4$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(D4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$`AMZN Month Change`)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$`AMZN Month Change`)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.3,.3,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(D4, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2, XOMchange3=x_grid^3, XOMchange4=x_grid^4))
plot(Training$`AMZN Month Change` ~ Training$`XOM Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AMZN Month Change` ~ Testing$`XOM Month Change`, col='red', pch=3)

hist(D4$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(D4$residuals) #UTILIZE J-B TEST FOR NORMLAITY


######################################
#########  MODEL COMPARISON  #########
######################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_IN #MODEL WITH LINEAR, QUADRATIC, CUBIC, AND QUARTIC TERM

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_OUT #MODEL WITH LINEAR, QUADRATIC, CUBIC, AND QUARTIC TERM

#CREATE VECTOR WITH EACH RESULTING IN-SAMPLE ERROR
c(RMSE_1_IN,RMSE_2_IN,RMSE_3_IN,RMSE_4_IN)

#CREATE VECTOR WITH EACH RESULTING OUT-OF-SAMPLE ERROR
c(RMSE_1_OUT,RMSE_2_OUT,RMSE_3_OUT,RMSE_4_OUT)

########################################################
###  PLOT THE REGRESSION MODELS AGAINST ONE ANOTHER  ###
########################################################

x_grid <- seq(-.3,.3,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$`AMZN Month Change` ~ Training$`XOM Month Change`, col='blue')
predictions_1 <- predict(D1, list(`XOM Month Change`=x_grid))
predictions_2 <- predict(D2, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2))
predictions_3 <- predict(D3, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2, XOMchange3=x_grid^3))
predictions_4 <- predict(D4, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2, XOMchange3=x_grid^3, XOMchange4=x_grid^4))
lines(x_grid, predictions_1, col='red', lwd=3) #PLOTS D1
lines(x_grid, predictions_2, col='orange', lwd=3) #PLOTS D2
lines(x_grid, predictions_3, col='yellow', lwd=3) #PLOTS D3
lines(x_grid, predictions_4, col='green', lwd=3) #PLOTS D4
points(Testing$`AMZN Month Change` ~ Testing$`XOM Month Change`, col='red', pch=3)


################################################################################
################################################################################

## LET'S LOOK INTO THE XOM'S MONTH CHANGE EFFECT ON AAPL'S MONTH CHANGE ##

#PLOTTING THE TRAINING AND TESTING PARTITIONS
plot(`AAPL Month Change`  ~ `XOM Month Change`, my_StockChange, xlim=c(-.2,.2), ylim=c(-.2,.2)) #PLOT ENTIRE DATASET
plot(`AAPL Month Change`  ~ `XOM Month Change`, Training, xlim=c(-.2,.2), ylim=c(-.2,.2), col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(`AAPL Month Change`  ~ `XOM Month Change`, Testing, xlim=c(-.2,.2), ylim=c(-.2,.2),  col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Training$`AAPL Month Change`, Training$`XOM Month Change`, col='blue') #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$`AAPL Month Change`, Testing$`XOM Month Change`, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

my_StockChange$XOMchange2<-my_StockChange$`XOM Month Change`^2 #QUADRATIC TRANSFORMATION (2nd ORDER)
my_StockChange$XOMchange3<-my_StockChange$`XOM Month Change`^3 #CUBIC TRANSFORMATION (3rd ORDER)
my_StockChange$XOMchange4<-my_StockChange$`XOM Month Change`^4 #QUARTIC TRANSFORMATION (4th ORDER)



p<-.7 #fraction of sample to be used for training


obs_count<-dim(my_StockChange)[1] #number of observations (rows) in the dataframe

#number of observations to be selected for the training partition
#the floor() function rounds down to the nearest integer
training_size <- floor(p * obs_count)
training_size

set.seed(1234) #set the seed to make your partition reproducible

train_ind <- sample(obs_count, size = training_size) #create a vector with the shuffled row numbers of the original dataset

Training <- my_StockChange[train_ind, ] #pulls random rows for training
Testing <- my_StockChange[-train_ind, ] #pulls random rows for testing

#CHECK THE DIMENSIONS OF THE PARTITIONED DATA
dim(Training)
dim(Testing)


E1 <- lm(`AAPL Month Change` ~ `XOM Month Change`, Training)
summary(E1) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_1_IN <- predict(E1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(E1$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_1_OUT <- predict(E1, Testing) #generate predictions on the (out-of-sample) testing data

RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$`AAPL Month Change`)^2)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$`AAPL Month Change`)^2)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_1_IN #IN-SAMPLE ERROR
RMSE_1_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.3,.3,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(E1, list(`XOM Month Change`=x_grid))
plot(Training$`AAPL Month Change` ~ Training$`XOM Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AAPL Month Change` ~ Testing$`XOM Month Change`, col='red', pch=3)

hist(E1$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(E1$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR 2ND MODEL ##


E2 <- lm(`AAPL Month Change` ~ `XOM Month Change` + `XOMchange2`, Training)
summary(E2) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_2_IN <- predict(E2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(E2$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_2_OUT <- predict(E2, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$`AAPL Month Change`)^2)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$`AAPL Month Change`)^2)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_2_IN #IN-SAMPLE ERROR
RMSE_2_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.3,.3,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(E2, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2))
plot(Training$`AAPL Month Change` ~ Training$`XOM Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AAPL Month Change` ~ Testing$`XOM Month Change`, col='red', pch=3)

hist(E2$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(E2$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR THE 3RD MODEL ##


#BUILD THE CUBIC MODEL FROM THE TRAINING DATA
E3 <- lm(`AAPL Month Change` ~ `XOM Month Change` + `XOMchange2` + `XOMchange3`, Training)
summary(E3) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_3_IN <- predict(E3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(E3$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_3_OUT <- predict(E3, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$`AAPL Month Change`)^2)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$`AAPL Month Change`)^2)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_3_IN #IN-SAMPLE ERROR
RMSE_3_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.3,.3,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(E3, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2, XOMchange3=x_grid^3))
plot(Training$`AAPL Month Change` ~ Training$`XOM Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AAPL Month Change` ~ Testing$`XOM Month Change`, col='red', pch=3)

hist(E3$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(E3$residuals) #UTILIZE J-B TEST FOR NORMLAITY


## TIME FOR THE 4TH MODEL ##


#BUILD THE QUARTIC MODEL FROM THE TRAINING DATA
E4 <- lm(`AAPL Month Change` ~ `XOM Month Change` + `XOMchange2` + `XOMchange3` + `XOMchange4`, Training)
summary(E4) #generates summary diagnostic output

#GENERATE PREDICTIONS BASED ON THE TRAINING DATA
PRED_4_IN <- predict(E4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(E4$fitted.values) #these are the same as the fitted values

#GENERATE PREDICTIONS BASED ON THE TEST DATA FOR BENCHMARKING
PRED_4_OUT <- predict(E4, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$`AAPL Month Change`)^2)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$`AAPL Month Change`)^2)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_4_IN #IN-SAMPLE ERROR
RMSE_4_OUT #OUT-OF-SAMPLE ERROR

#PLOT THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS

x_grid <- seq(-.3,.3,.1) #CREATES GRID OF X-AXIS VALUES
predictions <- predict(E4, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2, XOMchange3=x_grid^3, XOMchange4=x_grid^4))
plot(Training$`AAPL Month Change` ~ Training$`XOM Month Change`, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(Testing$`AAPL Month Change` ~ Testing$`XOM Month Change`, col='red', pch=3)

hist(E4$residuals) #PLOT RESIDUALS TO DETERMINE IF NORMAL
jarque.bera.test(E4$residuals) #UTILIZE J-B TEST FOR NORMLAITY


######################################
#########  MODEL COMPARISON  #########
######################################

#COMPARISON OF IN-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_IN #MODEL WITH ONLY LINEAR TERM
RMSE_2_IN #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_IN #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_IN #MODEL WITH LINEAR, QUADRATIC, CUBIC, AND QUARTIC TERM

#COMPARISON OF OUT-OF-SAMPLE MODEL PERFORMANCE BY RMSE
RMSE_1_OUT #MODEL WITH ONLY LINEAR TERM
RMSE_2_OUT #MODEL WITH LINEAR AND QUADRATIC TERM
RMSE_3_OUT #MODEL WITH LINEAR, QUADRATIC, AND CUBIC TERM
RMSE_4_OUT #MODEL WITH LINEAR, QUADRATIC, CUBIC, AND QUARTIC TERM

#CREATE VECTOR WITH EACH RESULTING IN-SAMPLE ERROR
c(RMSE_1_IN,RMSE_2_IN,RMSE_3_IN,RMSE_4_IN)

#CREATE VECTOR WITH EACH RESULTING OUT-OF-SAMPLE ERROR
c(RMSE_1_OUT,RMSE_2_OUT,RMSE_3_OUT,RMSE_4_OUT)

########################################################
###  PLOT THE REGRESSION MODELS AGAINST ONE ANOTHER  ###
########################################################

x_grid <- seq(-.3,.3,.1) #CREATES GRID OF X-AXIS VALUES
plot(Training$`AAPL Month Change` ~ Training$`XOM Month Change`, col='blue')
predictions_1 <- predict(E1, list(`XOM Month Change`=x_grid))
predictions_2 <- predict(E2, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2))
predictions_3 <- predict(E3, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2, XOMchange3=x_grid^3))
predictions_4 <- predict(E4, list(`XOM Month Change`=x_grid, XOMchange2=x_grid^2, XOMchange3=x_grid^3, XOMchange4=x_grid^4))
lines(x_grid, predictions_1, col='red', lwd=3) #PLOTS E1
lines(x_grid, predictions_2, col='orange', lwd=3) #PLOTS E2
lines(x_grid, predictions_3, col='yellow', lwd=3) #PLOTS E3
lines(x_grid, predictions_4, col='green', lwd=3) #PLOTS E4
points(Testing$`AAPL Month Change` ~ Testing$`XOM Month Change`, col='red', pch=3)

################################################################################
################################################################################

