---
title: "Prediction of manners in motion excises on accelerometers"
author: "DAHAO LING"
date: "Saturday, May 23, 2015"
output: html_document
---

Summary of project:

The project is aim to study the manners of activities by 6 people from motion device accelerometers to test how well they do when doing excises. Recently large amount of data generated from the motion devices. Most of the data related to what kind of activities measures but not how the subject do the activities. This study uses the motion detector data to predict the manner and test how well a subject can do an activity. Prediction models build base on the data from the devices and test the sample errors by cross validation and testing set. Finally apply the final model on a 20 sample test set to be submitted.


##Data loading and Exploratory


The original data can be found here:http://groupware.les.inf.puc-rio.br/har.
The training data for us to use is half the original data and contain 160 columns and 19622 rows. Among these some columns contain degenerated data from the the motion detector, such as min_,max_, skew_, and some are raw data from the motion detectors. The variable classe is the variable representing the activity manners and has ABCDE five levels. I use classe as the respond variable and other 52 columns which contain raw data from motion detectors as predictors.   


```{r Data origin,echo=FALSE}
require(ggplot2);require(caret);require(randomForest);set.seed(345)
url1="http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2="http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
```
---
The first step is cleaning the data. From the plot below we can see there is a pattern between certain raw data and activity pattern. But degenerated data
columns are huge and contain large amount of NAs. I will clean it by deleting those degenerating columns which containing large amount of NAs.I finally got a data set with 52 columns of raw data from motion detectors and one column of variable "classe".



```{r Data loading and Cleaning,echo=FALSE}
rtrain=read.csv(file=url1,na.strings=c("","NA","#DIV/0!"),stringsAsFactors=FALSE)
test=read.csv(file=url2,na.strings=c("","NA","#DIV/0!"),stringsAsFactors=FALSE)
rtrain_NA<-rtrain[,colSums(is.na(rtrain))<800]
dim(rtrain_NA)
#head(rtrain_NA,2)
#str(test)
rtrain2<-rtrain_NA[,8:60]
#str(rtrain2)
rtrain2$classe<-factor(rtrain2$classe)
```


## Building Prediction Models


The method I used to build the prediction model is randomForest. I also use randomForest() function instead of train() to build the model. Since randomForest function needs big RAM to run, I have to use 40% of the clean data from above as the training set and use a 5 fold cross validation and the testing set (60% clean data) to test the model. Only very small portions of red points missed in the plots. The oob(out of bag) error is only 1.29% for the validation of the model when rf use 40 to 50 variables in the model building. The accuracy is 98.71% 

I estimated the cross validation and test set should be the same error rate since they from the same experiment(same collecting method) and same set of data.

On testing set, which is the other part of data from cleaned NAs otherwise than training data, the error rate is around 1% to 2% as indicated by the table and qplot. 

The error rate of testing data prediction calculated as: 
(20+42+62+10+5)/(3356+2278+1907+2156)=0.014 
based on the data in the table. The accurarcy rate is: 98.6%

I got 100% prediction right on the 20 sample test set.

Data partiton and plot the raw data with classe to show their correlations:
```{r Data partitiion, echo=FALSE}
inTrain<-createDataPartition(y=rtrain2$classe, p=0.4, list=FALSE)
training<-rtrain2[inTrain,]; testing<-rtrain2[-inTrain,]
qplot(roll_belt,yaw_belt,colour=classe, data=training, main="Raw data exploratory")
```

Prediction model building up using randomForest function
```{r Prediction modeling,echo=FALSE}
modFit<-randomForest(classe~.,data=training,importance=TRUE, do.trace=100, prox=TRUE, na.action=na.omit)
modFit
#varImp(modFit)
```

Testing prediction model on 20 sample test set: 
```{r model testing on 20 sample test data,echo=FALSE}
a<-predict(modFit, test)
a
```


Testing prediction model on testing set, gives the error rate based on the cross validation table and matching plot. Only a very small portion marked red as mismatch on the validation Plot.
```{r model testing on testing data,echo=FALSE}
b<-predict(modFit,testing)
testing$predRight<-b==testing$classe
table(b,testing$classe)
errorate<-(20+42+62+10+5)/(3356+2278+1907+2156); 
paste("Errorate is", errorate)
qplot(roll_belt,yaw_belt,colour=predRight, data=testing, main="Validation Prediction")
```


In random forests, there is no need for cross-validation or a separate test set to get an unbiased estimate of the test set error. It is estimated internally, during the run. 

Each tree is constructed using a different bootstrap sample from the original data. About one-third of the cases are left out of the bootstrap sample and not used in the construction of the kth tree. The OOB number is given automatically. To be specific and learn how to do a cross validation, I did it as follows: 5 fold cv. The error rate is between 0.016 to 0.019 when using  30 to 50 variables. 

All the prediction testing match my estimation. 


## Cross Validation


The list shows the variable number on the first row and the second row is error.cv.


```{r Cross Validation,echo=FALSE}
x<-training; x$classe<-NULL
y<-training$classe
rf.cv<-rfcv(x,y,cv.fold=5, recursive=FALSE)
rf.cv$error.cv
with(rf.cv, plot(n.var, error.cv))
```


## Discussuion about the sample errors


```{r Sample errors discussion,echo=FALSE}
rtrain3<-rtrain2[,1:52]
PCA<-preProcess(rtrain3, method="pca", pcaComp=2)
qplot(PC1,PC2, data=predict(PCA, rtrain3), color=rtrain_NA$user_name)
```


The graph shows that different subject has quite diverse pattern of predictors. That indicated that the subjects maybe one of the source of sample errors.

##References:
Origin data source: Weight Lifting Exercises Dataset at http://groupware.les.inf.puc-rio.br/har
Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013. 




