library(gmodels)
library(Hmisc)
library(ROCR) #Install the ROCR package if you have not yet done so. The ROC curves will not   
#be plotted if the package is not installed!

#Enter a name of your project below, after the equality sign, in ""s 

proj_name="CTG"

#Enter the path of your Reports directory below after the equality sign, in ""s. 
#This is where the predictions will be exported in *.csv format at the end of the program.

#report_path="C:/Users/jgozal1/Documents/MyRStudio/Reports/"

#Enter the cutoff for classification

cutoff=0.5


#START OF DATA IMPORT

#update the path below to point to the directory and name of your data in *.csv format  

mydata=read.csv("C:/Users/jgozal1/Desktop/CTG/CTG Modified.csv")
str(mydata)
#END OF DATA IMPORT


#START OF VARIABLE REDEFINITION

mydata$myresponse=mydata$NSP #Substitute "Personal.Loan" with the name of your response variable
mydata$NSP=NULL #Substitute "Personal.Loan" with the name of your response variable

#Un-comment the statement below ONLY IF the categorical outcome, the probability of which 
#you are modeling, is NOT given on the 0-1 scale, but instead is given as text. In that case, 
#substitute the Success_Category below by the level of the outcome variable which 
#you define/consider as the "success" category. If the outcome variable is already on 
#the 0-1 scale, then leave the statement below commented out. 

#mydata$myresponse=as.numeric(mydata$myresponse=="Success_Category") 

str(mydata)

#END OF VARIABLE REDEFINITION


#In the following statements substitute the names after "$" sign with the names of variables
#in your data that are categorical but are read into R as "int" (integer). If there are no such 
#variables in your data, then the statements in the "VARIABLE TRANSFORMATION"
#section below should be deleted or commented out.

#START OF VARIABLE TRANSFORMATION

mydata$A=as.factor(mydata$A)
mydata$B=as.factor(mydata$B)
mydata$C  =as.factor(mydata$C)
mydata$D  =as.factor(mydata$D)
mydata$E  =as.factor(mydata$E)
mydata$AD  =as.factor(mydata$AD)
mydata$DE  =as.factor(mydata$DE)
mydata$LD  =as.factor(mydata$LD)
mydata$FS  =as.factor(mydata$FS)
mydata$SUSP  =as.factor(mydata$SUSP)
mydata$myresponse  =as.factor(mydata$myresponse)
#mydata$CLASS = as.factor(mydata$CLASS)
#add statements similar to above as needed


#END OF VARIABLE TRANSFORMATION

#The statements below remove all the variables that will not be used
#as predictors. If no such redundant variables exist in your dataset, then the statements
#in the "REDUNDANT VARIABLE REMOVAL" section should be deleted or commented out.

#START OF REDUNDANT VARIABLE REMOVAL
mydata$FileName=NULL
mydata$Date=NULL
mydata$SegFile=NULL


#Add as many statements similar to the above as needed.

#END OF REDUNDANT VARIABLE REMOVAL

#############################################################################################
#####################################ATTENTION###############################################
#############################################################################################

#######################IF THE ABOVE MODIFICATIONS ARE MADE CORRECTLY,########################
####AT THIS POINT "MYDATA" DATA FRAME SHOULD CONTAIN ONLY THE PREDICTORS AND THE OUTCOME.#### 
####IN CASE IT CONTAINS ANYTHING MORE OR LESS, THE CODE BELOW WILL NOT FUNCTION PROPERLY.####
#############################################################################################


str(mydata)
describe(mydata)
table(mydata$myresponse)
#############################################################################################
########################DO NOT MODIFY LINES BELOW UNTIL WHERE IT SAYS########################
###########################"END DATA BREAKDOWN FOR HOLDOUT METHOD"###########################

#START DATA BREAKDOWN FOR HOLDOUT METHOD

#Find the number of categorical predictors first

numpredictors=dim(mydata)[2]-1

numfac=0

for (i in 1:numpredictors) {
  if ((is.factor(mydata[,i]))){
    numfac=numfac+1} 
}

#End finding the number of categorical predictors 

nobs=dim(mydata)[1]
set.seed(1) #sets the seed for random sampling

#Below is the setup for stratified 80-20 holdout sampling

prop = prop.table(table(mydata$myresponse))
length.vector = round(0.8*nobs*prop)
train_size=sum(length.vector)
test_size=nobs-train_size
class.names = as.data.frame(prop)[,1]
numb.class = length(class.names)
resample=1

#The 'while' conditional construct below breaks the data into testing(20%) and training(80%) sets assuring that the unique levels
#of each of the categorical variables is the same in mydata, testing, and training sets. If for a particular partition
#those levels do not match, then RStudio continues to perform 80-20 random splits untill such partition is found.

while (resample==1) {
  
  train_index = c()
  
  for(i in 1:numb.class){
    index_temp = which(mydata$myresponse==class.names[i])
    train_index_temp = sample(index_temp, length.vector[i], replace = F)
    train_index = c(train_index, train_index_temp)
  }
  
  mydata_train=mydata[train_index,] #randomly select the data for training set using the row numbers generated above
  mydata_test=mydata[-train_index,]#everything not in the training set should go into testing set
  
  right_fac=0 #denotes the number of factors with "right" distributions (i.e. - the unique levels match across mydata, test, and train data sets)
  
  for (i in 1:numpredictors) {
    if (is.factor(mydata_train[,i])) {
      if (setequal(intersect(as.vector(unique(mydata_train[,i])), as.vector(unique(mydata_test[,i]))),as.vector(unique(mydata[,i])))==TRUE)
        right_fac=right_fac+1
    }
  }
  
  if (right_fac==numfac) (resample=0) else (resample=1)
  
}

dim(mydata_test) #confirms that testing data has only 20% of observations
dim(mydata_train) #confirms that training data has 80% of observations


#END DATA BREAKDOWN FOR HOLDOUT METHOD

#############################################################################################
############################SPECIFY THE MODEL TO BE FITTED BELOW#############################
#############################################################################################

logistic_fit=glm(formula=myresponse~.,family=binomial,data=mydata_train)  #This fits the logistic regression model
#"family"=binomial tells the "glm" function
#that we are dealing with a binary outcome and
#that logistic regression is what needs to be fit.
#If all of the predictors that you retained
#in 'mydata' need to be used in regression
#then leave 'myresponse~.' notation unchanged.
#If, however, only part of the predictors need
#to be used, say x1, x2, and x3, then list 
#those predictors as follows 'myresponse ~ x1+x2+x3' 
#by leaving the rest of the syntax in 'glm' formula the same.


#############################################################################################
############################DO NOT MODIFY THE CODE BEYOND THIS POINT########################
#############################################################################################

summary(logistic_fit) #this displays the summary of the fitted model
predicted=predict(logistic_fit, mydata_test, type="response") #Predicts the probabilities in the testing set
#using the model built on a training set.

#START CONSTRUCTING THE CONFUSION MATRIX FOR TESTING DATA

predicted1=as.data.frame(predicted)
id=as.numeric(rownames(predicted1))
predicted_tbl0=cbind(id,predicted1)
mydata_test=cbind(id, mydata_test)
predicted_tbl=merge(predicted_tbl0,mydata_test, by.x="id", all.x=T)
predicted_tbl$predicted_class=as.numeric(predicted>=cutoff)
Confusion_Matrix=CrossTable(predicted_tbl$myresponse,predicted_tbl$predicted_class,dnn=c("True Class","Predicted Class"), prop.chisq=F,prop.t=F, prop.c=F, prop.r=F) 


#END CONSTRUCTING THE CONFUSION MATRIX FOR TESTING DATA

#START CONSTRUCTING THE ROC CURVE FOR TESTING DATA

#Below is a user-defined function from "Data Mining and Business Analytics with R" textbook
#by Johannes Ledolter.

roc=function(p,y) {
  y=factor(y)
  n=length(p)
  p=as.vector(p)
  Q=p>matrix(rep(seq(0,1,length=500),n),ncol=500,byrow=T)
  fp=colSums((y==levels(y)[1])*Q)/sum(y==levels(y)[1])
  tp=colSums((y==levels(y)[2])*Q)/sum(y==levels(y)[2])
  plot(fp,tp,xlab="1-Specificity", ylab="Sensitivity")
  abline(a=0,b=1,lty=2,col=8)
}

roc(p=predicted,y=mydata_test$myresponse) #plots the ROC curve

#END CONSTRUCTING THE ROC CURVE FOR TESTING DATA


#############################################################################################
##############################THIS IS THE END OF THE MACRO###################################
#############################################################################################
names(mydata)