 #########################################################################################
#########################################################################################
###################################ATTENTION#############################################
########THIS MACRO IS INTENDED FOR CLASSIFICATION AND REGRESSION TREES (CART)############
#########################################################################################
#########################################################################################

 str(mydata)
library(tree)
library(ftsa)
library(Hmisc)
library(gmodels)


#############################################################################################
##############################UPDATE THE SECTION BELOW#######################################
#############################################################################################

#START OF SETUP

#Enter a name of your project below, after the equality sign, in ""s 

proj_name="CTG"

#Enter the path of your Reports directory below after the equality sign, in ""s. 
#This is where the predictions will be exported in *.csv format at the end of the program.

report_path="C:/Users/jgozal1/Documents/MyRStudio/Reports/"

#Enter "R" for a regression tree and "C" for a classificaiton tree below.

tree_type="C"

#Enter the minium number of items in a node before it can be considered for a split

min_size_bef_split=2

#Enter the required change in deviance for a split to be carried out

mid_deviance_improv=0


#END OF SETUP


#START OF DATA IMPORT

#update the path below to point to the directory and name of your data in *.csv format  

mydata=read.csv("C:/Users/jgozal1/Desktop/CTG/CTG Modified.csv")

#END OF DATA IMPORT

#START OF VARIABLE REDEFINITION

mydata$myresponse=mydata$NSP #Substitute "RESPONSE" with the name of your response variable
mydata$NSP=NULL #Substitute "RESPONSE" with the name of your response variable

str(mydata)

#END OF VARIABLE REDEFINITION

#In the following statements substitute the names after "$" sign with the names of variables
#in your data that are categorical but are read into R as "int" (integer). If there are no such 
#variables in your data, then the statements in the "VARIABLE TRANSFORMATION"
#section below should be deleted.

#START OF VARIABLE TRANSFORMATION

mydata$myresponse=as.factor(mydata$myresponse) 
mydata$A=as.factor(mydata$A)
mydata$E=as.factor(mydata$E)
mydata$AD=as.factor(mydata$AD)
mydata$DE=as.factor(mydata$DE)
mydata$FS=as.factor(mydata$FS)
mydata$SUSP=as.factor(mydata$SUSP)

str(mydata)

#add statements similar to above as needed

#Remember that compared to a Regression Tree, when growing a Classificaiton Tree 
#the response needs to be a factor.
#Un-comment the line below only if the response needs to be converted to categorical(factor)

#mydata$myresponse=as.factor(mydata$myresponse)

#END OF VARIABLE TRANSFORMATION

#The statements below remove all the variables that will not be passed to the tree algorithm
#as predictors. If no such redundant variables exist in your dataset, then the statements
#in the "REDUNDANT VARIABLE REMOVAL" section should be deleted.

#START OF REDUNDANT VARIABLE REMOVAL

mydata$FileName=NULL #Substitute "OBS." with the name of the variable in your data that 
#will not be passed to the tree algorithm. Add as many statements similar 
#to this as needed.

mydata$Date=NULL  #Substitute "CHK_ACCT" with the name of the variable in your data that 
#will not be passed to the tree algorithm.

#mydata$Width=NULL
#mydata$Min=NULL
#mydata$Max=NULL
mydata$Nmax=NULL
mydata$Nzeros=NULL
#mydata$Mode=NULL
#mydata$Mean=NULL
#mydata$Median=NULL
#mydata$Variance=NULL
mydata$Tendency=NULL
mydata$SegFile=NULL
mydata$DR=NULL
#mydata$A=NULL
mydata$B=NULL
mydata$C=NULL
mydata$D=NULL
#mydata$E=NULL
#mydata$AD=NULL
#mydata$DE=NULL
mydata$LD=NULL
#mydata$FS=NULL
#mydata$SUSP=NULL
mydata$CLASS=NULL
mydata$DS=NULL

mydata$FM=NULL
mydata$DL=NULL
#mydata$DP=NULL
#mydata$AC=NULL
#mydata$ALTV=NULL


#END OF REDUNDANT VARIABLE REMOVAL

#############################################################################################
#####################################ATTENTION###############################################
#############################################################################################

#######################IF THE ABOVE MODIFICATIONS ARE MADE CORRECTLY,########################
####AT THIS POINT "MYDATA" DATA FRAME SHOULD CONTAIN ONLY THE PREDICTORS AND THE OUTCOME.#### 
####IN CASE IT CONTAINS ANYTHING MORE OR LESS, THE CODE BELOW WILL NOT FUNCTION PROPERLY.####
#############################################################################################

str(mydata) #make sure the structure of your data reflects all the modifications made above

#############################################################################################
##############################DO NOT MODIFY THE CODE BELOW###################################
#############################################################################################


#START DATA BREAKDOWN FOR HOLDOUT METHOD

#Start finding the categorical predictors

numpredictors=dim(mydata)[2]-1

numfac=0

for (i in 1:numpredictors) {
  if ((is.factor(mydata[,i]))){
    numfac=numfac+1} 
}

#End finding the number of categorical predictors 

nobs=dim(mydata)[1]



if (tree_type=="R") {
  
  #Below is the setup for stratified 80-20 holdout sampling for a Regression Tree
  
  train_size=floor(0.8*nobs)
  test_size=nobs-train_size
  
} else {
  
  #Below is the setup for stratified 80-20 holdout sampling for a Classification Tree
  
  prop = prop.table(table(mydata$myresponse))
  length.vector = round(nobs*0.8*prop)
  train_size=sum(length.vector)
  test_size=nobs-train_size
  class.names = as.data.frame(prop)[,1]
  numb.class = length(class.names)}


resample=1
set.seed(1) #sets the seed for random sampling

while (resample==1) {
  
  
  if (tree_type=="C") {
    
    train_index = c()
    
    for(i in 1:numb.class){
      index_temp = which(mydata$myresponse==class.names[i])
      train_index_temp = sample(index_temp, length.vector[i], replace = F)
      train_index = c(train_index, train_index_temp)
    }} else {
      train_index=sample(nobs,train_size, replace=F)
    }
  
  mydata_train=mydata[train_index,] #randomly select the data for training set using the row numbers generated above
  mydata_test=mydata[-train_index,]#everything not in the training set should go into testing set
  
  right_fac=0 #denotes the number of factors with "right" distributions (i.e. - the unique levels match across mydata, test, and train data sets)
  
  
  for (i in 1:numpredictors) {
    if (is.factor(mydata_train[,i])) {
      if (sum(as.vector(unique(mydata_test[,i])) %in% as.vector(unique(mydata_train[,i])))==length(unique(mydata_test[,i])))
        right_fac=right_fac+1
    }
  }
  
  if (right_fac==numfac) (resample=0) else (resample=1)
  
}

dim(mydata_test) #confirms that testing data has only 20% of observations
dim(mydata_train) #confirms that training data has 80% of observations


#END DATA BREAKDOWN FOR HOLDOUT METHOD


#Start growing the reference tree
full_tree=tree(myresponse ~ .,split="deviance",mindev=mid_deviance_improv, minsize=min_size_bef_split,data=mydata_train)
summary(full_tree)
#End growing the reference tree


#START 10-FOLD CROSS-VALIDATION TO FIND THE SIZE THAT THE FULL TREE WILL BE REDUCED TO

b_list=rep(1,100)

for (i in 1:100){
  
  set.seed(i)
  cv_tree=cv.tree(full_tree,K=10)
  cv_tree$size
  cv_tree$dev
  bestsize=min(cv_tree$size[cv_tree$dev==min(cv_tree$dev)])
  b_list[i]=bestsize
  #plot(cv_tree, type="p")
  
}

mytable=as.data.frame(table(b_list))
mytable_s=mytable[order(mytable$Freq),]
final_tree_size=as.numeric(paste(mytable_s[dim(mytable_s)[1],1]))
#END K-FOLD CROSS-VALIDATION TO FIND THE SIZE THAT THE FULL TREE WILL BE REDUCED TO


#START REDUCING THE FULL TREE TO OPTIMAL SIZE AND PLOTTING

bestcut=prune.tree(full_tree,best=final_tree_size)
plot(bestcut, type=c("uniform"))
text(bestcut, cex=0.6, pretty=1)
bestcut
deviance(bestcut)

#END REDUCING THE FULL TREE TO OPTIMAL SIZE AND PLOTTING


#START PREDICTING THE RESPONSE IN THE TESTING SET (20 % SUBSET)

predicted=predict(bestcut,newdata=mydata_test, type="vector")

if (tree_type=="R") {
  id=as.numeric(names(predicted))
  temp_tbl=cbind(id,as.data.frame(predicted))
  mydata_test2=cbind(as.numeric(rownames(mydata_test)),mydata_test)
  colnames(mydata_test2)[1]="id"
  pred_table=merge(temp_tbl, mydata_test2, by.x="id", all.x=T)
  predicted=pred_table$predicted
  pred_table$predicted=NULL
  final_table=cbind(pred_table,predicted)
  final_table$id=NULL } else {
    
    predicted=as.data.frame(predicted)
    
    
    new.col = c()
    
    for(i in 1:(dim(predicted)[1])){
      new.col = c(new.col, names(predicted)[which(predicted[i,]==max(predicted[i,]))])
    }
    predicted$predicted=new.col
    
    id=as.numeric(rownames(predicted))
    newdat=as.data.frame(id)
    newdat$predicted=predicted$predicted
    mydata_test2=cbind(as.numeric(rownames(mydata_test)),mydata_test)
    colnames(mydata_test2)[1]="id"
    final_table=merge(mydata_test2,newdat,by.x="id", all.x=T)}


#Measuring predictive accuracy below

if (tree_type=="R") {
  
  print(paste("RMSE for Testing Set Is:", 
              round(error(forecast=final_table$predicted, true=final_table$myresponse, method="rmse"),2)))
  print(paste("MAPE for Testing Set Is:", 
              round(error(forecast=final_table$predicted, true=final_table$myresponse, method="mape"),2)))} else {
                print("Confusion Matrix Is:")
                CrossTable(final_table$myresponse,final_table$predicted,prop.chisq=F,prop.t=F) }

#END PREDICTING THE RESPONSE IN THE TESTING SET (20 % SUBSET)


#START EXPORTING THE TABLE OF PREDICTIONS FOR THE TESTING SET (20 % SUBSET)

write.csv(final_table,paste(report_path, proj_name, ".csv",sep=""), row.names=F)

#END EXPORTING THE TABLE OF PREDICTIONS FOR THE TESTING SET (20 % SUBSET)


#############################################################################################
##############################THIS IS THE END OF THE MACRO###################################
#############################################################################################

str(mydata)
