library(gmodels)
library(Hmisc)
library(class) #The package that supports KNN. You have to install it first before loading with 
#the "library" command.


#Enter a name of your project below, after the equality sign, in ""s 

proj_name="CTG"

#Enter the path of your Reports directory below after the equality sign, in ""s. 
#This is where the predictions will be exported in *.csv format at the end of the program.

report_path="C:/Users/jgozal1/Documents/MyRStudio/Reports/"


#START OF DATA IMPORT

#update the path below to point to the directory and name of your data in *.csv format  

mydata=read.csv("C:/Users/jgozal1/Desktop/CTG/CTG Modified.csv")
str(mydata)

#END OF DATA IMPORT


#START OF VARIABLE REDEFINITION

mydata$myresponse=mydata$NSP #Substitute "RESPONSE" with the name of your response variable
mydata$NSP=NULL #Substitute "RESPONSE" with the name of your response variable

#Un-comment the statement below ONLY IF your outcome variable is read-into RStudio as an
#integer. If it is read in as a factor, then commented out the next statement.

mydata$myresponse=as.factor(mydata$myresponse) 


str(mydata)

#END OF VARIABLE REDEFINITION


#The statements below remove all the variables that will not be passed to the tree algorithm
#as predictors. If no such redundant variables exist in your dataset, then the statements
#in the "REDUNDANT VARIABLE REMOVAL" section should be deleted or commented out.

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


#DONOT MODIFY THE NEXT TWO LINES OF CODE
str(mydata)
raw_for_export=mydata


#Use the following statement to standardize the continuous predictors which are in need of
#standardization. You need to carefully list the numbers of the columns that need to be 
#standardized.Be careful NOT to standardize columns that are categorical or the response column.
#NOTE: If none of the predictors need to be standardized, then the entire code from
#'START OF VARIABLE STANDARDIZATION' to 'END OF VARIABLE STANDARDIZATION' need to be either
#deleted or commented out.

#START OF VARIABLE STANDARDIZATION

col_nums=c(1:24); #Substitute (1,2,3) with the possitions at which columns that
#are in need of standardization appear in the updated "mydata"
#dataframe. You can find out the numbers of columns by running
#the str(mydata) command.In this example, columns listed 
#as the 1st, 2nd and 3rd will be standardized.

#############################################################################################
#####################DO NOT MODIFY THE LINES BELOW UNTIL WHERE IT SAYS#######################
#############################"END OF VARIABLE STANDARDIZATION"###############################


cols_for_standard=as.matrix(mydata[,col_nums])
standardized=scale(cols_for_standard)
all_col_nums=c(1:length(names(mydata)))
remaining_cols=as.vector(all_col_nums[is.na(pmatch(all_col_nums, col_nums))])
remaining_data=subset(mydata,select=remaining_cols)
mydata=cbind(remaining_data, standardized)

#END OF VARIABLE STANDARDIZATION


#############################################################################################
#####################################ATTENTION###############################################
#############################################################################################

#######################IF THE ABOVE MODIFICATIONS ARE MADE CORRECTLY,########################
####AT THIS POINT "MYDATA" DATA FRAME SHOULD CONTAIN ONLY THE PREDICTORS AND THE OUTCOME.#### 
####IN CASE IT CONTAINS ANYTHING MORE OR LESS, THE CODE BELOW WILL NOT FUNCTION PROPERLY.####
#############################################################################################


str(mydata)
describe(mydata) #0=default 1=not default

#############################################################################################
########################DO NOT MODIFY LINES BELOW UNTIL WHERE IT SAYS########################
#######################################"END KNN"#############################################


#START DATA BREAKDOWN FOR HOLDOUT METHOD

nobs=dim(mydata)[1]
set.seed(1) #sets the seed for random sampling

prop = prop.table(table(mydata$myresponse))
length.vector = round(0.8*nobs*prop)
train_size=sum(length.vector)
test_size=nobs-train_size
class.names = as.data.frame(prop)[,1]
numb.class = length(class.names)
train_index = c()

for(i in 1:numb.class){
  index_temp = which(mydata$myresponse==class.names[i])
  train_index_temp = sample(index_temp, length.vector[i], replace = F)
  train_index = c(train_index, train_index_temp)
}


train=mydata[train_index,] #randomly select the data for training set using the row numbers generated above
test=mydata[-train_index,]#everything not in the training set should go into testing set

y_train=train$myresponse
y_test=as.data.frame(test$myresponse)


pred_train=train
pred_test=test
pred_train$myresponse=NULL
pred_test$myresponse=NULL


dim(pred_train) #confirms that testing data has only 20% of observations
dim(pred_test) #confirms that training data has 80% of observations

#END DATA BREAKDOWN FOR HOLDOUT METHOD

#START KNN

#Note, that below I am capping the maximum value of K to be 20.
#More specifically, if the training data is 200 observations or less then the
#maximum possible value for K is going to be equal to the 10% of the dimension
#of the training data. Otherwise, the highest value it is going to be 20. 

rate=matrix(0,100,min(round(dim(train)[1]/10),20))#initialize
percent_correct=c(1:min(round(dim(train)[1]/10),20))#initialize

#Note, that since in "knn" function the ties are broken at random,
#running the function multiple times may result in slightly different result
#for each value of K. For that reason, for each K I am running the function 100
#times and averageing the results.

for (i in 1:100){
  for (j in 1:min(round(dim(train)[1]/10),20)){
    nearest=knn(train=pred_train, test=pred_test, cl=y_train, k=j)
    rate[i,j]=100*sum(nearest==as.character(y_test[,1]))/dim(y_test)[1]
  }
}


for (i in 1:length(percent_correct)){
  percent_correct[i]=mean(rate[,i])
}

percent_correct

#END KNN



#############################################################################################
############################SPECIFICATION OF THE FINAL KNN###################################
#############################################################################################

#You will need to choose the value for K that resulted in the highest pecentage of correct
#classifications for the test data set, as based on "percent_correct" list. That value of
#K needs to be subsequently passed to function KNN below to display the best classification
#That value of K needs to be substituted for "15" in the last argument in the code below.

#START FINAL KNN RUN FOR THE BEST VALUE OF K

nearest_final=knn(train=pred_train, test=pred_test, cl=y_train, k=1)

#############################################################################################
############################DO NOT MODIFY BEYOND THIS POINT##################################
#############################################################################################


percent_correct=100*sum(as.character(nearest_final)==as.character(y_test[,1]))/dim(y_test)[1]

#END FINAL KNN RUN FOR THE BEST VALUE OF K

#The code below calculates the performance of a simple classificaiton, which just assigns 
#each observation from the test set to the class that is dominating the training set.
#We then compare the performance of the KNN with best K (as found above) to this simple
#classification, to find out how KNN compares to this "naive" benchmark.

#START BENCHMARKING COMPARISON. Confirming that the simple classification
#rule yields a correct classification rate equal to the perentage of the 
#dominant class in the entire population.

prop_train = as.data.frame(prop.table(table(train$myresponse)))
prop_train=prop_train[order(-prop_train$Freq),]
dominant_class=prop_train[1,1]
test_benchmark=test
test_benchmark$simple_classification=as.character(dominant_class)
percent_correct_simple=100*sum(test_benchmark$simple_classification==as.character(y_test[,1]))/dim(y_test)[1]

#END BENCHMARKING COMPARISON

print(paste("Percentage of Correct Classifications for 'Best' KNN is:",percent_correct, "percent")) 
print(paste("Percentage of Correct Classifications for the Benchmark Classification is:",percent_correct_simple, "percent"))             

#START EXPORTING KNN CLASSIFICATIONS (WITH "BEST" VALUE OF K) TO A CSV FILE


colnames(y_test)="myresponse"
table_for_export=cbind(raw_for_export[-train_index,],nearest_final)
table_for_export$knn_classification=table_for_export$nearest_final
table_for_export$nearest_final=NULL


#Getting the confusion matrix below  
Confusion_Matrix = CrossTable(table_for_export$myresponse, table_for_export$knn_classification,dnn=c("True Class","Predicted Class"), prop.chisq=F,prop.t=F, prop.c=F, prop.r=F)

write.csv(table_for_export,paste(report_path, proj_name, ".csv",sep=""), row.names=F)

#END EXPORTING KNN CLASSIFICATIONS (WITH "BEST" VALUE OF K) TO A CSV FILE

#############################################################################################
##############################THIS IS THE END OF THE MACRO###################################
#############################################################################################

names (mydata)
