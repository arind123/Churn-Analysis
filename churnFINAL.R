###### Data preparation - variable selection ##################
rm(list = ls())
Sys.setenv(LANG = "en")

# necessary packages
library("leaps")
library("randomForest")
library("klaR")
library("Hmisc")
library("caret")
library("gbm")

# load the data
d = read.csv("C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\Churn Data.csv",stringsAsFactors = FALSE)

vars <- read.csv("C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\Variables_V2.csv", sep=";", stringsAsFactors=FALSE)

#Store data to ensure original set is not changed
dnew <- d

#-------------------------------------------------------------------------------
#Convert variable types of data to type in Excel Description and 
#Merge blanks and NAs of factor variables to level "Missing"

for(i in 1:ncol(dnew)){
  
  var  <- colnames(dnew)[i]  #Store variable name
  
  #Find position of 'var' in 'vars'
  ind  <- which(vars$Variable == colnames(dnew)[i])  #Position  
  type <- vars[ind,2]  #Get variable type
  
  #Ensure continuous variables are class numeric
  if(type == "Continuous")  dnew[,i] <- as.numeric(dnew[,i])
  
  #Ensure Categorical variables are class factor
  if(type == "Categorical"){
    
    #Convert to factor and keep NAs as level
    dnew[,i] <- factor(dnew[,i], exclude = NULL)
    
    #Set <NA> level as "Missing" (converts values as well)
    if(is.na(levels(dnew[,i])[length(levels(dnew[,i]))])){
      
      levels(dnew[,i])[length(levels(dnew[,i]))] <- "Missing"
    }
    
    #Merge blank level and <NA> level as jointly "Missing"
    if(levels(dnew[,i])[1] == "") levels(dnew[,i])[1] <- "Missing"
    
    
  }
}


#dnew <- dnew[, -173]  #Delete the customer id 

#Handle factor label 'Unknown' manually
levels(dnew$dualband)[4] <- "Missing"
levels(dnew$ethnic)[16]  <- "Missing"
levels(dnew$kid0_2)[2]   <- "Missing"
levels(dnew$kid3_5)[2]   <- "Missing"
levels(dnew$kid6_10)[2]  <- "Missing"
levels(dnew$kid11_15)[2] <- "Missing"
levels(dnew$kid16_17)[2] <- "Missing"
levels(dnew$marital)[6]  <- "Missing"
levels(dnew$new_cell)[2] <- "Missing"

#create binary age1/age2 variable and to data - to not miss information of people
age1_bool <- factor(dnew$age1, exclude = NULL)
levels(age1_bool)[length(levels(age1_bool))] <- 0
levels(age1_bool)[levels(age1_bool) != 0] <- 1

age2_bool <- factor(dnew$age2, exclude = NULL)
levels(age2_bool)[length(levels(age2_bool))] <- 0
levels(age2_bool)[levels(age2_bool) != 0] <- 1


dnew$age1_bool <- age1_bool  #Add age1_bool to data
dnew$age2_bool <- age2_bool  #Add age2_bool to data

# Age variables - bin it in ca. 10 year intervals + "Missing". Note zeros are labeled as "Missing"

agebreaks <- c(0,30,40,50,60,max(dnew$age1, dnew$age2, na.rm = TRUE))

dnew$age1 <- cut(dnew$age1, breaks = agebreaks)
levels(dnew$age1) <- c(levels(dnew$age1), "Missing")
dnew$age1[is.na(dnew$age1)] <- "Missing"

dnew$age2 <- cut(dnew$age2, breaks = agebreaks)
levels(dnew$age2) <- c(levels(dnew$age2), "Missing")
dnew$age2[is.na(dnew$age2)] <- "Missing"


#Convert last_swap to time difference (is given as date)
swap_char                    <- as.character(dnew$last_swap) 
swap_date                    <- as.Date(swap_char[swap_char != "Missing"],
                                        format = "%m/%d/%Y")
swap_diff                    <- as.Date("2010-01-01") - swap_date
swap                         <- swap_char 
swap[swap_char != "Missing"] <- swap_diff
swap_numeric                 <- as.numeric(swap)

#dnew$swap_numeric            swap_numeric introduces too many NAs - just keep the factor variable that is why we did the below for last_swap variable

cor(dnew$totrev, as.numeric(dnew$churn), use = "pairwise.complete.obs")
summary(dnew$eqpdays)

#Bin original swap and take as factor (replace original variable)
swap_breaks            <- quantile(na.omit(swap_numeric), c(0, 0.25, 0.5, 0.75, 1))
swap_factor            <- factor(cut(swap_numeric, breaks = swap_breaks), exclude = NULL)
levels(swap_factor)[5] <- "No Swap" 
dnew$last_swap         <- swap_factor



#ref_qty to binary: Referral or no Referral
levels(dnew$REF_QTY)[length(levels(dnew$REF_QTY))] <- 0  #'Missing' indicates no referral

ref_qty_bool <- dnew$REF_QTY
levels(ref_qty_bool)[levels(ref_qty_bool) != "0"] <- "Y"
dnew$REF_QTY_bool <- ref_qty_bool

#Create binary variable for retention call days
retdays_bool <- factor(dnew$retdays, exclude = NULL)
levels(retdays_bool)[length(levels(retdays_bool))] <- "Not Called"
levels(retdays_bool)[levels(retdays_bool) != "Not Called"] <- "Called"
# add it to the dnew dataframe
dnew$retdays_bool <- retdays_bool

# Binning of retdays to avoid having so many missing values
#Bin original swap and take as factor (replace original variable)
retdays_numeric <- dnew$retdays


retdays_breaks <- quantile(na.omit(retdays_numeric), c(0, 0.25, 0.5, 0.75, 1))
retdays_factor <- factor(cut(retdays_numeric, breaks = retdays_breaks), exclude = NULL)
levels(retdays_factor) <- c(levels(retdays_factor), "Missing")
retdays_factor[is.na(retdays_factor)] <- "Missing"
dnew$retdays_factor         <- retdays_factor


# Redefine label "Missing" for variables total calls and total accepted offers from retention team that replaces missing by no call (to differentiate from the ones who we know that really didn't accept)
# Total calls
levels(dnew$tot_ret)[levels(dnew$tot_ret) == "Missing"] <- "Not Called"

# Total accepted offers
levels(dnew$tot_acpt)[levels(dnew$tot_acpt) == "Missing"] <- "Not Called"

# Create a numeric version of churn
dnew$churnNumeric <- as.numeric(dnew$churn) - 1

# Create dummies for rmcalls = NA, rmmou = NA, rmrev = NA 
dnew$rmcallsNA <- is.na(dnew$rmcalls)

dnew$rmmouNA <- is.na(dnew$rmmou)

dnew$rmrevNA <- is.na(dnew$rmrev)

# deal with csa - It denotes different cities in combination with area codes

dnew$csaCities <- factor(substr(dnew$csa, 1, 3)) 
levels(dnew$csaCities)[levels(dnew$csaCities) == "Mis"] <- "Missing"
levels(dnew$csaCities)[levels(dnew$csaCities) == "SLU" |
                         levels(dnew$csaCities) == "LAW" |
                         levels(dnew$csaCities) == "HOP" |
                         levels(dnew$csaCities) == "INU" |
                         levels(dnew$csaCities) == "SFU"] <- "Missing"


nlevels(dnew$csaCities)
summary(as.factor(dnew$csaCities))

# Total amount of levels 58 - we need max 56 to be able to perform the random forest approach. There were
# 3 cities with just one observation - I changed them to be missing


# Split info into two data frames with i) all categorical variables and ii) all numeric variables

is_factor <- sapply(dnew, is.factor) # Logical vector that tells which variables are categorical
is_numeric <- sapply(dnew, is.numeric)
dnewCategorical <- dnew[, is_factor] # Data frame just with categorical variables
dnewNumeric <- dnew[, is_numeric]





#-------------------------------------------------------------------------------
#Count function to count 'Missing' in Factors and NAs in numericals



count<- function(x){
  
  if(class(x) == "factor"){ 
    z <- sum(x == "Missing")/length(x)
    return(z)
  }
  
  if(class(x) == "numeric"){
    z <- 1-length(na.omit(x))/length(x)
    return(z)
  }
  
}

#-------------------------------------------------------------------------------
#Compute proportion of missing values for all variables

missing_var <- sapply(dnew, count)
missing_var_categorical <- sapply(dnewCategorical, count)
missing_var_numeric <- sapply(dnewNumeric, count)

sort(missing_var_categorical, decreasing = TRUE) # check percentages 
sort(missing_var_numeric, decreasing = TRUE)

# Idea behind this different check division into categorical and numeric: NAs ("Missing") in Categorical variables
# can be used as a category, whereas for numeric variables they truly are NAs (some models cannot like 
# random forests cannot run with NAs, so we want to minimize them)


# After checking the percentages for the numeric variables We eliminate retdays, rmcalls, rmmou 
# The rest of the variables have less than 3% NAs this shouldn't change dramatically the results

dnew2 <- subset(dnew, select = -c(retdays, rmcalls, rmmou, rmrev))

summary(dnew2$hnd_price)



############################################################
#### Data Partition into Training and test set #############
############################################################

set.seed(13)
# Division in two uses 70% for training and 30% for testint
ind <- createDataPartition(dnew2$churn, p = .7, list = FALSE)


trainData  <- dnew2[ind,]
testData <- dnew2[-ind,]



summary(trainData$churn)
class(trainData$churn)
################## END CROSS-VALIDATION CREATION OF DATA SETS ######

#-------------------------------------------------------------------------------
######### VARIABLE SELECTION ########
#-------------------------------------------------------------------------------




############################
# 1. WOE Analysis for categorical variables ####
##########################


# Re do the split of the data frames
is_factor2 <- sapply(trainData, is.factor) # Logical vector that tells which variables are categorical
is_numeric2 <- sapply(trainData, is.numeric)

dnewCategorical <- trainData[, is_factor2] # Data frame just with categorical variables
dnewNumeric <- trainData[, is_numeric2]



# Calculation of the importance score through WOE

varimp.woe <- woe(churn~., data = dnewCategorical, zeroadj = TRUE)
infoValue <- varimp.woe$IV
plot(varimp.woe)
sort(infoValue, decreasing = TRUE)

categoricalVariablesToSelect <- infoValue[infoValue>=.02]
sort(categoricalVariablesToSelect, decreasing = TRUE)
write.csv2(sort(categoricalVariablesToSelect, decreasing = TRUE),file = "C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\categoricalVariablesToSelectfromWOE.csv")

#############################

# Correlation Analysis for numeric variables ####

#######################ää

#  NOTE on the option use of cor : "pairwise.complete.obs" the correlation or covariance between each pair of variables is computed using all complete pairs of observations on those variables
# Hmisc calculates per default the option "pairwise.complete.obs" and one can get the amount of observations that were used (not NAs)

corrMatrix <- cor(dnewNumeric, use= "pairwise.complete.obs", method = "pearson")
churnCorrelations  <- corrMatrix[,"churnNumeric"] # get just the correlations between churn and all the other variables
sort(churnCorrelations, decreasing = TRUE)
sort(abs(churnCorrelations),decreasing = TRUE)


numericalVariablesToSelect <- churnCorrelations[abs(churnCorrelations)>=.03] # arbitrary cut at correlation .03 (not really meaningful)
numericalVariablesToSelect <- na.omit(numericalVariablesToSelect) # eliminate NAs 

# Correlation of Retained Numerical Variables
corrRetainedVars <- cor(dnewNumeric[,names(numericalVariablesToSelect)],
                        use = "pairwise.complete.obs", method = "pearson")


######
# Create vector of names from retained variables

namesOfRetainedVariables <- c(names(numericalVariablesToSelect ), names(categoricalVariablesToSelect))
write.csv2(namesOfRetainedVariables,file = "C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\selectedVariablesfromWOE.csv")
#######################################################################################
### the above variables are selected from all variable by selecting numerical vaiables through correlation analysis and the categorical variables through woe(weights of evidence)
#######################################################################################

#### DATAFRAME WITHOUT NA's

numberOfLevels <- sapply(dnewCategorical, nlevels) # doing because random forest cannot handle categorical variable greater than 53 levels or categories !!!!!!!!!!!!!
sort(numberOfLevels, decreasing = TRUE)
levels(dnewCategorical$crclscod) # 54 level a problem
levels(dnew$csa) # 799 levels we can remove it for randomForest because we already have csaCities

dataWithoutNas <- na.omit(trainData)
dataWithoutNas <- subset(dataWithoutNas, select = -c(churnNumeric))
dataWithoutNas <- subset(dataWithoutNas,select = -c(csa)) # dropping the csa


################ have a problem with crclscod have 54 levels ####################
nlevels(dataWithoutNas$crclscod)
sort(summary(dataWithoutNas$crclscod),decreasing = TRUE)
## clubbing the code S,V,ZF to one CLUBBED because they have only one obs.
levels(dataWithoutNas$crclscod)[levels(dataWithoutNas$crclscod) == "S"] <- "CLUBBED"
levels(dataWithoutNas$crclscod)[levels(dataWithoutNas$crclscod) == "V"] <- "CLUBBED"
levels(dataWithoutNas$crclscod)[levels(dataWithoutNas$crclscod) == "ZF"] <- "CLUBBED"

nlevels(dataWithoutNas$crclscod)
levels(dataWithoutNas$crclscod)
write.csv2(dataWithoutNas, file = "C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\DataWithoutNAs.csv", row.names = FALSE)
##########################################################################################
### Random Forest for variable selection  ################################################
##########################################################################################


rf <- randomForest(churn ~ . , data = dataWithoutNas, importance = TRUE) # importance (variable importance)
RFConfusion <- rf$confusion
errorRate <- rf$err.rate
summaryImportance <-  rf$importance
predictions <- rf$predictions
plot <- varImpPlot(rf)


# system.time(rf2 <- randomForest(churn ~ . , data = dataWithoutNas, importance = TRUE))

write.csv2(summaryImportance, file = "/home/sysadm/Desktop/newchurn/SummaryImportanceRandomForestWcsaCities3.csv", row.names = TRUE)
write.csv2(predictions, file = "/home/sysadm/Desktop/newchurn/SummaryImportanceRandomForestWcsaCitiesPredictions3.csv", row.names = TRUE)
write.csv2(RFConfusion, file = "/home/sysadm/Desktop/newchurn/SummaryImportanceRandomForestWcsaCitiesConfusionMatrix3.csv", row.names = TRUE)
write.csv2(errorRate, file = "/home/sysadm/Desktop/newchurn/SummaryImportanceRandomForestWcsaCitiesErrorRate3.csv", row.names = TRUE)

##########################################################################################
######### variable selection through GBM ################################################
############################################################################################
gbm_data=read.csv("/home/sysadm/Desktop/newchurn/Churn Data.csv", header = TRUE, sep = ",", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "",na.strings=c(""," ","NA"))
gbm_data<-subset(gbm_data,select = -c(Customer_ID))
gbm_data<-gbm_data[,-which(colMeans(is.na(gbm_data))>0.1)]
gbm_data<-gbm_data[complete.cases(gbm_data),]
library('gbm')
gbmFit <- gbm(churn ~ .,                                  # formula
              data = gbm_data,                             # full data set
              distribution = "bernoulli",                 # distribution of response
              n.trees = 500,                            # number of trees
              interaction.depth = 7,                     # depth of the trees
              shrinkage = 0.01,                          # shrinkage parameter
              cv.folds = 10,                             # k-fold cross validation
              n.cores = 2)                               # number of cores to use

gbmFit                                                   # Optimal GBM information
summary(gbmFit)                                          # Variable Importance Factor
write.csv(summary(gbmFit),file = "/home/sysadm/Desktop/newchurn/importantVariablesbyGBM.csv",row.names = TRUE)


#### END OF VARIABLE SELECTION ####

##########################################
######## SUBSET DATAFRAMES TO CONTAIN JUST THE SELECTED VARIABLES ################
##########################################


trainDataForModels <- subset(trainData, select = c( Customer_ID,churn, eqpdays, months, mou_Mean, totmrc_Mean,last_swap,
                                                   hnd_price,hnd_webcap,crclscod,retdays_factor,tot_acpt,retdays_bool,tot_ret,
                                                   adjrev, change_mou, mou_cvce_Mean, avg3mou,
                                                   totrev,mou_Range, mou_opkv_Mean,totcalls,phones, avg3qty,
                                                   complete_Mean, peak_vce_Mean,comp_vce_Mean, totmou, opk_vce_Mean,
                                                   mou_peav_Mean,adjqty, avgqty,adjmou,rev_Mean, ovrmou_Range,
                                                   rev_Range, ovrmou_Mean, plcd_vce_Mean))

testDataForModels <- subset(testData, select = c(Customer_ID,churn, eqpdays, months, mou_Mean, totmrc_Mean,last_swap,
                                                 hnd_price,hnd_webcap,crclscod,retdays_factor,tot_acpt,retdays_bool,tot_ret,
                                                 adjrev, change_mou, mou_cvce_Mean, avg3mou,
                                                 totrev,mou_Range, mou_opkv_Mean,totcalls,phones, avg3qty,
                                                 complete_Mean, peak_vce_Mean,comp_vce_Mean, totmou, opk_vce_Mean,
                                                 mou_peav_Mean,adjqty, avgqty,adjmou,rev_Mean, ovrmou_Range,
                                                 rev_Range, ovrmou_Mean, plcd_vce_Mean))


summary(trainDataForModels$hnd_price)

write.csv2(trainDataForModels, file = "C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\TrainingDataSet.csv", row.names = FALSE)
write.csv2(testDataForModels, file = "C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\TestDataSet.csv", row.names = FALSE)

###############################################################################
####################### MODELLING #############################################
###############################################################################

#train <-read.csv2("C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\TrainingDataSet.csv" )
#test<-read.csv2("C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\TestDataSet.csv")
data<-rbind(trainDataForModels,testDataForModels)
data$churn<-as.factor(data$churn)
data<-na.omit(data)
data<-data[,-1]

# generalize outcome and predictor variables
outcomeName <- 'churn'
predictorsNames <- names(data)[names(data) != "churn"]

################ have a problem with crclscod have 54 levels ####################
nlevels(data$crclscod)
sort(summary(data$crclscod),decreasing = TRUE)
## clubbing the code S,V,ZF to one CLUBBED because they have only one obs.
levels(data$crclscod)[levels(data$crclscod) == "S"] <- "CLUBBED"
levels(data$crclscod)[levels(data$crclscod) == "V"] <- "CLUBBED"
levels(data$crclscod)[levels(data$crclscod) == "ZF"] <- "CLUBBED"

nlevels(data$crclscod)
levels(data$crclscod)

feature.names=names(data)

for (f in feature.names) {
  if (class(data[[f]])=="factor") {
    levels <- unique(c(data[[f]]))
    data[[f]] <- factor(data[[f]],
                         labels=make.names(levels))
  }
}

# Split into Train and Validation sets
# Training Set : Validation Set = 1 : 1 (random)
set.seed(11)
tr <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
tra <- data[tr,]
Val <-data[-tr,]

# create caret trainControl object to control the number of cross-validations performed
objControl <- trainControl(method='cv', number=5, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

# run model
objModel <- train(tra[,predictorsNames], as.factor(tra[,outcomeName]), 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))
# find out model details
objModel
save(objModel,file = "C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\objGbMmodel.txt")

#################################################
# evalutate model
#################################################
# get predictions on your testing data

# class prediction
predictions1 <- predict(object=objModel, Val[,predictorsNames], type='raw')
head(predictions1)
postResample(pred=predictions1, obs=as.factor(Val[,outcomeName]))

# probabilities 
predictions2 <- predict(object=objModel, Val[,predictorsNames], type='prob')
head(predictions2)
postResample(pred=predictions2[[2]], obs=ifelse(Val[,outcomeName]=='X1',1,0))


##### Auc liftscore ###############################################

prediction<-predictions2[,2]
churn_num  = as.numeric(Val$churn) - 1
auc        = roc.area(churn_num, prediction)$A
auc  #[1] 0.6714297
pred       = prediction(prediction,Val$churn )
perf       = performance(pred, measure="lift", x.measure="rpp")
lift       = cbind(perf@x.values[[1]], perf@y.values[[1]])
lift_score = lift[which.min(abs(lift[,1] - 0.1)) ,2]
lift_score #[1] 0.4906835

# print customer id and predicted churn probablity
customerid <-read.csv("C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\customer_id.csv")
results = cbind.data.frame(customerid,predictions)
write.csv(results,file = "C:\\Users\\Kalyanbrata\\Desktop\\newchurn2\\churnprob37varGBM.csv",row.names = TRUE)














#######################################################################################
################### glmnet ##########################################################


data<-rbind(trainDataForModels,testDataForModels)
data$churn<-as.factor(data$churn)
data<-na.omit(data)
data<-data[,-1]


# generalize outcome and predictor variables
outcomeName <- 'churn'
predictorsNames <- names(data)[names(data) != "churn"]

################ have a problem with crclscod have 54 levels ####################
nlevels(data$crclscod)
sort(summary(data$crclscod),decreasing = TRUE)
## clubbing the code S,V,ZF to one CLUBBED because they have only one obs.
levels(data$crclscod)[levels(data$crclscod) == "S"] <- "CLUBBED"
levels(data$crclscod)[levels(data$crclscod) == "V"] <- "CLUBBED"
levels(data$crclscod)[levels(data$crclscod) == "ZF"] <- "CLUBBED"

nlevels(data$crclscod)
levels(data$crclscod)

feature.names=names(data)

for (f in feature.names) {
  if (class(data[[f]])=="factor") {
    levels <- unique(c(data[[f]]))
    data[[f]] <- factor(data[[f]],
                        labels=make.names(levels))
  }
}

# Split into Train and Validation sets
# Training Set : Validation Set = 1 : 1 (random)
set.seed(14)
tr <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
tra <- data[tr,]
Val <-data[-tr,]



#logFit<-glm(churn~ eqpdays+months+mou_Mean+crclscod+change_mou+totmrc_Mean+avgqty+rev_Range+mou_Range+mou_cvce_Mean+retdays_factor+ovrmou_Mean+tot_ret+tot_acpt, data=tra,family=binomial(link = 'logit'))

#testinglog<-subset(Val,select = c(eqpdays,months,mou_Mean,crclscod,change_mou,totmrc_Mean,avgqty,rev_Range,mou_Range,mou_cvce_Mean,retdays_factor,ovrmou_Mean,tot_ret,tot_acpt))

#prediction<-predict(logFit,type='response',newdata = testinglog)
logFit<-glm(churn~.,data = tra,family = binomial(link='logit'))

prediction<-predict(logFit,newdata=Val,type="response")












#############################################################################
#-----------------------------random forest-------------------------------------------------
#Function that is used in parallelized code
data<-rbind(trainDataForModels,testDataForModels)
data$churn<-as.factor(data$churn)
data<-na.omit(data)
data<-data[,-1]

# generalize outcome and predictor variables
outcomeName <- 'churn'
predictorsNames <- names(data)[names(data) != "churn"]

################ have a problem with crclscod have 54 levels ####################
nlevels(data$crclscod)
sort(summary(data$crclscod),decreasing = TRUE)
## clubbing the code S,V,ZF to one CLUBBED because they have only one obs.
levels(data$crclscod)[levels(data$crclscod) == "S"] <- "CLUBBED"
levels(data$crclscod)[levels(data$crclscod) == "V"] <- "CLUBBED"
levels(data$crclscod)[levels(data$crclscod) == "ZF"] <- "CLUBBED"

nlevels(data$crclscod)
levels(data$crclscod)

feature.names=names(data)

for (f in feature.names) {
  if (class(data[[f]])=="factor") {
    levels <- unique(c(data[[f]]))
    data[[f]] <- factor(data[[f]],
                        labels=make.names(levels))
  }
}

# Split into Train and Validation sets
# Training Set : Validation Set = 1 : 1 (random)
set.seed(11)
tr <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
tra <- data[tr,]
Val <-data[-tr,]






  
  library(foreign)
  library(randomForest)
  library(tree)
  library("AUC")
  library("party")
  library("gbm")
  library("dplyr")
  library("caret")
  library("verification")
  library("ROCR")
  
  
  fit        = randomForest(x = tra[,which(names(tra) != "churn")], 
                            y = tra$churn, 
                            ntree = 100) 
  
  prediction = predict(fit, newdata = Val[,which(names(tra) != "churn")], type="prob")[,2]
  
  
  churn_num  = as.numeric(Val$churn) - 1
  
  auc        = roc.area(churn_num, prediction)$A
  #0.63
  pred       = prediction(prediction,Val$churn )
  perf       = performance(pred, measure="lift", x.measure="rpp")
  lift       = cbind(perf@x.values[[1]], perf@y.values[[1]])
  lift_score = lift[which.min(abs(lift[,1] - 0.1)) ,2]
  # 0.59
###########################################################################    

 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
   #######################################################################################
  ############# svm
  data<-rbind(trainDataForModels,testDataForModels)
  data$churn<-as.factor(data$churn)
  data<-data[,-1]
  
  # generalize outcome and predictor variables
  outcomeName <- 'churn'
  predictorsNames <- names(data)[names(data) != "churn"]
  
  ################ have a problem with crclscod have 54 levels ####################
  nlevels(data$crclscod)
  sort(summary(data$crclscod),decreasing = TRUE)
  ## clubbing the code S,V,ZF to one CLUBBED because they have only one obs.
  levels(data$crclscod)[levels(data$crclscod) == "S"] <- "CLUBBED"
  levels(data$crclscod)[levels(data$crclscod) == "V"] <- "CLUBBED"
  levels(data$crclscod)[levels(data$crclscod) == "ZF"] <- "CLUBBED"
  
  nlevels(data$crclscod)
  levels(data$crclscod)
  
  feature.names=names(data)
  
  for (f in feature.names) {
    if (class(data[[f]])=="factor") {
      levels <- unique(c(data[[f]]))
      data[[f]] <- factor(data[[f]],
                          labels=make.names(levels))
    }
  }
  
  # Split into Train and Validation sets
  # Training Set : Validation Set = 1 : 1 (random)
  set.seed(11)
  tr <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
  tra <- data[tr,]
  Val <-data[-tr,]
  
  
  library("klaR")
  library("gmodels")
  library("AUC")
  
  
library(e1071)
 svm_model<-svm(churn~eqpdays+months+mou_Mean+crclscod+change_mou+totmrc_Mean+avgqty+rev_Range+mou_Range+mou_cvce_Mean+retdays_factor+ovrmou_Mean+tot_ret+tot_acpt,data = tra,family = binomial,probablity=TRUE)

 
## Make the predictions on the test data
 testingSVM <- subset(Val,select=c(eqpdays,months,mou_Mean,crclscod,change_mou,totmrc_Mean,avgqty,rev_Range,mou_Range,mou_cvce_Mean,retdays_factor,ovrmou_Mean,tot_ret,tot_acpt))
 
 prediction <- predict(svm_model, newdata = testingSVM, type="prob")
 prediction<-attr(prediction,"probabilities")[,1]
 churn_num  = as.numeric(Val$churn) - 1
 auc        = roc.area(churn_num, prediction)$A
 pred       = prediction(prediction,Val$churn )
 perf       = performance(pred, measure="lift", x.measure="rpp")
 lift       = cbind(perf@x.values[[1]], perf@y.values[[1]])
 lift_score = lift[which.min(abs(lift[,1] - 0.1)) ,2]
 
 
###############################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################################
  ####### naive bayes
  library("klaR")
  library("gmodels")
  library("AUC")
  
  
  
  data<-rbind(trainDataForModels,testDataForModels)
  data$churn<-as.factor(data$churn)
  data<-na.omit(data)
  
  
  # generalize outcome and predictor variables
  outcomeName <- 'churn'
  predictorsNames <- names(data)[names(data) != "churn"]
  
  ################ have a problem with crclscod have 54 levels ####################
  nlevels(data$crclscod)
  sort(summary(data$crclscod),decreasing = TRUE)
  ## clubbing the code S,V,ZF to one CLUBBED because they have only one obs.
  levels(data$crclscod)[levels(data$crclscod) == "S"] <- "CLUBBED"
  levels(data$crclscod)[levels(data$crclscod) == "V"] <- "CLUBBED"
  levels(data$crclscod)[levels(data$crclscod) == "ZF"] <- "CLUBBED"
  
  nlevels(data$crclscod)
  levels(data$crclscod)
  
  feature.names=names(data)
  
  for (f in feature.names) {
    if (class(data[[f]])=="factor") {
      levels <- unique(c(data[[f]]))
      data[[f]] <- factor(data[[f]],
                          labels=make.names(levels))
    }
  }
  
  # Split into Train and Validation sets
  # Training Set : Validation Set = 1 : 1 (random)
  set.seed(11)
  tr <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
  tra <- data[tr,]
  Val <-data[-tr,]
  
  testingNB<-subset(Val,select = c(eqpdays,months,mou_Mean,crclscod,change_mou,totmrc_Mean,avgqty,rev_Range,mou_Range,mou_cvce_Mean,retdays_factor,ovrmou_Mean,tot_ret,tot_acpt))
  
  
  NB <-NaiveBayes(churn~ eqpdays+months+mou_Mean+crclscod+change_mou+totmrc_Mean+avgqty+rev_Range+mou_Range+mou_cvce_Mean+retdays_factor+ovrmou_Mean+tot_ret+tot_acpt,data = tra)
  
  ## Make the predictions on the test data
  
  
  prediction <- predict(NB, newdata = testingNB,type="prob")
  prediction<-prediction$posterior[,2]
  churn_num  = as.numeric(Val$churn) - 1
  auc        = roc.area(churn_num, prediction)$A  # 0.58
  pred       = prediction(prediction,Val$churn )
  perf       = performance(pred, measure="lift", x.measure="rpp")
  lift       = cbind(perf@x.values[[1]], perf@y.values[[1]])
  lift_score = lift[which.min(abs(lift[,1] - 0.1)) ,2]   #0.79
  