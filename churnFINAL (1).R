###### Data preparation - variable selection ##################
rm(list = ls())
Sys.setenv(LANG = "en")

# necessary packages
library("leaps")
library("randomForest")
library("klaR")
library("Hmisc")
library("caret")

# load the data
d = read.csv("C:\\Users\\Kalyanbrata\\Desktop\\churn_analysis\\newchurn\\Churn Data.csv",stringsAsFactors = FALSE)

vars <- read.csv("C:\\Users\\Kalyanbrata\\Desktop\\churn_analysis\\newchurn\\Variables_V2.csv", sep=";", stringsAsFactors=FALSE)

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


dnew <- dnew[, -173]  #Delete the customer id 

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
write.csv2(dataWithoutNas, file = "C:\\Users\\Kalyanbrata\\Desktop\\churn_analysis\\newchurn\\DataWithoutNAs.csv", row.names = FALSE)
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

write.csv2(summaryImportance, file = "~/Google Drive/BADS_SWT/Missing_and_Variable_Selection/SummaryImportanceRandomForestWcsaCities3.csv", row.names = TRUE)
write.csv2(predictions, file = "~/Google Drive/BADS_SWT/Missing_and_Variable_Selection/SummaryImportanceRandomForestWcsaCitiesPredictions3.csv", row.names = TRUE)
write.csv2(RFConfusion, file = "~/Google Drive/BADS_SWT/Missing_and_Variable_Selection/SummaryImportanceRandomForestWcsaCitiesConfusionMatrix3.csv", row.names = TRUE)
write.csv2(errorRate, file = "~/Google Drive/BADS_SWT/Missing_and_Variable_Selection/SummaryImportanceRandomForestWcsaCitiesErrorRate3.csv", row.names = TRUE)


#### END OF VARIABLE SELECTION ####

##########################################
######## SUBSET DATAFRAMES TO CONTAIN JUST THE SELECTED VARIABLES ################
##########################################


trainDataForModels <- subset(trainData, select = c(Customer_ID, churn, eqpdays, months, mou_Mean, totmrc_Mean,last_swap,
                                                   hnd_price,adjrev, change_mou, mou_cvce_Mean, avg3mou,
                                                   totrev,mou_Range, mou_opkv_Mean,totcalls,phones, avg3qty,
                                                   complete_Mean, peak_vce_Mean,comp_vce_Mean, totmou, opk_vce_Mean,
                                                   mou_peav_Mean,adjqty, avgqty,adjmou,rev_Mean, ovrmou_Range,
                                                   rev_Range, ovrmou_Mean, plcd_vce_Mean, csa))

testDataForModels <- subset(testData, select = c(Customer_ID, churn, eqpdays, months, mou_Mean, totmrc_Mean,last_swap,
                                                 hnd_price,adjrev, change_mou, mou_cvce_Mean, avg3mou,
                                                 totrev,mou_Range, mou_opkv_Mean,totcalls,phones, avg3qty,
                                                 complete_Mean, peak_vce_Mean,comp_vce_Mean, totmou, opk_vce_Mean,
                                                 mou_peav_Mean,adjqty, avgqty,adjmou,rev_Mean, ovrmou_Range,
                                                 rev_Range, ovrmou_Mean, plcd_vce_Mean, csa))


summary(trainDataForModels$hnd_price)

write.csv2(trainDataForModels, file = "~/Google Drive/BADS_SWT/TrainingDataSet.csv", row.names = FALSE)
write.csv2(testDataForModels, file = "~/Google Drive/BADS_SWT/TestDataSet.csv", row.names = FALSE)





#-------------------------------------------------------------------------------

# Check with Hmisc Package
# corrCalculationsHmisc <-  rcorr(as.matrix(dnewNumeric), type = "pearson") 
# corrMatrix <- as.data.frame(corrCalculationsHmisc$r) # get correlation matrix
# corrCalculationsHmisc  <- corrMatrix[,"churnNumeric"] # get just the correlations between churn and all the other variables
# sort(corrCalculationsHmisc, decreasing = TRUE)
# amountObservationsCorrelations <- as.data.frame(corrCalculationsHmisc$n) # get the number of observations used in every case
# churnAmountObservations <- amountObservationsCorrelations[,"churnNumeric"]

















# Delete all Variables with Missing Values >= 50%

#five     <- missing_var[missing_var>=0.5]  # Contains variables where more than 50% are missing
#length(five) #22 Variables with at least 50% NAs


#idx <- missing_var < 0.5  #Index for variables that have more than 50% of observations
#dnew2 <- dnew[,idx]  # Dataset without Variables having more thant 50% Missings 


#-------------------------------------------------------------------------------
#Check for horizontal NAs, i.e. NAs per individual (DELETE IN NEXT SECTION)

#Create data set to count NAs faster but use information to subset 'dnew'
# dnew_hoz <- dnew2

#Coerce 'Missing' in factors to NA
# factors <- which(sapply(dnew_hoz, is.factor))  #Find position of factors

# for(i in factors){

#   miss_idx <- levels(dnew_hoz[,i]) == "Missing"  #Find where 'Missing' is

#If 'Missing' is contained, coerce to NA
#  if(sum(miss_idx) == 1){
#   id <- which(miss_idx)  #Position of 'Missing'
#  levels(dnew_hoz[,i])[id] <- NA  #Coerce to NA
#}

# }

#Count horizontal missings
# n     <- nrow(dnew_hoz)  #Number of individuals
# n_var <- ncol(dnew_hoz)  #Number of variables
# missing_ind <- rep(NA, n)  #Vector to safe result
# for(i in 1:n){

# num_na         <- sum(is.na(dnew_hoz[i,]))  #Sum of NAs
# missing_ind[i] <- num_na/n_var  #Share of NAs
# }


#------------------------------------------------------------------------------------
# Delete Observations with too many Missing Values

# ten <- missing_ind[missing_ind>=0.1]
# length(ten)  #1081 individuals with at least 10% NAs

# idx <- missing_ind < 0.1
# dnew3 <- dnew2[idx,]  # Dataset without Observations with at least 10% Missings 


#-------------------------------------------------------------------------------------
# Dataset without Missing Values in Continuous Variables

# data <- na.omit(dnew3)
