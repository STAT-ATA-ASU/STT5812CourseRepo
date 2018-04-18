## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = NA, fig.align = "center", warning = FALSE)

## ---- message = FALSE, warning = FALSE-----------------------------------
library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)

## ------------------------------------------------------------------------
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)

## ------------------------------------------------------------------------
dim(train)
str(train[ ,c(1:10, 81)]) #display first 10 variables and the response variable

## ------------------------------------------------------------------------
# Getting rid of the IDs but keeping the test IDs in a vector. 
# They are needed to compose the submission file
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL

## ------------------------------------------------------------------------
test$SalePrice <- NA
all <- rbind(train, test)
dim(all)

## ---- label = "SP", message = FALSE, fig.cap = "Sale Price in US dollars"----
ggplot(data = all[!is.na(all$SalePrice), ], aes(x = SalePrice)) +
        geom_histogram(fill = "lightblue", color = "black", binwidth = 20000) +
        scale_x_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) + 
        theme_bw()

## ------------------------------------------------------------------------
summary(all$SalePrice)

## ------------------------------------------------------------------------
numericVars <- which(sapply(all, is.numeric))  # index vector numeric variables
numericVarNames <- names(numericVars)          # saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs") # correlations of all numeric variables

# sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[ ,'SalePrice'], decreasing = TRUE))
# select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(corr = cor_numVar, tl.pos = "lt", , tl.col = "black")

## ------------------------------------------------------------------------
ggplot(data = all[!is.na(all$SalePrice), ], aes(x = factor(OverallQual), y = SalePrice)) +
        geom_boxplot() + 
        labs(x='Overall Quality', y = "Sale Price (US dollars)") +
        scale_y_continuous(breaks= seq(0, 800000, by = 100000), labels = comma) + 
        theme_bw()  

## ---- message = FALSE----------------------------------------------------
ggplot(data = all[!is.na(all$SalePrice), ], aes(x = GrLivArea, y = SalePrice)) +
        geom_point(color = 'blue', alpha = 0.15) + 
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        geom_smooth(se = FALSE, color = "red") + 
        scale_y_continuous(breaks= seq(0, 800000, by = 100000), labels = comma) +
        geom_text_repel(aes(label = ifelse(GrLivArea > 4000, rownames(all), ""))) + 
        theme_bw() + 
        labs(x = "Living Area (square feet)", y = "Sale Price (US dollars)")

## ------------------------------------------------------------------------
all[c(524, 1299, 692, 1183), c('SalePrice', 'GrLivArea', 'OverallQual')]

## ------------------------------------------------------------------------
NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')

## ------------------------------------------------------------------------
all$PoolQC[is.na(all$PoolQC)] <- 'None'

## ------------------------------------------------------------------------
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

## ---- message = FALSE----------------------------------------------------
all$PoolQC <- as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)

## ------------------------------------------------------------------------
all[all$PoolArea > 0 & all$PoolQC == 0, c('PoolArea', 'PoolQC', 'OverallQual')]

all$PoolQC[2421] <- 2
all$PoolQC[2504] <- 3
all$PoolQC[2600] <- 2

## ------------------------------------------------------------------------
all$MiscFeature[is.na(all$MiscFeature)] <- 'None'
all$MiscFeature <- as.factor(all$MiscFeature)

ggplot(all[!is.na(all$SalePrice), ], aes(x = reorder(MiscFeature, SalePrice), y = SalePrice)) +
        geom_bar(stat ='summary', fun.y = "median", fill = 'lightblue') +
        scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..)) + 
        theme_bw() + 
        labs(y = "Median Sale Price (US dollars)")


table(all$MiscFeature)

## ------------------------------------------------------------------------
all$Alley[is.na(all$Alley)] <- 'None'
all$Alley <- as.factor(all$Alley)

ggplot(all[!is.na(all$SalePrice),], aes(x = Alley, y = SalePrice)) +
        geom_bar(stat='summary', fun.y = "median", fill = "wheat4") +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..)) + 
        scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma) + 
        theme_bw() + 
        labs(y = "Median Sale Price (US dollars)")

table(all$Alley)
#
xtabs(~Alley, data = all)

## ------------------------------------------------------------------------
all$Fence[is.na(all$Fence)] <- 'None'
table(all$Fence)

all[!is.na(all$SalePrice), ] %>% 
  group_by(Fence) %>% 
  summarise(median = median(SalePrice), counts = n())

## ------------------------------------------------------------------------
all$Fence <- as.factor(all$Fence)

## ------------------------------------------------------------------------
xtabs(~Fireplaces, data = all)

all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
all$FireplaceQu <-as.integer(revalue(all$FireplaceQu, Qualities))
table(all$FireplaceQu)

## ------------------------------------------------------------------------
table(all$Fireplaces)
sum(table(all$Fireplaces))

## ------------------------------------------------------------------------
ggplot(all[!is.na(all$LotFrontage),], aes(x = reorder(as.factor(Neighborhood), LotFrontage),  y = LotFrontage)) +
        geom_bar(stat = 'summary', fun.y = "median", fill = "peachpuff") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = "", y = "Lot Frontage (median linear feet\n of street connected to property)")

## ------------------------------------------------------------------------
for (i in 1:nrow(all)){
        if(is.na(all$LotFrontage[i])){
               all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood==all$Neighborhood[i]], na.rm = TRUE)) 
        }
}

## ------------------------------------------------------------------------
all$LotShape<-as.integer(revalue(all$LotShape, c('IR3' = 0, 'IR2' = 1, 'IR1' = 2, 'Reg' = 3)))
table(all$LotShape)
sum(table(all$LotShape))

## ------------------------------------------------------------------------
ggplot(all[!is.na(all$SalePrice),], aes(x = reorder(as.factor(LotConfig), SalePrice), y = SalePrice)) +
        geom_bar(stat = 'summary', fun.y = "median", fill = "plum")+
        scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..)) + 
        theme_bw()

## ------------------------------------------------------------------------
all$LotConfig <- as.factor(all$LotConfig)
table(all$LotConfig)
sum(table(all$LotConfig))

## ------------------------------------------------------------------------
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]

## ------------------------------------------------------------------------
#check if all 157 NAs are the same observations among the variables with 157/159 NAs
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))

#Find the 2 additional NAs
kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

## ------------------------------------------------------------------------
#Imputing modes.
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]

#display "fixed" house
kable(all[2127, c('GarageYrBlt', 'GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

## ------------------------------------------------------------------------
#fixing 3 values for house 2577
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA

#check if NAs of the character variables are now all 158
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))

## ------------------------------------------------------------------------
all$GarageType[is.na(all$GarageType)] <- 'No Garage'
all$GarageType <- as.factor(all$GarageType)
table(all$GarageType)

## ------------------------------------------------------------------------
all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
Finish <- c('None' = 0, 'Unf' = 1, 'RFn' = 2, 'Fin' = 3)

all$GarageFinish <- as.integer(revalue(all$GarageFinish, Finish))
table(all$GarageFinish)

## ------------------------------------------------------------------------
all$GarageQual[is.na(all$GarageQual)] <- 'None'
all$GarageQual <- as.integer(revalue(all$GarageQual, Qualities))
table(all$GarageQual)

## ------------------------------------------------------------------------
all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond<-as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)

## ------------------------------------------------------------------------
#check if all 79 NAs are the same observations among the variables with 80+ NAs
length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) & is.na(all$BsmtExposure) & is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))

#Find the additional NAs; BsmtFinType1 is the one with 79 NAs
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond)|is.na(all$BsmtQual)|is.na(all$BsmtExposure)|is.na(all$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

## ------------------------------------------------------------------------
#Imputing modes.
all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]

## ---- message=FALSE------------------------------------------------------
all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
all$BsmtQual<-as.integer(revalue(all$BsmtQual, Qualities))
table(all$BsmtQual)

## ---- message=FALSE------------------------------------------------------
all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtCond<-as.integer(revalue(all$BsmtCond, Qualities))
table(all$BsmtCond)

## ------------------------------------------------------------------------
all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
Exposure <- c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)

all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, Exposure))
table(all$BsmtExposure)

## ------------------------------------------------------------------------
all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
FinType <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)

all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, FinType))
table(all$BsmtFinType1)

## ------------------------------------------------------------------------
all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
FinType <- c('None' = 0, 'Unf' = 1, 'LwQ' = 2, 'Rec' = 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)

all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, FinType))
table(all$BsmtFinType2)

## ------------------------------------------------------------------------
#display remaining NAs. Using BsmtQual as a reference for the 79 houses without basement agreed upon earlier
all[(is.na(all$BsmtFullBath)|is.na(all$BsmtHalfBath)|is.na(all$BsmtFinSF1)|is.na(all$BsmtFinSF2)|is.na(all$BsmtUnfSF)|is.na(all$TotalBsmtSF)), c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]

## ------------------------------------------------------------------------
all$BsmtFullBath[is.na(all$BsmtFullBath)] <- 0
table(all$BsmtFullBath)

## ------------------------------------------------------------------------
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <- 0
table(all$BsmtHalfBath)

## ------------------------------------------------------------------------
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <- 0

## ------------------------------------------------------------------------
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <- 0

## ------------------------------------------------------------------------
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <- 0

## ------------------------------------------------------------------------
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <- 0

## ------------------------------------------------------------------------
#check if the 23 houses with veneer area NA are also NA in the veneer type
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))

#find the one that should have a MasVnrType
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]

## ------------------------------------------------------------------------
#fix this veneer type by imputing the mode
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2] #taking the 2nd value as the 1st is 'none'
all[2611, c('MasVnrType', 'MasVnrArea')]

## ------------------------------------------------------------------------
all$MasVnrType[is.na(all$MasVnrType)] <- 'None'

all[!is.na(all$SalePrice),] %>% group_by(MasVnrType) %>% summarise(median = median(SalePrice), counts=n()) %>% arrange(median)

## ------------------------------------------------------------------------
Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
all$MasVnrType<-as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)

## ------------------------------------------------------------------------
all$MasVnrArea[is.na(all$MasVnrArea)] <- 0

## ------------------------------------------------------------------------
#imputing the mode
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.factor(all$MSZoning)
table(all$MSZoning)
sum(table(all$MSZoning))

## ---- message=FALSE------------------------------------------------------
all$KitchenQual[is.na(all$KitchenQual)] <- 'TA' #replace with most common value
all$KitchenQual <- as.integer(revalue(all$KitchenQual, Qualities))
table(all$KitchenQual)
sum(table(all$KitchenQual))

## ------------------------------------------------------------------------
table(all$KitchenAbvGr)
sum(table(all$KitchenAbvGr))

## ---- message=FALSE------------------------------------------------------
table(all$Utilities)
kable(all[is.na(all$Utilities) | all$Utilities=='NoSeWa', 1:9])
all$Utilities <- NULL

## ---- message=FALSE------------------------------------------------------
#impute mode for the 1 NA
all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]

all$Functional <- as.integer(revalue(all$Functional, c('Sal' = 0, 'Sev' = 1, 'Maj2' = 2, 'Maj1' = 3, 'Mod' = 4, 'Min2' = 5, 'Min1' = 6, 'Typ' = 7)))
table(all$Functional)
sum(table(all$Functional))

## ------------------------------------------------------------------------
#imputing mode
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]

all$Exterior1st <- as.factor(all$Exterior1st)
table(all$Exterior1st)
sum(table(all$Exterior1st))

## ------------------------------------------------------------------------
#imputing mode
all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]

all$Exterior2nd <- as.factor(all$Exterior2nd)
table(all$Exterior2nd)
sum(table(all$Exterior2nd))

## ------------------------------------------------------------------------
all$ExterQual<-as.integer(revalue(all$ExterQual, Qualities))
table(all$ExterQual)
sum(table(all$ExterQual))

## ------------------------------------------------------------------------
all$ExterCond <- as.integer(revalue(all$ExterCond, Qualities))
table(all$ExterCond)
sum(table(all$ExterCond))

## ------------------------------------------------------------------------
#imputing mode
all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]

all$Electrical <- as.factor(all$Electrical)
table(all$Electrical)
sum(table(all$Electrical))

## ------------------------------------------------------------------------
#imputing mode
all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]

all$SaleType <- as.factor(all$SaleType)
table(all$SaleType)
sum(table(all$SaleType))

## ------------------------------------------------------------------------
all$SaleCondition <- as.factor(all$SaleCondition)
table(all$SaleCondition)
sum(table(all$SaleCondition))

## ------------------------------------------------------------------------
Charcol <- names(all[,sapply(all, is.character)])
Charcol
cat('There are', length(Charcol), 'remaining columns with character values')

## ------------------------------------------------------------------------
#No ordinality, so converting into factors
all$Foundation <- as.factor(all$Foundation)
table(all$Foundation)
sum(table(all$Foundation))

## ------------------------------------------------------------------------
#No ordinality, so converting into factors
all$Heating <- as.factor(all$Heating)
table(all$Heating)
sum(table(all$Heating))

## ------------------------------------------------------------------------
# making the variable ordinal using the Qualities vector
all$HeatingQC <- as.integer(revalue(all$HeatingQC, Qualities))
table(all$HeatingQC)
sum(table(all$HeatingQC))

## ------------------------------------------------------------------------
all$CentralAir <- as.integer(revalue(all$CentralAir, c('N' = 0, 'Y' = 1)))
table(all$CentralAir)
sum(table(all$CentralAir))

## ------------------------------------------------------------------------
#No ordinality, so converting into factors
all$RoofStyle <- as.factor(all$RoofStyle)
table(all$RoofStyle)
sum(table(all$RoofStyle))

## ------------------------------------------------------------------------
# No ordinality, so converting into factors
all$RoofMatl <- as.factor(all$RoofMatl)
table(all$RoofMatl)
sum(table(all$RoofMatl))

## ------------------------------------------------------------------------
#No ordinality, so converting into factors
all$LandContour <- as.factor(all$LandContour)
table(all$LandContour)
sum(table(all$LandContour))

## ------------------------------------------------------------------------
# Ordinal, so label encoding
all$LandSlope<-as.integer(revalue(all$LandSlope, c('Sev' = 0, 'Mod' = 1, 'Gtl' = 2)))
table(all$LandSlope)
sum(table(all$LandSlope))

## ------------------------------------------------------------------------
ggplot(all[!is.na(all$SalePrice),], aes(x = reorder(as.factor(BldgType), SalePrice), y = SalePrice)) +
        geom_bar(stat = 'summary', fun.y = "median", fill = 'green') +
        scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..)) + 
        theme_bw()

## ------------------------------------------------------------------------
# No ordinality, so converting into factors
all$BldgType <- as.factor(all$BldgType)
table(all$BldgType)
sum(table(all$BldgType))

## ------------------------------------------------------------------------
#No ordinality, so converting into factors
all$HouseStyle <- as.factor(all$HouseStyle)
table(all$HouseStyle)
sum(table(all$HouseStyle))

## ------------------------------------------------------------------------
#No ordinality, so converting into factors
all$Neighborhood <- as.factor(all$Neighborhood)
table(all$Neighborhood)
sum(table(all$Neighborhood))

## ------------------------------------------------------------------------
#No ordinality, so converting into factors
all$Condition1 <- as.factor(all$Condition1)
table(all$Condition1)
sum(table(all$Condition1))

## ------------------------------------------------------------------------
#No ordinality, so converting into factors
all$Condition2 <- as.factor(all$Condition2)
table(all$Condition2)
sum(table(all$Condition2))

## ------------------------------------------------------------------------
#Ordinal, so label encoding
all$Street <- as.integer(revalue(all$Street, c('Grvl' = 0, 'Pave' = 1)))
table(all$Street)
sum(table(all$Street))

## ------------------------------------------------------------------------
#Ordinal, so label encoding
all$PavedDrive <- as.integer(revalue(all$PavedDrive, c('N' = 0, 'P' = 1, 'Y' = 2)))
table(all$PavedDrive)
sum(table(all$PavedDrive))

## ------------------------------------------------------------------------
str(all$YrSold)
str(all$MoSold)
all$MoSold <- as.factor(all$MoSold)

## ---- fig.width = 10, fig.height = 5-------------------------------------
ys <- ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(YrSold), y = SalePrice)) +
        geom_bar(stat = 'summary', fun.y = "median", fill = 'lightblue')+
        scale_y_continuous(breaks = seq(0, 800000, by = 25000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3) +
        coord_cartesian(ylim = c(0, 200000)) +
        theme_bw() +
        labs(x = "Year Sold", y = "Median Sale Price (US dollars)") +
        geom_hline(yintercept = 163000, linetype = "dashed", color = "red") #dashed line is median SalePrice

ms <- ggplot(all[!is.na(all$SalePrice),], aes(x = MoSold, y = SalePrice)) +
        geom_bar(stat = 'summary', fun.y = "median", fill = 'lightblue')+
        scale_y_continuous(breaks = seq(0, 800000, by = 25000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3) +
        coord_cartesian(ylim = c(0, 200000)) +
        theme_bw() +
        labs(x = "Month Sold", y = "Median Sale Price (US dollars)") +
        geom_hline(yintercept = 163000, linetype="dashed", color = "red") #dashed line is median SalePrice

grid.arrange(ys, ms, widths=c(1, 2))

## ------------------------------------------------------------------------
str(all$MSSubClass)

all$MSSubClass <- as.factor(all$MSSubClass)
 #revalue for better readability
all$MSSubClass<-revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1.5 story unf', '50'='1.5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2.5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1.5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))
str(all$MSSubClass)

## ------------------------------------------------------------------------
numericVars <- which(sapply(all, is.numeric))  # index vector numeric variables
factorVars <- which(sapply(all, is.factor))    # index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categorical variables')

## ---- out.width = "100%"-------------------------------------------------
all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") # correlations of all numeric variables

# sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[ ,'SalePrice'], decreasing = TRUE))
# select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x){abs(x) > 0.5})))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

## ------------------------------------------------------------------------
set.seed(2018)
quick_RF <- randomForest(x = all[1:1460, -79], y = all$SalePrice[1:1460], ntree = 100, importance = TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[ , 1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE), ]

ggplot(imp_DF[1:20, ], aes(x = reorder(Variables, MSE), y = MSE, fill = MSE)) + 
  geom_bar(stat = 'identity') + 
  labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + 
  coord_flip() + 
  theme_bw() +
  theme(legend.position = "none")

## ---- warning=FALSE, message=FALSE, out.width="100%"---------------------
s1 <- ggplot(data= all, aes(x = GrLivArea)) +
        geom_density() + 
        labs(x = 'Square feet living area') + 
        theme_bw()
s2 <- ggplot(data=all, aes(x = as.factor(TotRmsAbvGrd))) +
        geom_histogram(stat = 'count') + 
        labs(x = 'Rooms above Ground') + 
        theme_bw()
s3 <- ggplot(data = all, aes(x = X1stFlrSF)) +
        geom_density() + 
        labs(x = 'Square feet first floor') + 
        theme_bw()
s4 <- ggplot(data = all, aes(x = X2ndFlrSF)) +
        geom_density() + 
        labs(x = 'Square feet second floor') + 
        theme_bw()
s5 <- ggplot(data = all, aes(x = TotalBsmtSF)) +
        geom_density() + 
        labs(x = 'Square feet basement') + 
        theme_bw()
s6 <- ggplot(data = all[all$LotArea < 100000, ], aes(x = LotArea)) +
        geom_density() + 
        labs(x = 'Square feet lot') + 
        theme_bw()
s7 <- ggplot(data = all, aes(x = LotFrontage)) +
        geom_density() + 
        labs(x = 'Linear feet lot frontage') + 
        theme_bw()
s8 <- ggplot(data = all, aes(x = LowQualFinSF)) +
        geom_histogram() + 
        labs(x = 'Low quality square feet 1st & 2nd') + 
        theme_bw()

# layout <- matrix(c(1,2,5,3,4,8,6,7),4,2,byrow=TRUE)
# multiplot(s1, s2, s3, s4, s5, s6, s7, s8, layout=layout)

grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, nrow = 4)

## ------------------------------------------------------------------------
cor(all$GrLivArea, (all$X1stFlrSF + all$X2ndFlrSF + all$LowQualFinSF))
head(all[all$LowQualFinSF > 0, c('GrLivArea', 'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF')])

## ---- warning = FALSE, out.width = "100%", fig.height = 6----------------
n1 <- ggplot(all[!is.na(all$SalePrice), ], aes(x = Neighborhood, y = SalePrice)) +
        geom_bar(stat = 'summary', fun.y = "median", fill='blue') +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 2) +
        geom_hline(yintercept = 163000, linetype="dashed", color = "red") #dashed line is median SalePrice
n2 <- ggplot(data = all, aes(x = Neighborhood)) +
        geom_histogram(stat = 'count') +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 2) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(n1, n2, nrow = 2)

## ---- warning = FALSE, message = FALSE, out.width = "100%"---------------
q1 <- ggplot(data=all, aes(x=as.factor(OverallQual))) +
        geom_histogram(stat='count') + 
        theme_bw()
q2 <- ggplot(data=all, aes(x=as.factor(ExterQual))) +
        geom_histogram(stat='count') + 
        theme_bw()
q3 <- ggplot(data=all, aes(x=as.factor(BsmtQual))) +
        geom_histogram(stat='count') + 
        theme_bw()
q4 <- ggplot(data=all, aes(x=as.factor(KitchenQual))) +
        geom_histogram(stat='count') + 
        theme_bw()
q5 <- ggplot(data=all, aes(x=as.factor(GarageQual))) +
        geom_histogram(stat='count') + 
        theme_bw()
q6 <- ggplot(data=all, aes(x=as.factor(FireplaceQu))) +
        geom_histogram(stat='count') + 
        theme_bw()
q7 <- ggplot(data=all, aes(x=as.factor(PoolQC))) +
        geom_histogram(stat='count') + 
        theme_bw()

# layout <- matrix(c(1,2,8,3,4,8,5,6,7),3,3,byrow=TRUE)
# multiplot(q1, q2, q3, q4, q5, q6, q7, layout=layout)

grid.arrange(q1, q2, q3, q4, q5, q6, q7, nrow = 3)

## ---- warning = FALSE, out.width = "100%", fig.height = 6----------------
ms1 <- ggplot(all[!is.na(all$SalePrice), ], aes(x = MSSubClass, y = SalePrice)) +
        geom_bar(stat = 'summary', fun.y = "median", fill = 'green') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 2) +
        geom_hline(yintercept = 163000, linetype = "dashed", color = "red") #dashed line is median SalePrice
ms2 <- ggplot(data = all, aes(x = MSSubClass)) +
        geom_histogram(stat = 'count')+
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 2) +
        theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(ms1, ms2)

## ------------------------------------------------------------------------
#correct error
all$GarageYrBlt[2593] <- 2007 #this must have been a typo. GarageYrBlt=2207, YearBuilt=2006, YearRemodAdd=2007.

## ---- warning=FALSE, message=FALSE, out.width="100%"---------------------
g1 <- ggplot(data=all[all$GarageCars !=0,], aes(x=GarageYrBlt)) +
        geom_histogram() + 
        theme_bw()
g2 <- ggplot(data=all, aes(x=as.factor(GarageCars))) +
        geom_histogram(stat='count') + 
        theme_bw()
g3 <- ggplot(data= all, aes(x=GarageArea)) +
        geom_density() + 
        theme_bw()
g4 <- ggplot(data=all, aes(x=as.factor(GarageCond))) +
        geom_histogram(stat='count') + 
        theme_bw()
g5 <- ggplot(data=all, aes(x=GarageType)) +
        geom_histogram(stat='count') + 
        theme_bw()
g6 <- ggplot(data=all, aes(x=as.factor(GarageQual))) +
        geom_histogram(stat='count') + 
        theme_bw()
g7 <- ggplot(data=all, aes(x=as.factor(GarageFinish))) +
        geom_histogram(stat='count') + 
        theme_bw()

layout <- matrix(c(1,5,5,2,3,8,6,4,7),3,3,byrow=TRUE)
multiplot(g1, g2, g3, g4, g5, g6, g7, layout=layout)


## ---- warning=FALSE, message=FALSE, out.width="100%"---------------------
b1 <- ggplot(data=all, aes(x=BsmtFinSF1)) +
        geom_histogram() + 
        labs(x='Type 1 finished square feet') + 
        theme_bw()
b2 <- ggplot(data=all, aes(x=BsmtFinSF2)) +
        geom_histogram() + 
        labs(x='Type 2 finished square feet') + 
        theme_bw()
b3 <- ggplot(data=all, aes(x=BsmtUnfSF)) +
        geom_histogram() + 
        labs(x='Unfinished square feet') + 
        theme_bw()
b4 <- ggplot(data=all, aes(x=as.factor(BsmtFinType1))) +
        geom_histogram(stat='count') + 
        labs(x='Rating of Type 1 finished area') + 
        theme_bw()
b5 <- ggplot(data=all, aes(x=as.factor(BsmtFinType2))) +
        geom_histogram(stat='count') + 
        labs(x='Rating of Type 2 finished area') + 
        theme_bw()
b6 <- ggplot(data=all, aes(x=as.factor(BsmtQual))) +
        geom_histogram(stat='count') + 
        labs(x='Height of the basement') + 
        theme_bw()
b7 <- ggplot(data=all, aes(x=as.factor(BsmtCond))) +
        geom_histogram(stat='count') + 
        labs(x='Rating of general condition') + 
        theme_bw()
b8 <- ggplot(data=all, aes(x=as.factor(BsmtExposure))) +
        geom_histogram(stat='count') + 
        labs(x='Walkout or garden level walls') + 
        theme_bw()

layout <- matrix(c(1,2,3,4,5,9,6,7,8),3,3,byrow=TRUE)
multiplot(b1, b2, b3, b4, b5, b6, b7, b8, layout=layout)


## ------------------------------------------------------------------------
all$TotBathrooms <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)

## ---- warning = FALSE----------------------------------------------------
tb1 <- ggplot(data = all[!is.na(all$SalePrice),], aes(x=as.factor(TotBathrooms), y = SalePrice))+
        geom_point(col = 'lightblue') + 
        geom_smooth(method = "lm", se = FALSE, color = "black", aes(group=1)) +
        scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) + 
        theme_bw()
tb2 <- ggplot(data = all, aes(x = as.factor(TotBathrooms))) +
        geom_histogram(stat = 'count', fill = "red") +
        theme_bw()
grid.arrange(tb1, tb2)

## ------------------------------------------------------------------------
all$Remod <- ifelse(all$YearBuilt == all$YearRemodAdd, 0, 1) # 0 = No Remodeling, 1 = Remodeling
all$Age <- as.numeric(all$YrSold)-all$YearRemodAdd

## ------------------------------------------------------------------------
ggplot(data=all[!is.na(all$SalePrice), ], aes(x = Age, y = SalePrice))+
        geom_point(col = 'lightblue') + 
        geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
        scale_y_continuous(breaks = seq(0, 800000, by = 100000), labels = comma) +
        theme_bw()

## ------------------------------------------------------------------------
cor(all$SalePrice[!is.na(all$SalePrice)], all$Age[!is.na(all$SalePrice)])

## ---- out.width="50%"----------------------------------------------------
ggplot(all[!is.na(all$SalePrice),], aes(x = as.factor(Remod), y = SalePrice)) +
        geom_bar(stat = 'summary', fun.y = "median", fill = 'lightblue') +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 6) +
        scale_y_continuous(breaks = seq(0, 800000, by = 50000), labels = comma) +
        theme_bw(base_size = 18) +
        geom_hline(yintercept = 163000, linetype = "dashed") #dashed line is median SalePrice

## ------------------------------------------------------------------------
all$IsNew <- ifelse(all$YrSold==all$YearBuilt, 1, 0)
table(all$IsNew)

## ------------------------------------------------------------------------
all$YrSold <- as.factor(all$YrSold) # the numeric version is now not needed anymore

## ------------------------------------------------------------------------
nb1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN = median), y = SalePrice)) + 
        geom_bar(stat = 'summary', fun.y = "median", fill = 'lightblue') + 
        labs(x = 'Neighborhood', y = 'Median SalePrice') +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks= seq(0, 800000, by = 50000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3) +
        geom_hline(yintercept = 163000, linetype = "dashed", color = "red") #dashed line is median SalePrice
nb2 <- ggplot(all[!is.na(all$SalePrice), ], aes(x = reorder(Neighborhood, SalePrice, FUN = mean), y = SalePrice)) + 
        geom_bar(stat = 'summary', fun.y = "mean", fill = 'lightblue') + 
        labs(x = 'Neighborhood', y = "Mean SalePrice") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
        geom_label(stat = "count", aes(label = ..count.., y = ..count..), size = 3) +
        geom_hline(yintercept = 163000, linetype = "dashed", color = "red") #dashed line is median SalePrice
grid.arrange(nb1, nb2)

## ------------------------------------------------------------------------
all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

## ------------------------------------------------------------------------
table(all$NeighRich)
sum(table(all$NeighRich))

## ------------------------------------------------------------------------
all$TotalSqFeet <- all$GrLivArea + all$TotalBsmtSF

## ------------------------------------------------------------------------
all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch

## ---- message = FALSE----------------------------------------------------
por1 <- ggplot(data = all, aes(x = TotalPorchSF)) +
        geom_histogram(fill = "brown") + 
        labs(x=' Total Porch square feet') + 
        theme_bw()
por2 <- ggplot(data = all, aes(x = WoodDeckSF)) +
        geom_histogram(fill = "brown") + 
        labs(x = 'Wood Deck square feet') + 
        theme_bw()
grid.arrange(por1, por2, nrow = 1)

## ------------------------------------------------------------------------
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

all <- all[ , !(names(all) %in% dropVars)]

## ------------------------------------------------------------------------
all <- all[-c(524, 1299), ]

## ------------------------------------------------------------------------
numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames was created before having done anything
numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- all[, names(all) %in% numericVarNames]

DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')

## ------------------------------------------------------------------------
for(i in 1:ncol(DFnumeric)){
        if (abs(skew(DFnumeric[, i])) > 0.8){
                DFnumeric[ , i] <- log(DFnumeric[ , i] +1)
        }
}

## ------------------------------------------------------------------------
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

## ------------------------------------------------------------------------
DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

## ------------------------------------------------------------------------
DFdummies <- as.data.frame(model.matrix(~. -1, data = DFfactors))
dim(DFdummies)

## ------------------------------------------------------------------------
#check if some values are absent in the test set
ZerocolTest <- which(colSums(DFdummies[1459:2917, ])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[ , -ZerocolTest] #removing predictors

## ------------------------------------------------------------------------
#check if some values are absent in the train set
ZerocolTrain <- which(colSums(DFdummies[1:1458, ])==0)
colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[ , -ZerocolTrain] #removing predictor

## ------------------------------------------------------------------------
fewOnes <- which(colSums(DFdummies[1:1458, ]) <10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[ , -fewOnes] #removing predictors
dim(DFdummies)

## ------------------------------------------------------------------------
combined <- cbind(DFnorm, DFdummies) #combining all (now numeric) predictors into one dataframe
dim(combined)

## ------------------------------------------------------------------------
skew(all$SalePrice)

## ------------------------------------------------------------------------
qqnorm(all$SalePrice)
qqline(all$SalePrice)

## ------------------------------------------------------------------------
all$SalePrice <- log(all$SalePrice) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(all$SalePrice)

## ------------------------------------------------------------------------
qqnorm(all$SalePrice)
qqline(all$SalePrice)

## ------------------------------------------------------------------------
train1 <- combined[!is.na(all$SalePrice), ]
test1 <- combined[is.na(all$SalePrice), ]
dim(train1)
dim(test1)
names(train1)
names(test1)

## ------------------------------------------------------------------------
set.seed(4)
my_control <-trainControl(method = "cv", number = 10)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by = 0.0005))

lasso_mod <- train(x = train1, 
                   y = all$SalePrice[!is.na(all$SalePrice)], 
                   method = 'glmnet', 
                   trControl = my_control, 
                   tuneGrid = lassoGrid) 
lasso_mod$bestTune
min(lasso_mod$results$RMSE)

## ------------------------------------------------------------------------
LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)

## ------------------------------------------------------------------------
label_train <- all$SalePrice[!is.na(all$SalePrice)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train1), label = label_train)
dtest <- xgb.DMatrix(data = as.matrix(test1))

## ------------------------------------------------------------------------
default_param<-list(
        objective = "reg:linear",
        booster = "gbtree",
        eta = 0.1, #default = 0.3
        gamma = 0,
        max_depth = 6,
        min_child_weight = 1,
        subsample = 1,
        colsample_bytree = 1,
        seed = 2018
)

## ------------------------------------------------------------------------
xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)

## ------------------------------------------------------------------------
#train the model using the best iteration found by cross validation
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 139)

## ------------------------------------------------------------------------
XGBpred <- predict(xgb_mod, dtest)
predictions_XGB <- exp(XGBpred) #need to reverse the log to the real values
head(predictions_XGB)

## ---- out.width="100%"---------------------------------------------------
#view variable importance plot
library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance (feature_names = colnames(train1),model = xgb_mod)
xgb.ggplot.importance(importance_matrix = mat[1:20], rel_to_first = TRUE) 

## ------------------------------------------------------------------------
fitControl <- trainControl(method = "cv", number = 10)
set.seed(52)
mtry <- sqrt(dim(train1)[1])
gridRF <- expand.grid(.mtry = mtry)

mod_RF <- train(x = train1, 
                   y = all$SalePrice[!is.na(all$SalePrice)], 
                   method = 'rf', 
                   trControl = fitControl, 
                   tuneGrid = gridRF) 

mod_RF
mod_RF$bestTune
min(mod_RF$results$RMSE)

## ------------------------------------------------------------------------
RFpred <- predict(mod_RF, test1)
predictions_RF <- exp(RFpred) #need to reverse the log to the real values
head(predictions_RF)

## ------------------------------------------------------------------------
sub_avg <- data.frame(Id = test_labels, SalePrice = (0.40*predictions_XGB + 0.60*predictions_lasso))
head(sub_avg)
write.csv(sub_avg, file = 'average.csv', row.names = F)

