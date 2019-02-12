#
# Automobile Price Prediction
# 
# Assumptions
# Data Set File   - "CarPrice_Assignment.csv" is available in the working directory
#

#--------------------------------------------------------------------------------------------------#

#
# Load required libraries
#
library(MASS)
library(car)


#
# Load & view the Data Set
#
DataSetFile <- "CarPrice_Assignment.csv"

if (file.exists(DataSetFile)) {
  CarPrice <- read.csv(DataSetFile, stringsAsFactors = FALSE)
  cat("Info  : Data Set file \"",DataSetFile,"\" successfully loaded\n")
  View(CarPrice)
} else {
  cat("Error : Unable to access Data Set csv file -",DataSetFile,"\n")
  cat("Info  : Please verify the working directory and set as per Data Set file location\n")
}

#--------------------------------------------------------------------------------------------------#

#
# Data Cleaning and Manipulation
#


#
# Understanding the data and its structure available for analysis
cat("Info  : Total number of records available for analysis = ",nrow(CarPrice),"\n")
cat("Info  : Total number of variables - dependent and independent = ",ncol(CarPrice),"\n")

cat("Info  : Structure of the data available for analysis is as follows :\n")
str(CarPrice)


#
# Column - CarName
# CarName which is comprised of two parts - the first word is the name of 'car company' and the 
# second is the 'car model'. 
# Only Company name as the independent variable for the model building is to be considered
# CarName column should be manipulated to retain only car company name
summary(as.factor(CarPrice$CarName))

# Extract car company name from CarName
CarPrice$CarCompany <- gsub("\\ .*", "", CarPrice$CarName)
str(CarPrice$CarCompany)          # Its a Char variable
summary(as.factor(CarPrice$CarCompany))

# Mulitple entries are present for same car company with difference spellings. 
# All such car companies should be consolidated

# Combine "maxda" & "mazda" as "mazda"
CarPrice$CarCompany <- gsub("maxda", "mazda", CarPrice$CarCompany)
# Combine "nissan" & "Nissan" as "nissan"
CarPrice$CarCompany <- gsub("Nissan", "nissan", CarPrice$CarCompany)
# Combine "porcshce" & "porsche" as "porsche"
CarPrice$CarCompany <- gsub("porcshce", "porsche", CarPrice$CarCompany)
# Combine "toyota" & "toyouta" as "toyota"
CarPrice$CarCompany <- gsub("toyouta", "toyota", CarPrice$CarCompany)
# Combine "vokswagen", "volkswagen" & "vw" as "volkswagen"
CarPrice$CarCompany <- gsub("vokswagen", "volkswagen", CarPrice$CarCompany)
CarPrice$CarCompany <- gsub("vw", "volkswagen", CarPrice$CarCompany)

# Convert CarCompany to factor as its a categorical variable
CarPrice$CarCompany <- as.factor(CarPrice$CarCompany)
str(CarPrice$CarCompany)      # Cars reduced to 22 levels based on company
summary(CarPrice$CarCompany)



#
# Identify columns which are categorical and convert as factors
CarPrice$symboling      <- as.factor(CarPrice$symboling)
CarPrice$fueltype       <- as.factor(CarPrice$fueltype)
CarPrice$aspiration     <- as.factor(CarPrice$aspiration)
CarPrice$doornumber     <- as.factor(CarPrice$doornumber)
CarPrice$carbody        <- as.factor(CarPrice$carbody)
CarPrice$drivewheel     <- as.factor(CarPrice$drivewheel)
CarPrice$enginelocation <- as.factor(CarPrice$enginelocation)
CarPrice$enginetype     <- as.factor(CarPrice$enginetype)
CarPrice$cylindernumber <- as.factor(CarPrice$cylindernumber)
CarPrice$fuelsystem     <- as.factor(CarPrice$fuelsystem)


#
# Check for missing values
sum(is.na(CarPrice))        # No missing values


#
# Check for duplicate data
sum(duplicated(CarPrice))   # No duplicated data


#--------------------------------------------------------------------------------------------------#

#
# Dummy Variables creation
#


#
# Variables with 2 levels

# Column - fueltype - diesel as 1 & gas as 0 
levels(CarPrice$fueltype) <- c(1,0)
CarPrice$fueltype         <- as.numeric(levels(CarPrice$fueltype))[CarPrice$fueltype]

# Column - aspiration - std as 1 and turbo as 0
levels(CarPrice$aspiration) <- c(1,0)
CarPrice$aspiration         <- as.numeric(levels(CarPrice$aspiration))[CarPrice$aspiration]

# Column - doornumber - four as 1 and two as 0
levels(CarPrice$doornumber) <- c(1,0)
CarPrice$doornumber         <- as.numeric(levels(CarPrice$doornumber))[CarPrice$doornumber]

# Column - enginelocation - front as 1 and rear as 0
levels(CarPrice$enginelocation) <- c(1,0)
CarPrice$enginelocation     <- as.numeric(levels(CarPrice$enginelocation))[CarPrice$enginelocation]


#
# Variables with levels > 2

# Column - symboling - 6 levels
Dummy_symboling <- data.frame(model.matrix( ~symboling, data = CarPrice))
Dummy_symboling <- Dummy_symboling[,-1]

# Column - carbody - 5 levels
Dummy_carbody <- data.frame(model.matrix( ~carbody, data = CarPrice))
Dummy_carbody <- Dummy_carbody[,-1]

# Column - drivewheel - 3 levels
Dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = CarPrice))
Dummy_drivewheel <- Dummy_drivewheel[,-1]

# Column - enginetype - 7 levels
Dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = CarPrice))
Dummy_enginetype <- Dummy_enginetype[,-1]

# Column - cylindernumber - 7 levels
Dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = CarPrice))
Dummy_cylindernumber <- Dummy_cylindernumber[,-1]

# Column - fuelsystem - 8 levels
Dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = CarPrice))
Dummy_fuelsystem <- Dummy_fuelsystem[,-1]

# Column - CarCompany - 22 levels
Dummy_CarCompany <- data.frame(model.matrix( ~CarCompany, data = CarPrice))
Dummy_CarCompany <- Dummy_CarCompany[,-1]


#
# Update the data set with dummy variables
CarPrice <- cbind(CarPrice, Dummy_symboling, Dummy_carbody, Dummy_drivewheel,
                  Dummy_enginetype, Dummy_cylindernumber, Dummy_fuelsystem, 
                  Dummy_CarCompany)


# 
# Remove columns not required for analysis
# Car_ID - Unique id of each observation (Interger) - Can be removed from data set
CarPrice$car_ID         <- NULL
# CarName can we removed as the analysis will be done using CarCompany
CarPrice$CarName        <- NULL
# Columns for which dummy vraiables were created can also be removed
CarPrice$symboling      <- NULL
CarPrice$carbody        <- NULL
CarPrice$drivewheel     <- NULL
CarPrice$enginetype     <- NULL
CarPrice$cylindernumber <- NULL
CarPrice$fuelsystem     <- NULL
CarPrice$CarCompany     <- NULL


# 
# View the final Data Set post data cleaning and manipulation
View(CarPrice)
str(CarPrice)

#--------------------------------------------------------------------------------------------------#

#
# Data Setup for model building
#

# Set seed to 100
set.seed(100)

# Split data randomly 70% as Training Data and 30% as Test Data 
indices   = sample(1:nrow(CarPrice), 0.7*nrow(CarPrice))
TrainData = CarPrice[indices,]
TestData  = CarPrice[-indices,]


#--------------------------------------------------------------------------------------------------#

#
# Multiple Linear Regression - Model building
#


# For Model building following procedure will be used :
# 1. Build model 1 with all variables. Analyse model 1 and identify the insignificant variables
# 2. Using StepAIC method to confirm which variables can be eliminated due to insignificance
# 3. Post StepAIC cleanup, for each model check p value and vif value and eliminate variables one 
#    at a time. In addition R squared value in tracked to ensure no significant variable is
#    eliminated by error.
# 4. Once most of insignificant variables are eliminated and only those variables remain with low
#    p value, eliminate one variable at a time and analyse the effect on R squared.
# 5. Identify the final model with all significant variables and test the model


#
# Develop the first model 
Model_01 <- lm(price~., data = TrainData[,-1])
summary(Model_01)
# Model_01 - Multiple R-squared:  0.9819,	Adjusted R-squared:  0.9691


# Model_01 has many variables which are insignificant based on the pvalue
# Using StepAIC method, we can identify variables which are insignicant and can be eliminated.
# Apply the stepwise approach
step <- stepAIC(Model_01, direction = "both")

# Run the step object
step

Model_02 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + symboling.1 + 
               symboling0 + symboling3 + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
               enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
               cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
               CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
               CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_02)
vif(Model_02)
# Model_02 - Multiple R-squared:  0.9804,	Adjusted R-squared:  0.9735


# Analysing summary and vif outputs, fuelsystem2bbl variable is selected for removal.
# p value of fuelsystem2bbl = 0.210361
# vif of fuelsystem2bbl = 3.285979
# Other variables with high vif values was found to have a low p value. 
# Thus not being selected for removal for now.

# Remove fuelsystem2bbl
Model_03 <- lm(formula = price ~ aspiration + enginelocation + carwidth + curbweight + enginesize +
               stroke + peakrpm + symboling.1 + symboling0 + symboling3 + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv +
               enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive +
               cylindernumberthree + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
               CarCompanyhonda + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
               CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
               CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_03)
vif(Model_03)
# Model_03 - Multiple R-squared:  0.9801,	Adjusted R-squared:  0.9734


# Analysing model 3, symboling.1 and symboling0 are the candidates for removal.
# Since the p value of symboling0 is higher and also vif is higher than symboling.1, 
# this will be eliminated

# Remove symboling0
Model_04 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + symboling.1 + 
               symboling3 + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
               enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
               cylindernumberfive + cylindernumberthree + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
               CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
               CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_04)
vif(Model_04)
# Model_04 - Multiple R-squared:  0.9797,	Adjusted R-squared:  0.9731


# Remove symboling.1
Model_05 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm +
               symboling3 + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
               enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
               cylindernumberfive + cylindernumberthree + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
               CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
               CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_05)
vif(Model_05)
# Model_05 - Multiple R-squared:  0.9795,	Adjusted R-squared:  0.9731


# From model 5, CarCompanymercury can be removed since the p value is high
# Remove CarCompanymercury
Model_06 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm +
               symboling3 + carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
               enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
               cylindernumberfive + cylindernumberthree + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + 
               CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
               CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_06)
vif(Model_06)
# Model_06 - Multiple R-squared:  0.9791,	Adjusted R-squared:  0.9728


# From model 6, symboling3 has a high p value and has to be eliminated
# Remove symboling3
Model_07 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm +
               carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
               enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
               cylindernumberfive + cylindernumberthree + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + 
               CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
               CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_07)
vif(Model_07)
# Model_07 - Multiple R-squared:  0.9787,	Adjusted R-squared:  0.9725


# Remove cylindernumberfive
Model_08 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm +
               carbodyhardtop + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
               enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
               cylindernumberthree + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + 
               CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
               CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_08)
vif(Model_08)
# Model_08 - Multiple R-squared:  0.9781,	Adjusted R-squared:  0.972


# Remove carbodyhardtop
Model_09 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + carbodyhatchback + 
               carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
               enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
               cylindernumberthree + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + 
               CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
               CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_09)
vif(Model_09)
# Model_09 - Multiple R-squared:  0.9773,	Adjusted R-squared:  0.9712


# Remove carbodysedan
Model_10 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
               curbweight + enginesize + stroke + peakrpm + carbodyhatchback + 
               carbodywagon + drivewheelrwd + enginetypedohcv + 
               enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
               cylindernumberthree + 
               CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
               CarCompanyjaguar + CarCompanymazda + 
               CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
               CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_10)
vif(Model_10)
# Model_10 - Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9708


# Remove carbodywagon 
Model_11 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + enginesize + stroke + peakrpm + carbodyhatchback + 
                 drivewheelrwd + enginetypedohcv + 
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                 CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_11)
vif(Model_11)
# Model_11 - Multiple R-squared:  0.9766,	Adjusted R-squared:  0.9708


# Remove carbodyhatchback
Model_12 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + enginesize + stroke + peakrpm + drivewheelrwd + enginetypedohcv + 
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 cylindernumberthree + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyhonda + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_12)
vif(Model_12)
# Model_12 - Multiple R-squared:  0.9762,	Adjusted R-squared:  0.9706


# Remove cylindernumberthree
Model_13 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 curbweight + enginesize + stroke + peakrpm + drivewheelrwd + enginetypedohcv + 
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyhonda + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_13)
vif(Model_13)
# Model_13 - Multiple R-squared:  0.9756,	Adjusted R-squared:  0.9701


# Remove curbweight
Model_14 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + enginetypedohcv + 
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyhonda + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_14)
vif(Model_14)
# Model_14 - Multiple R-squared:  0.9747,	Adjusted R-squared:  0.9693


# Remove CarCompanysaab
Model_15 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + enginetypedohcv + 
                 enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyhonda + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_15)
vif(Model_15)
# Model_15 - Multiple R-squared:  0.9737,	Adjusted R-squared:  0.9683


# Remove enginetypeohc
Model_16 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + enginetypedohcv + 
                 enginetypel + enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyhonda + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_16)
vif(Model_16)
# Model_16 - Multiple R-squared:  0.9726,	Adjusted R-squared:  0.9673


# Remove enginetypel
Model_17 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + enginetypedohcv + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyhonda + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_17)
vif(Model_17)
# Model_17 - Multiple R-squared:  0.9723,	Adjusted R-squared:  0.9673


# Remove enginetypedohcv
Model_18 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyhonda + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen, 
               data = TrainData[, -1])
summary(Model_18)
vif(Model_18)
# Model_18 - Multiple R-squared:  0.9706,	Adjusted R-squared:  0.9655


# Remove CarCompanyvolkswagen
Model_19 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyhonda + CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota, 
               data = TrainData[, -1])
summary(Model_19)
vif(Model_19)
# Model_19 - Multiple R-squared:  0.9685,	Adjusted R-squared:  0.9633


# Remove CarCompanyhonda
Model_20 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota, 
               data = TrainData[, -1])
summary(Model_20)
vif(Model_20)
# Model_20 - Multiple R-squared:  0.9672,	Adjusted R-squared:  0.9622


# Remove CarCompanyrenault
Model_21 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanytoyota, 
               data = TrainData[, -1])
summary(Model_21)
vif(Model_21)
# Model_21 - Multiple R-squared:  0.9664,	Adjusted R-squared:  0.9615


# Remove CarCompanytoyota
Model_22 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth, 
               data = TrainData[, -1])
summary(Model_22)
vif(Model_22)
# Model_22 - Multiple R-squared:  0.9648,	Adjusted R-squared:   0.96


# Remove CarCompanynissan
Model_23 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda + 
                 CarCompanymitsubishi + CarCompanyplymouth, 
               data = TrainData[, -1])
summary(Model_23)
vif(Model_23)
# Model_23 - Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9592


# Remove CarCompanymazda
Model_24 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + 
                 CarCompanymitsubishi + CarCompanyplymouth, 
               data = TrainData[, -1])
summary(Model_24)
vif(Model_24)
# Model_24 - Multiple R-squared:  0.9627,	Adjusted R-squared:  0.9583


# Remove CarCompanyplymouth
Model_25 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymitsubishi, 
               data = TrainData[, -1])
summary(Model_25)
vif(Model_25)
# Model_25 - Multiple R-squared:  0.9613,	Adjusted R-squared:  0.9571


# Remove CarCompanydodge
Model_26 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + drivewheelrwd + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar + CarCompanymitsubishi, 
               data = TrainData[, -1])
summary(Model_26)
vif(Model_26)
# Model_26 - Multiple R-squared:  0.9599,	Adjusted R-squared:  0.9559


# Remove drivewheelrwd
Model_27 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + 
                 enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar + CarCompanymitsubishi, 
               data = TrainData[, -1])
summary(Model_27)
vif(Model_27)
# Model_27 - Multiple R-squared:  0.9578,	Adjusted R-squared:  0.9539


# Remove CarCompanymitsubishi
Model_28 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + enginetypeohcf + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanyjaguar, 
               data = TrainData[, -1])
summary(Model_28)
vif(Model_28)
# Model_28 - Multiple R-squared:  0.9552,	Adjusted R-squared:  0.9514


# With model 28 following are the stats:
# Multiple R-squared:  0.9552,	Adjusted R-squared:  0.9514
# All the variables remaining seem to be significant.
# Lets remove one variable at a time and check the results


# Remove enginetypeohcf
Model_29 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + enginetyperotor + 
                 CarCompanybmw + CarCompanybuick + CarCompanyjaguar, 
               data = TrainData[, -1])
summary(Model_29)
vif(Model_29)
# Model_29 - Multiple R-squared:  0.9497,	Adjusted R-squared:  0.9459
# Change is minimal. This variable can be eliminated.


# Remove enginetyperotor
Model_30 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm +
                 CarCompanybmw + CarCompanybuick + CarCompanyjaguar, 
               data = TrainData[, -1])
summary(Model_30)
vif(Model_30)
# Model_30 - Multiple R-squared:  0.9446,	Adjusted R-squared:  0.9409
# Change is minimal. This variable can be eliminated.


# Remove CarCompanyjaguar
Model_31 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm +
                 CarCompanybmw + CarCompanybuick, 
               data = TrainData[, -1])
summary(Model_31)
vif(Model_31)
# Model_31 - Multiple R-squared:  0.9315,	Adjusted R-squared:  0.9275
# Change is minimal. This variable can be eliminated.


# Remove aspiration
Model_32 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + stroke + peakrpm +
                 CarCompanybmw + CarCompanybuick, 
               data = TrainData[, -1])
summary(Model_32)
vif(Model_32)
# Model_32 - Multiple R-squared:  0.9261,	Adjusted R-squared:  0.9223
# Change is minimal. This variable can be eliminated.


# Remove stroke
Model_33 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + peakrpm +
                 CarCompanybmw + CarCompanybuick, 
               data = TrainData[, -1])
summary(Model_33)
vif(Model_33)
# Model_33 - Multiple R-squared:  0.9216,	Adjusted R-squared:  0.9182
# Change is minimal. This variable can be eliminated.


# Remove peakrpm
Model_34 <- lm(formula = price ~ enginelocation + carwidth + 
                 enginesize + CarCompanybmw + CarCompanybuick, 
               data = TrainData[, -1])
summary(Model_34)
vif(Model_34)
# Model_34 - Multiple R-squared:  0.9113,	Adjusted R-squared:  0.908
# Change is minimal. This variable can be eliminated.


# Remove enginelocation
Model_35.0 <- lm(formula = price ~ carwidth + enginesize + CarCompanybmw + CarCompanybuick, 
               data = TrainData[, -1])
summary(Model_35.0)
vif(Model_35.0)
# Model_35.0 - Multiple R-squared:  0.8365,	Adjusted R-squared:  0.8317
# ~7% difference which means enginelocation is quite significant


# Retain enginelocation, remove carwidth
Model_35.1 <- lm(formula = price ~ enginelocation + enginesize + CarCompanybmw + CarCompanybuick, 
                data = TrainData[, -1])
summary(Model_35.1)
vif(Model_35.1)
# Model_35.1 - Multiple R-squared:  0.8666,	Adjusted R-squared:  0.8627
# ~4% difference from model 34 which means carwidth is also significant


# Retain carwidth, remove enginesize
Model_35.2 <- lm(formula = price ~ enginelocation + carwidth + CarCompanybmw + CarCompanybuick, 
                 data = TrainData[, -1])
summary(Model_35.2)
vif(Model_35.2)
# Model_35.2 - Multiple R-squared:  0.8517,	Adjusted R-squared:  0.8474
# ~6% difference from model 34 which means enginesize is also significant


# Retain enginesize, remove CarCompanybmw
Model_35.3 <- lm(formula = price ~ enginelocation + carwidth + enginesize + CarCompanybuick, 
                 data = TrainData[, -1])
summary(Model_35.3)
vif(Model_35.3)
# Model_35.3 - Multiple R-squared:  0.8901,	Adjusted R-squared:  0.8869
# ~2% difference from model 34 which means CarCompanybmw is also significant


# Retain CarCompanybmw, remove CarCompanybuick
Model_35.4 <- lm(formula = price ~ enginelocation + carwidth + CarCompanybmw + CarCompanybuick, 
                 data = TrainData[, -1])
summary(Model_35.4)
vif(Model_35.4)
# Model_35.4 - Multiple R-squared:  0.8517,	Adjusted R-squared:  0.8474
# ~6% difference from model 34 which means CarCompanybuick is also significant


#
# Final model
# Model 34 w is best model with all significant variables in use
CarPriceModel <- Model_34
summary(CarPriceModel)
vif(CarPriceModel)

#
#  > summary(CarPriceModel)
#
# Call:
#   lm(formula = price ~ enginelocation + carwidth + enginesize + 
#        CarCompanybmw + CarCompanybuick, data = TrainData[, -1])
#
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5207.4 -1499.5  -322.4  1258.5  6821.0 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -69563.79    9448.61  -7.362 1.52e-11 ***
#   enginelocation  -17519.74    1630.39 -10.746  < 2e-16 ***
#   carwidth          1342.90     161.70   8.305 8.58e-14 ***
#   enginesize          86.62       9.03   9.592  < 2e-16 ***
#   CarCompanybmw     8415.83    1472.49   5.715 6.55e-08 ***
#   CarCompanybuick   5820.80    1189.75   4.892 2.75e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2478 on 137 degrees of freedom
# Multiple R-squared:  0.9113,	Adjusted R-squared:  0.908 
# F-statistic: 281.4 on 5 and 137 DF,  p-value: < 2.2e-16  
# 


#--------------------------------------------------------------------------------------------------#

#
# Multiple Linear Regression - Model testing
#


# Test the model on test dataset
PredictData <- predict(CarPriceModel,TestData[,-c(1,20)])

# Append predicted data PredictData to TestData data set
TestData$TestPrice <- PredictData

# calculate the test R2 
R_Value <- cor(TestData$price,TestData$TestPrice)
R_Squared_Value <- R_Value^2
cat("Info  : R_Value from test data =",round(R_Value,3),"\n")
cat("Info  : R_Squared_Value from test data",round(R_Squared_Value,3),"\n")


#--------------------------------------------------------------------------------------------------#

#
# Summary
#

# Which variables are significant in predicting the price of a car?

# Following variables are significant in predicting the price of a car :
# enginelocation
# carwidth
# enginesize
# CarCompanybmw
# CarCompanybuick

#--------------------------------------------------------------------------------------------------#




