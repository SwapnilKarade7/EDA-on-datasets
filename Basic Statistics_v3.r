2 + 2 # control + enter or control + R
# works as calculator

#Packages
install.packages("readr")
library(readr)

# Read data into R
education <- read_csv("C:/Data/education.csv")
xyz <- read.csv(file.choose()) # load csv file into R
?read_csv
# C:\Users\education.csv - this is windows default file path with a '\'
# C:\\Users\\Desktop\\education.csv - change it to '\\' to make it work in R

View(education)

#### Exploratory Data Analysis ####

# Measures of Central Tendency / First moment business decision

mean(education$workex) # '$' is used to refer to the variables within object
attach(education) # When used we can directly refer to the variable name

mean(gmat)
mean(workex)

rm(xyz) #Remove specific object to free RAM (memory)
rm(list=ls()) # Remove all to free RAM (memory)

median(workex) #Median

mode(workex)

#Mode
y <- c(19,4,5,7,29,19,19,19,19,29,13,25,29,5,5,5,5,4,4,4,4)

# Mode function 
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

Modes(y)
Modes(workex)

# Measures of Dispersion / Second moment business decision
var(workex) # variance
sd(workex)  # standard deviation
range <- max(workex) - min(workex) # range

install.packages("moments")
library(moments)

# Third moment business decision
skewness(workex)

# Fourth moment business decision
kurtosis(workex)

# Graphical Representation
barplot(gmat)
dotchart(gmat)

hist(gmat) # Histogram

boxplot(gmat) # Boxplot
z <- boxplot(gmat)
z$out # to see outliers

# Probability Distribution
install.packages("UsingR")
library("UsingR")
densityplot(gmat)

# Normal Quantile-Quantile Plot
qqnorm(gmat)
qqline(gmat)

qqnorm(workex)
qqline(workex)

# Transformation to make workex variable normal
qqnorm(log(workex))
qqline(log(workex))

####### /\/\/\/\/\/\/\/\/\/ ###########
####### Data Pre-processing ###########

################### Type Casting ########################
# Type casting is used to convert one data type into another.
# ex: categorical data to numeric data

data <- read.csv(file.choose()) # Load ethnic diversity dataset

# Checking str and summary of the data
str(data)
summary(data)
attach(data)

data$age <- as.numeric(data$age)

is.numeric(data$age) # it will return true or false
is.integer(data$Salaries)

# Ex: Convert Numeric to Integer
data$age <- as.integer(data$age)

# Ex: Convert categorical data to factor type
str(data) # categorical data is character

# convert sex column to factors
data$Sex <- as.factor(data$Sex)
str(data) # check now for Sex column

# Alternatively we can use argument stringAsFactors=TRUE
data1 <- read.csv(file.choose(), stringsAsFactors = TRUE)
str(data1)
summary(data1)



############ Handling duplicates ##################
# Duplicate entries are removed using 'duplicated' function
# it stores the duplicate values into another variable

# Load the data set 'mtcars_dup.csv'
dup_data <- read.csv(file.choose())
dup <- duplicated(dup_data)
dup
data_new <- dup_data[!duplicated(dup_data), ]
data_new


############ zero Variance ############
# Use 'apply' and 'var' functions to 
# check for variance on numerical values
apply(data, 2, var)

# Check for the columns having zero variance

which(apply(data, 2, var)==0) # ignore the warnings


################################################
## Missing values - imputation

# Load 'modified ethnic.csv' dataset - Data with missing values
data.mis <- read.csv(file.choose())

attach(data.mis)
sum(is.na(data.mis)) # Returns the count of rows with missing values

summary(data.mis)

# Omitting NA values from the Data 
data_omit <- na.omit(data.mis)
# na.omit => will omit the entire row with even 1 NA value in numeric columns

dim(data_omit)
sum(is.na(data_omit))

#### Substitution based Imputation - mean/median/mode imputation
# Mean imputation for continuous data - CLMAGE
data.mis$Salaries[is.na(data.mis$Salaries)] <- mean(data.mis$Salaries, na.rm = TRUE)
summary(data.mis$Salaries)

data.mis$age[is.na(data.mis$age)] <- mean(data.mis$age, na.rm = TRUE)

#### Mode imputation ####
# Missing values are present in categorical columns as well
# Convert the data to factor
summary(data.mis$Race)

data.mis$Race <- as.factor(data.mis$Race)
summary(data.mis$Race)

# Custom function to calculate Mode
Mode <- function(x){
  a = table(x) # x is a vector
  names(a[which.max(a)])
}

data.mis$Position[is.na(data.mis$Position)] <- Mode(data.mis$Position[!is.na(data.mis$Position)])

data.mis$Department[is.na(data.mis$Department)] <- Mode(data.mis$Department[!is.na(data.mis$Department)])

# select categorical columns with NA
data_cat <- data.mis[, -c(1,8,9)]
sum(is.na(data_cat))

# Apply is a inbuilt function
### X ---- dataframe, matrix, list
### MARGIN --- 2 implies to columns and 1 implies to rows
### FUN ---- Functions such as mean, sum, average, or any custom defined function

data_tran <- as.data.frame(apply(data_cat, 2, Mode))
data_tran

############## Dummy variables creation ###############
# Load ethnic diversity dataset
data <- read.csv(file.choose())

# checking str and summary of the data
str(data)
summary(data)
attach(data)

install.packages("fastDummies")
library(fastDummies)

data_dummy <- dummy_cols(data, select_columns = c("Position","State","Sex","MaritalDesc","CitizenDesc","EmploymentStatus","Department","Race"),
                      remove_first_dummy = TRUE,remove_most_frequent_dummy = FALSE,remove_selected_columns = TRUE)
?dummy_cols

# you can choose between remove_first_dummy and remove_most_frequent_dummy


##########################
##### Label encoding #####
library(CatEncoders)
View(data)

# Character column: 'Position'
lb_new <- LabelEncoder.fit(data$Position)

position_new <- transform(lb_new,data$Position)
position_new

# Using cbind to combine with original dataset
newdata <- cbind(data, position_new)


############# Normalization & Standardization ################
# Load ethnic diversity dataset as df

df <- data_dummy[, -c(1,2,3)]

# Normalize the data using custom function 
norm <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

df_norm <- as.data.frame(lapply(df, norm))
# Normalized data have values ranging between 0 to 1

# Standardization can be applied using inbuilt function 'scale'
# Load a new dataset mtcars
df_mt <- read.csv(file.choose())
View(df_mt)

# Use scale function 
df_scale <- as.data.frame(scale(df_mt))


###############################################
######## Outlier Treatment ########
# Load ethnic diversity dataset 
data <- read.csv(file.choose()) 

attach(data)
View(data)

boxplot(data$Salaries) # we can see outliers
boxplot(data$age) # no outliers

# There are outliers in the salary column
qunt1 <- quantile(data$Salaries, probs = c(0.25, 0.75))
qunt1 # 25% 3092.95, # 75% = 51452.89

H <- 1.5*IQR(data$Salaries, na.rm = T)
H # 42539.92

data$Salaries[data$Salaries < (qunt1[1] - H)] <- qunt1[1]
data$Salaries[data$Salaries > (qunt1[2] + H)] <- qunt1[2]
boxplot(data$Salaries)

# 'robustHD' package is used for winsorize function
install.packages("robustHD")
library(robustHD)

# Load ethnic diversity dataset 
data <- read.csv(file.choose())
View(data)

sal_out <- boxplot(data$Salaries) # we can see outliers
sal_out$out # ex: 24th row is an outlier

wins_data <- winsorize(data$Salaries)
boxplot(wins_data) # default prob 95% - 74446.48

##########################
##### z-distribution #####
pnorm(680, 711, 29) # Given a value, find the probability
qnorm(0.025) # Given probability, find the Z value


##### t-distribution #####
pt(1.98, 139) # Given a value, find the probability
qt(0.975, 139) # Given probability, find the t value
