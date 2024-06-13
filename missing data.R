library(dplyr)
library(openxlsx)
library(readxl)
library(ggplot2)
hepatitis <- read_excel("C:/Users/Palash/Desktop/r/asmer/hepatitis/hepatitis.xlsx")
pure_data <- hepatitis
View(hepatitis)
str(hepatitis)
summary(hepatitis)
str(hepatitis)
attach(hepatitis)

colnames(hepatitis) <- tolower(colnames(hepatitis))


#since most of the variables are character we change them to numeric
hepatitis$fatigue <- as.numeric(hepatitis$fatigue)
hepatitis$fatigue
hepatitis$malaise <- as.numeric(hepatitis$malaise) 
hepatitis$malaise
hepatitis$steroid <- as.numeric(hepatitis$steroid) 
hepatitis$steroid
hepatitis$anorexia <- as.numeric(hepatitis$anorexia) 
hepatitis$anorexia
hepatitis$`liver big` <- as.numeric(hepatitis$`liver big`) 
hepatitis$`liver big`
hepatitis$`liver firm` <- as.numeric(hepatitis$`liver firm`) 
hepatitis$`liver firm`
hepatitis$`spleen palpable` <- as.numeric(hepatitis$`spleen palpable`) 
hepatitis$`spleen palpable`
hepatitis$spiders <- as.numeric(hepatitis$spiders) 
hepatitis$spiders
hepatitis$ascites <- as.numeric(hepatitis$ascites) 
hepatitis$ascites
hepatitis$varices <- as.numeric(hepatitis$varices) 
hepatitis$varices
hepatitis$bilirubin <- as.numeric(hepatitis$bilirubin) 
hepatitis$bilirubin
hepatitis$`alk phosphate` <- as.numeric(hepatitis$`alk phosphate`) 
hepatitis$`alk phosphate`
hepatitis$sgot <- as.numeric(hepatitis$sgot) 
hepatitis$sgot
hepatitis$albumin <- as.numeric(hepatitis$albumin) 
hepatitis$albumin
hepatitis$protime <- as.numeric(hepatitis$protime) 
hepatitis$protime
hepatitis$histology <- as.numeric(hepatitis$histology) 
hepatitis$histology

#making a reusable r function to check the missing values
check_missing_values <- function(data) {
  for (i in colnames(data)) {
    cat(i, "--->", sum(is.na(data[, i])), "\n")
  }
}
check_missing_values(hepatitis)


summary(hepatitis)
str(hepatitis)

attach(hepatitis)



#replacing the values of some columns of hepatitis by their mode
# Find the mode
mode <- as.numeric(names(freq_table)[which(freq_table == max(freq_table))])
print(mode)

hepatitis$steroid[is.na(hepatitis$steroid)] <- mode
sum(is.na(hepatitis$steroid))

hepatitis$fatigue[is.na(hepatitis$fatigue)] <- as.numeric(names(table(fatigue))[which(table(fatigue) == max(table(fatigue)))])
sum(is.na(hepatitis$fatigue))

hepatitis$malaise[is.na(hepatitis$malaise)] <- as.numeric(names(table(malaise))[which(table(malaise) == max(table(malaise)))])
sum(is.na(hepatitis$malaise))

hepatitis$anorexia[is.na(hepatitis$anorexia)] <- as.numeric(names(table(anorexia))[which(table(anorexia) == max(table(anorexia)))])
sum(is.na(hepatitis$anorexia))


check_missing_values(hepatitis)

#here we replace the null values with the clear understanding that most of the people who have liver big are obviously on the verge of dying 
library(ggplot2)
table(hepatitis[class==2,"liver big"])
table(hepatitis[class==1,"liver big"])
`liver big`
sum(is.na(hepatitis[class==2,"liver big"]))
sum(is.na(hepatitis[class==1,"liver big"]))
hepatitis[class==2,"liver big"]
hepatitis[class==1,"liver big"]
#null values which has class=2 are replaced with 2(meaning they have histology)
hepatitis$`liver big`[is.na(hepatitis$`liver big`)&class==2] <- 2
#null values which has class=1 are replaced with 2(meaning they dont have histology)
hepatitis$`liver big`[is.na(hepatitis$`liver big`)&class==1] <- 1
hepatitis$`liver big`
sum(is.na(hepatitis$`liver big`))


#---------------------------------------------------------
table(hepatitis[`liver big`==1,"liver firm"])
table(hepatitis[`liver big`==2,"liver firm"])
sum(is.na(hepatitis[class==2,"liver firm"]))
sum(is.na(hepatitis[class==1,"liver firm"]))
#null values which has class=2 are replaced with 2(meaning the have liver firm) 
hepatitis$`liver firm`[is.na(hepatitis$`liver firm`)&class==2] <- 2
#null values which has class=1 are replaced with 1(meaning they dont have liver firm)
hepatitis$`liver firm`[is.na(hepatitis$`liver firm`)&class==1] <- 1
sum(is.na(hepatitis$`liver firm`))



#----------------------------------------------------
sum(is.na(`spleen palpable`))
sum(is.na(hepatitis[class==2,"spleen palpable"]))
sum(is.na(hepatitis[class==1,"spleen palpable"]))
#the null values here are replaced with the mode of the data
table(hepatitis$`spleen palpable`)
hepatitis$`spleen palpable`[is.na(hepatitis$`spleen palpable`)] <- as.numeric(names(table(`spleen palpable`))[which(table(`spleen palpable`) == max(table(`spleen palpable`)))])
sum(is.na(hepatitis$`spleen palpable`))

#------------------------------------------------------
table(spiders)
table(hepatitis[class==2,'spiders'])
sum(is.na(hepatitis$`spiders`))
sum(is.na(hepatitis[class==2,"spiders"]))
sum(is.na(hepatitis[class==1,"spiders"]))
#null values are replaced with 2 in case class is 2
hepatitis$`spiders`[is.na(hepatitis$`spiders`)&class==2] <- 2
#null values are replaced with 1 in case class is 1
hepatitis$`spiders`[is.na(hepatitis$`spiders`)&class==1] <- 1
hepatitis$spiders

#-------------------------------------------------------
sum(is.na(ascites))
table(ascites)
sum(is.na(hepatitis[class==2,"ascites"]))
sum(is.na(hepatitis[class==1,"ascites"]))
table(hepatitis[class==2,"ascites"])
#null values are replaced with the knowledge that those who are dead must have ascites
hepatitis$ascites[is.na(hepatitis$ascites)&class==2] <- 2
hepatitis$ascites[is.na(hepatitis$ascites)&class==1] <- 1
hepatitis$ascites

#-----------------------------------------------------
sum(is.na(varices))
table(varices)
table(hepatitis[class==1,"varices"])
table(hepatitis[class==2,"varices"])
sum(is.na(hepatitis[class==2,"varices"]))
sum(is.na(hepatitis[class==1,"varices"]))
#here the data is replaced with mode
hepatitis$varices[is.na(hepatitis$varices)] <- as.numeric(names(table(varices))[which(table(varices) == max(table(varices)))])
hepatitis$varices

#-----------------------------------------------------
#missing values in continuous plot
library(ggplot2)
ggplot(hepatitis,aes(x=bilirubin))+geom_density()
sum(is.na(bilirubin))
#to check if the data replacement with mean was correct or not!!
#ask sir!!!
hepatitis$bilirubin[is.na(hepatitis$bilirubin)] <- mean(hepatitis$bilirubin,na.rm = T)
mean(hepatitis$bilirubin,na.rm = T)
hepatitis$bilirubin
str(hepatitis)
View(hepatitis)

#------------------------------------------------------------------
#to check if the data replacement with mean was correct or not!!
ggplot(hepatitis,aes(x=sgot))+geom_density()#+geom_vline(xintercept = c(mean(sgot,na.rm=T),median(sgot,na.rm=T)))
sum(is.na(hepatitis$sgot))
hepatitis$sgot[is.na(hepatitis$sgot)] <- mean(hepatitis$sgot,na.rm = T)
hepatitis$sgot


summary(hepatitis[class==2,"protime"])
summary(hepatitis[class==1,"protime"])
summary(hepatitis$protime)


check_missing_values(hepatitis)

data1 <- hepatitis[class==2,"protime"][!is.na(hepatitis[class==2,"protime"])]

# Create a density plot
plot(density(data1), main = "Density Plot (NA Removed) when there is death (protime)", xlab = "Data", ylab = "Density", col = "blue")
# Assuming you have already loaded the required data and libraries

# Create a density plot
plot(density(data1), 
     main = "Density Plot (NA Removed) when there is death", 
     xlab = "Data", 
     ylab = "Density", 
     col = "blue")

# Add a filled density curve with a transparent color
polygon(density(data1), col = rgb(0, 0, 1, alpha = 0.2))

# Add a legend
legend("topright", 
       legend = "Data Density", 
       fill = "blue", 
       border = NA)

# Customize axis labels and tick marks
axis(1, col = "black")
axis(2, col = "black")

# Add a grid
grid(col = "lightgray")




data2 <- hepatitis[class==1,"protime"][!is.na(hepatitis[class==1,"protime"])]
plot(density(data2), main = "Density Plot (NA Removed) when there is no death", xlab = "Data", ylab = "Density", col = "blue")


median(hepatitis[class==1,]$protime,na.rm = T)
median(hepatitis[class==2,]$protime,na.rm = T)

mean(hepatitis[class==1,]$protime,na.rm = T)
mean(hepatitis[class==2,]$protime,na.rm = T)

check_missing_values(hepatitis)


#dont run since we have already got the data we want
#write.csv(hepatitis, file = "data_cleaned.csv", row.names = T)
#View(hepatitis)

#library(openxlsx)
# Save data frame as an Excel file
#write.xlsx(hepatitis, file = "data.xlsx")
#getwd()
data_cleaned <- read_excel("C:/Users/Palash/Desktop/r/asmer/data_cleaned.xlsx")
View(data_cleaned)



View(data_cleaned)
ggplot(data_cleaned,aes(y=bilirubin))+geom_boxplot()
ggplot(data_cleaned,aes(y=`alk phosphate`))+geom_boxplot()
ggplot(data_cleaned,aes(y=sgot))+geom_boxplot()
ggplot(data_cleaned,aes(y=albumin))+geom_boxplot()
ggplot(data_cleaned,aes(y=protime))+geom_boxplot()


ggplot(data_cleaned,aes(x=bilirubin))+geom_histogram()
ggplot(data_cleaned,aes(x=`alk phosphate`))+geom_histogram()
ggplot(data_cleaned,aes(x=sgot))+geom_histogram()
ggplot(data_cleaned,aes(x=albumin))+geom_histogram()
ggplot(data_cleaned,aes(x=protime))+geom_histogram()


data_cleaned
hepatitis


#making a heatmap using missing values
library(viridis)



View(pure_data)
colnames(pure_data) <- colnames(hepatitis)

check_missing_values(hepatitis)

pure_data$fatigue <- as.numeric(pure_data$fatigue)
pure_data$fatigue
pure_data$malaise <- as.numeric(pure_data$malaise) 
pure_data$malaise
pure_data$steroid <- as.numeric(pure_data$steroid) 
pure_data$steroid
pure_data$anorexia <- as.numeric(pure_data$anorexia) 
pure_data$anorexia
pure_data$`liver big` <- as.numeric(pure_data$`liver big`) 
pure_data$`liver big`
pure_data$`liver firm` <- as.numeric(pure_data$`liver firm`) 
pure_data$`liver firm`
pure_data$`spleen palpable` <- as.numeric(pure_data$`spleen palpable`) 
pure_data$`spleen palpable`
pure_data$spiders <- as.numeric(pure_data$spiders) 
pure_data$spiders
pure_data$ascites <- as.numeric(pure_data$ascites) 
pure_data$ascites
pure_data$varices <- as.numeric(pure_data$varices) 
pure_data$varices
pure_data$bilirubin <- as.numeric(pure_data$bilirubin) 
pure_data$bilirubin
pure_data$`alk phosphate` <- as.numeric(pure_data$`alk phosphate`) 
pure_data$`alk phosphate`
pure_data$sgot <- as.numeric(pure_data$sgot) 
pure_data$sgot
pure_data$albumin <- as.numeric(pure_data$albumin) 
pure_data$albumin
pure_data$protime <- as.numeric(pure_data$protime) 
pure_data$protime
pure_data$histology <- as.numeric(pure_data$histology) 
pure_data$histology
View(pure_data)
binary_matrix <- ifelse(is.na(pure_data), 0, 1)
View(binary_matrix)
library(ggplot2)
heatmap(binary_matrix, Colv = NA, Rowv = NA, col = viridis(256), scale = "none")

#with this we understand that most of the people who were not there
# k Phosphate,albumin,protime test have the same null values

View(hepatitis)
#checking percentage missing in the data
for (i in colnames(hepatitis)){
  print(paste(i,"--->",sum(is.na(hepatitis[,i]))/154))
}

#now we need to change the null values in 3 features that is
#alk phosphate
#albumin
#protime

hepatitis[!is.na(hepatitis$`alk phosphate`),]



View(hepatitis[!is.na(hepatitis$`alk phosphate`),])
plot(density(hepatitis[!is.na(hepatitis$`alk phosphate`),]$"alk phosphate"))
plot(density(hepatitis[!is.na(hepatitis$"albumin"),]$'albumin'))
plot(density(hepatitis[!is.na(hepatitis$"protime"),]$'protime'))


# alk phosphate plot
# Filter out NA values
filtered_data <- hepatitis[!is.na(hepatitis$`alk phosphate`), ]
# Create a density plot with ggplot2
ggplot(filtered_data, aes(x = `alk phosphate`)) +
  geom_density(fill = "skyblue", color = "darkblue", alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Density Plot of Alkaline Phosphatase Levels",
    x = "Alkaline Phosphatase",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )


#albumin plot
# Filter out NA values
filtered_data <- hepatitis[!is.na(hepatitis$"albumin"),]
# Create a density plot
plot(density(filtered_data$'albumin'),
     main = "Density Plot of Albumin Levels",
     xlab = "Albumin Levels",
     ylab = "Density",
     col = "blue")
# Add a filled density curve with a transparent color
polygon(density(filtered_data$'albumin'), col = rgb(0, 0, 1, alpha = 0.2))
# Add a legend
legend("topright",
       legend = "Albumin Density",
       fill = "blue",
       border = NA)
# Customize axis labels and tick marks
axis(1, col = "black")
axis(2, col = "black")
# Add a grid
grid(col = "lightgray")


#protime plot
# Filter out NA values
filtered_data <- hepatitis[!is.na(hepatitis$"protime"),]
# Create a density plot
plot(density(filtered_data$'protime'),
     main = "Density Plot of Prothrombin Time Levels",
     xlab = "Prothrombin Time Levels",
     ylab = "Density",
     col = "blue")
# Add a filled density curve with a transparent color
polygon(density(filtered_data$'protime'), col = rgb(0, 0, 1, alpha = 0.2))
# Add a legend
legend("topleft",
       legend = "Protime Density")
# Customize axis labels and tick marks
axis(1, col = "black")
axis(2, col = "black")
# Add a grid
grid(col = "lightgray")









summary(hepatitis$`alk phosphate`)
table(hepatitis$`alk phosphate`)
max(table(hepatitis$`alk phosphate`))
summary(hepatitis$'albumin')
table(hepatitis$albumin)
max(table(hepatitis$"albumin"))



#from the calculated summary statistics we impute the values
hepatitis$albumin[is.na(hepatitis$albumin)] <- 3.8
hepatitis$albumin
hepatitis$`alk phosphate` <- pure_data$`alk phosphate`
hepatitis$`alk phosphate`
hepatitis$`alk phosphate`[is.na(hepatitis$`alk phosphate`)] <- 85

check_missing_values(hepatitis)

# Assuming "data" is your data frame with missing values
# Create the imputation model
#check since it is not working
# ------------------------------------




View(hepatitis[is.na(hepatitis$protime),])
View(hepatitis[!is.na(hepatitis$protime),])
protime.null.dataset <- hepatitis[is.na(hepatitis$protime),]
protime.dataset <- hepatitis[!is.na(hepatitis$protime),]


#not using linear regression we are going to take protime as the dependent variable
#we are going to impute the missing values
model <- lm(protime~.,data=protime.dataset)
summary(model)
predict(model,)

predicted.values <- predict(model,newdata = protime.null.dataset)
predicted.values

protime.null.dataset$protime<-predicted.values
View(protime.null.dataset)
View(rbind(protime.dataset,protime.null.dataset))
complete.data <- rbind(protime.dataset,protime.null.dataset)
View(complete.data)
#now this completes our filling of the missing values our dataset


check_missing_values(complete.data)
#the complete.data file does not contain any missing values in the data


# Replace 'dependent_var' with the name of your binary outcome variable,
# and 'independent_var1', 'independent_var2', etc., with your predictor variables.
View(hepatitis)
View(complete.data)
hepatitis$class <- ifelse(hepatitis$class==1,0,1)
View(complete.data)
complete.data$class <- ifelse(hepatitis$class==1,0,1)
View(complete.data)

complete.data$class <- ifelse(complete.data$class==0,1,0)

check_missing_values(complete.data)




# Install and load the caret package
#install.packages("caret")
library(caret)

complete.data$class
# Set the seed for reproducibility
set.seed(123)
# Split the data into training and testing sets
train_index <- createDataPartition(complete.data$class, p = 0.7, list = FALSE)
train_data <- complete.data[train_index, ]
test_data <- complete.data[-train_index, ]
View(train_data)
View(test_data)
dim(train_data)
dim(test_data)
View(subset(train_data,select=-class))

#making model on logistic regression using training data
model.logistic <- glm(class ~ ., 
             data = train_data, 
             family = "binomial")
summary(model.logistic)
predicted.logistic <- predict(model.logistic,newdata = subset(test_data,select=-class),type='response')
predicted.logistic
test.binary.predictions <- ifelse(predicted.logistic>0.5,1,0)
test.binary.predictions
test.accuracy = Accuracy(test.binary.predictions,test_data$class)
test.accuracy

predict.logistic.train <- predict(model.logistic,newdata=subset(train_data,select=-class),type='response')
predict.logistic.train
train.binary.predictions <- ifelse(predict.logistic.train>0.5,1,0)
train.binary.predictions
train.accuracy <- Accuracy(train.binary.predictions,train_data$class)
train.accuracy

library(MLmetrics)
# Create a confusion matrix
cm <- confusionMatrix(factor(binary.predictions), factor(test_data$class))
cm
accuracy <- Accuracy(binary.predictions,test_data$class)
# Calculate precision, recall, and F1-score
precision <- Precision(binary.predictions,test_data$class)
precision
recall <- Recall(binary.predictions,test_data$class)
recall
f1_score <- F1_Score(binary.predictions,test_data$class)
f1_score

df=data.frame(accuracy,precision,recall,f1_score)
df

#---------------------------------------------------------
library(pROC)
# Calculate ROC curve and AUC score
roc_curve <- roc(test_data$class, predicted.logistic)
auc_score <- auc(roc_curve)
# Plot ROC curve
plot(roc_curve, main = paste("ROC Curve (AUC =", auc_score, ")"))

#--------------------------------------------------------
library(ROCR)
# Create a prediction object
prediction_obj <- prediction(predicted.logistic, test_data$class)
# Calculate performance measures (ROC curve and AUC)
performance_obj <- performance(prediction_obj, "tpr", "fpr")
auc_score <- performance(prediction_obj, "auc")@y.values[[1]]
# Plot ROC curve
plot(performance_obj, main = paste("ROC Curve (AUC =", auc_score, ")"))

#-------------------------------------------------------------
library(car)
library(MASS)
# Perform likelihood-based feature selection using stepAIC
selected_model <- stepAIC(model.logistic, direction = "both")
selected_model
summary(selected_model)


#-------------------------------------------------
library(glmnet)
# Fit logistic regression with Lasso regularization
# Convert the response variable to a factor if it's not already

# Fit logistic regression with Lasso regularization using cross-validation
lasso_model <- cv.glmnet(as.matrix(train_data[, -which(names(train_data) == "class")]), 
                         train_data$class, 
                         family = "binomial", 
                         alpha = 1)

# Fit logistic regression with Ridge regularization
ridge_model <- cv.glmnet(as.matrix(train_data[, -which(names(train_data) == "class")]), 
                         train_data$class, 
                         family = "binomial", 
                         alpha=0)
# Print coefficients
coef(lasso_model, s = "lambda.min")
coef(ridge_model, s = "lambda.min")

# Plot the coefficient paths
plot(lasso_model, xvar = "lambda", label = TRUE)
plot(ridge_model, xvar = "lambda", label = TRUE)






View(complete.data)
summary(complete.data)
str(complete.data)


#Checking the distribution of the protime before and after filling the null values using regression
complete.data$protime
plot(density(complete.data$'protime'),
     main = "Density Plot of Prothrombin Time Levels(Regression)",
     xlab = "Prothrombin Time Levels",
     ylab = "Density",
     col = "blue")
# Add a filled density curve with a transparent color
polygon(density(complete.data$'protime'), col = rgb(0, 0, 1, alpha = 0.2))
# Add a legend
legend("topleft",
       legend = "Protime Density")
# Customize axis labels and tick marks
axis(1, col = "black")
axis(2, col = "black")
# Add a grid
grid(col = "lightgray")




#--------------------------------------------------------------



