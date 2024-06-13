library(dplyr)
library(openxlsx)
library(readxl)
library(ggplot2)
library(glmnet)
library(imputeTS)
hepatitis <- read_excel("C:/Users/Palash/Desktop/r/asmer/hepatitis/hepatitis.xlsx")
pure_data <- hepatitis
View(hepatitis)
str(hepatitis)
summary(hepatitis)
attach(hepatitis)
table(hepatitis$Class)

#make a reusable function to find the unique values 
get_unique_values <- function(df) {
  unique_values <- lapply(df, unique)
  return(unique_values)
}
print(get_unique_values(hepatitis))


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


#separating the data into different class
hepatitis.alive = hepatitis[hepatitis$class==1,]
hepatitis.dead = hepatitis[hepatitis$class==2,]

#now deleting the class column from the respective dataframe
hepatitis.alive <- subset(hepatitis.alive, select=-c(class,sex))
hepatitis.dead <- subset(hepatitis.dead, select=-class)

#making a reusable r function to check the missing values
check_missing_values <- function(data) {
  for (i in colnames(data)) {
    cat(i, "--->", sum(is.na(data[, i])), "\n")
  }
}
check_missing_values(hepatitis.alive)
check_missing_values(hepatitis.dead)

#making a reusable function to find mode of the data
Mode <- function(x) {
  # Remove NA values
  x <- na.omit(x)
  # Calculate mode
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#Hepatitis alive
# Step 1: Identify rows with null values
null_rows <- apply(hepatitis.alive, 1, function(x) any(is.na(x)))
# Step 2: Get row numbers of rows with null values
row_numbers_with_null <- which(null_rows)
# Step 3: Count the number of null values per row
null_values_per_row <- apply(hepatitis.alive, 1, function(x) sum(is.na(x)))
# Step 4: Get the row number with respect to the null value
row_number_null_value <- sapply(hepatitis.alive[null_rows,], function(x) which(is.na(x)))
# Print results
cat("Rows with null values:", row_numbers_with_null, "\n")
cat("Number of null values per row:", null_values_per_row, "\n")
cat("Row number with respect to the null value:", row_number_null_value, "\n")
#this row contains 7 null values therefor can be dropped 
hepatitis.alive <- hepatitis.alive[c(-19),]


#Hepatitis dead
# Step 1: Identify rows with null values
null_rows1 <- apply(hepatitis.dead, 1, function(x) any(is.na(x)))
# Step 2: Get row numbers of rows with null values
row_numbers_with_null1 <- which(null_rows)
# Step 3: Count the number of null values per row
null_values_per_row1 <- apply(hepatitis.dead, 1, function(x) sum(is.na(x)))
# Step 4: Get the row number with respect to the null value
row_number_null_value1 <- sapply(hepatitis.dead[null_rows,], function(x) which(is.na(x)))
# Print results
cat("Rows with null values:", row_numbers_with_null1, "\n")
cat("Number of null values per row:", null_values_per_row1, "\n")
cat("Row number with respect to the null value:", row_number_null_value1, "\n")
#the rows contain 14,7,7 null values and can be dropped
hepatitis.dead <- hepatitis.dead[c(-53,-38,-67),]


#removing null values from hepatitis alive dataframe
check_missing_values(hepatitis.alive)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#now we will replace the null values with respect to the mode
#liver big
table(hepatitis.alive$`liver big`)
Mode(hepatitis.alive$`liver big`)
hepatitis.alive$`liver big`[is.na(hepatitis.alive$`liver big`)] <- Mode(hepatitis.alive$`liver big`)
hepatitis.alive$`liver big`

#liver firm
table(hepatitis.alive$`liver firm`)
hepatitis.alive$`liver firm
hepatitis.alive$`liver firm`[is.na(hepatitis.alive$`liver firm`)] <-Mode(hepatitis.alive$`liver firm`)
hepatitis.alive$`liver firm`

#spleen palpabale
table(hepatitis.alive$`spleen palpable`)
hepatitis.alive$`spleen palpable`[is.na(hepatitis.alive$`spleen palpable`)] <-Mode(hepatitis.alive$`spleen palpable`)
hepatitis.alive$`spleen palpable`

#spiders
table(hepatitis.alive$spiders)
hepatitis.alive$spiders[is.na(hepatitis.alive$spiders)] <- Mode(hepatitis.alive$spiders)

#ascites
table(hepatitis.alive$ascites)
hepatitis.alive$ascites[is.na(hepatitis.alive$ascites)] <- Mode(hepatitis.alive$ascites)

#Varices
table(hepatitis.alive$varices)
hepatitis.alive$varices[is.na(hepatitis.alive$varices)] <- Mode(hepatitis.alive$varices)

#bilirubin
# Calculate mean median and mode
mean_val <- mean(hepatitis.alive$bilirubin, na.rm = TRUE)
median_val <- median(hepatitis.alive$bilirubin, na.rm = TRUE)
mode_val <- Mode(hepatitis.alive$bilirubin)
# Plot
ggplot(hepatitis.alive, aes(x = bilirubin)) +
  geom_density() +
  geom_vline(xintercept = c(mean_val, median_val, mode_val), 
             linetype = "dashed", color = c("blue", "red", "green")) +
  scale_color_manual(name = "Statistics", 
                     values = c("blue" = "blue", "red" = "red", "green" = "green"),
                     labels = c("Mean", "Median", "Mode"))
#replacing bilirubin by the series mean of hepatitis.alive
hepatitis.alive$bilirubin[is.na(hepatitis.alive$bilirubin)] <- mean(mean_val)


#alk phosphate
# Calculate mean median and mode
mean_val_alk <- mean(hepatitis.alive$'alk phosphate', na.rm = TRUE)
median_val_alk <- median(hepatitis.alive$'alk phosphate', na.rm = TRUE)
mode_val_alk <- Mode(hepatitis.alive$'alk phosphate')
# Plot
ggplot(hepatitis.alive, aes(x = `alk phosphate`)) +
  geom_density() +
  geom_vline(xintercept = c(mean_val_alk, median_val_alk, mode_val_alk), 
             linetype = "dashed", color = c("blue", "red", "green")) +
  scale_color_manual(name = "Statistics", 
                     values = c("blue" = "blue", "red" = "red", "green" = "green"),
                     labels = c("Mean", "Median", "Mode"))
#replacing alk phosphate by the series mean of hepatitis.alive
hepatitis.alive$`alk phosphate`[is.na(hepatitis.alive$`alk phosphate`)] <- mean(mean_val_alk)

#sgot
#calculate mean median and mode
mean_val_sgot <- mean(hepatitis.alive$'sgot', na.rm = TRUE)
median_val_sgot <- median(hepatitis.alive$'sgot', na.rm = TRUE)
mode_val_sgot <- Mode(hepatitis.alive$'sgot')
# Plot
ggplot(hepatitis.alive, aes(x = sgot)) +
  geom_density() +
  geom_vline(xintercept = c(mean_val_sgot, median_val_sgot, mode_val_sgot), 
             linetype = "dashed", color = c("blue", "red", "green")) +
  scale_color_manual(name = "Statistics", 
                     values = c("blue" = "blue", "red" = "red", "green" = "green"),
                     labels = c("Mean", "Median", "Mode"))
#replacing bilirubin by the series mean of hepatitis.alive
hepatitis.alive$sgot[is.na(hepatitis.alive$sgot)] <- mean(mean_val_sgot)


#albumin
#calculate mean median and mode
mean_val_alb <- mean(hepatitis.alive$'albumin', na.rm = TRUE)
median_val_alb <- median(hepatitis.alive$'albumin', na.rm = TRUE)
mode_val_alb <- Mode(hepatitis.alive$'albumin')
# Plot
ggplot(hepatitis.alive, aes(x = albumin)) +
  geom_density() +
  geom_vline(xintercept = c(mean_val_alb, median_val_alb, mode_val_alb), 
             linetype = "dashed", color = c("blue", "red", "green")) +
  scale_color_manual(name = "Statistics", 
                     values = c("blue" = "blue", "red" = "red", "green" = "green"),
                     labels = c("Mean", "Median", "Mode"))
#replacing bilirubin by the series mean of hepatitis.alive
hepatitis.alive$albumin[is.na(hepatitis.alive$albumin)] <- mean(mean_val_alb)


#protime
which(is.na(hepatitis.alive$protime))
row_numbers_with_null_in_protime <- which(is.na(hepatitis.alive$protime))


sum(is.na(hepatitis.alive))
clean_hepatitis.alive = kNN(hepatitis.alive,k=5)
sum(is.na(clean_hepatitis.alive))


#-------------------------------------------------------------------------------------------------------------------------------
#removing null values from hepatitis dead dataframe
check_missing_values(hepatitis.dead)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#now we will replace the null values with respect to the mode
#liver big
table(hepatitis.dead$`liver big`)
Mode(hepatitis.dead$`liver big`)
hepatitis.dead$`liver big`[is.na(hepatitis.dead$`liver big`)] <- Mode(hepatitis.dead$`liver big`)
hepatitis.dead$`liver big`

#liver firm
table(hepatitis.dead$`liver firm`)
hepatitis.dead$`liver firm
hepatitis.dead$`liver firm`[is.na(hepatitis.dead$`liver firm`)] <-Mode(hepatitis.dead$`liver firm`)
hepatitis.dead$`liver firm`

#spleen palpabale
table(hepatitis.dead$`spleen palpable`)
hepatitis.dead$`spleen palpable`[is.na(hepatitis.dead$`spleen palpable`)] <-Mode(hepatitis.dead$`spleen palpable`)
hepatitis.dead$`spleen palpable`

#steroid
table(hepatitis.dead$steroid)
hepatitis.dead$steroid[is.na(hepatitis.dead$steroid)] <- Mode(hepatitis.dead$steroid)

#spiders
table(hepatitis.dead$spiders)
hepatitis.dead$spiders[is.na(hepatitis.dead$spiders)] <- Mode(hepatitis.dead$spiders)

#ascites
table(hepatitis.dead$ascites)
hepatitis.dead$ascites[is.na(hepatitis.dead$ascites)] <- Mode(hepatitis.dead$ascites)

#Varices
table(hepatitis.dead$varices)
hepatitis.dead$varices[is.na(hepatitis.dead$varices)] <- Mode(hepatitis.dead$varices)

#bilirubin
# Calculate mean median and mode
mean_val <- mean(hepatitis.dead$bilirubin, na.rm = TRUE)
median_val <- median(hepatitis.dead$bilirubin, na.rm = TRUE)
mode_val <- Mode(hepatitis.dead$bilirubin)
# Plot
ggplot(hepatitis.dead, aes(x = bilirubin)) +
  geom_density() +
  geom_vline(xintercept = c(mean_val, median_val, mode_val), 
             linetype = "dashed", color = c("blue", "red", "green")) +
  scale_color_manual(name = "Statistics", 
                     values = c("blue" = "blue", "red" = "red", "green" = "green"),
                     labels = c("Mean", "Median", "Mode"))
#replacing bilirubin by the series mean of hepatitis.alive
hepatitis.dead$bilirubin[is.na(hepatitis.dead$bilirubin)] <- mean(mean_val)


#alk phosphate
# Calculate mean median and mode
mean_val_alk <- mean(hepatitis.dead$'alk phosphate', na.rm = TRUE)
median_val_alk <- median(hepatitis.dead$'alk phosphate', na.rm = TRUE)
mode_val_alk <- Mode(hepatitis.dead$'alk phosphate')
# Plot
ggplot(hepatitis.dead, aes(x = `alk phosphate`)) +
  geom_density() +
  geom_vline(xintercept = c(mean_val_alk, median_val_alk, mode_val_alk), 
             linetype = "dashed", color = c("blue", "red", "green")) +
  scale_color_manual(name = "Statistics", 
                     values = c("blue" = "blue", "red" = "red", "green" = "green"),
                     labels = c("Mean", "Median", "Mode"))
#replacing alk phosphate by the series mean of hepatitis.alive
hepatitis.dead$`alk phosphate`[is.na(hepatitis.dead$`alk phosphate`)] <- mean(mean_val_alk)

#sgot
#calculate mean median and mode
mean_val_sgot <- mean(hepatitis.dead$'sgot', na.rm = TRUE)
median_val_sgot <- median(hepatitis.dead$'sgot', na.rm = TRUE)
mode_val_sgot <- Mode(hepatitis.dead$'sgot')
# Plot
ggplot(hepatitis.dead, aes(x = sgot)) +
  geom_density() +
  geom_vline(xintercept = c(mean_val_sgot, median_val_sgot, mode_val_sgot), 
             linetype = "dashed", color = c("blue", "red", "green")) +
  scale_color_manual(name = "Statistics", 
                     values = c("blue" = "blue", "red" = "red", "green" = "green"),
                     labels = c("Mean", "Median", "Mode"))
#replacing bilirubin by the series mean of hepatitis.alive
hepatitis.dead$sgot[is.na(hepatitis.dead$sgot)] <- mean(mean_val_sgot)


#albumin
#calculate mean median and mode
mean_val_alb <- mean(hepatitis.dead$'albumin', na.rm = TRUE)
median_val_alb <- median(hepatitis.dead$'albumin', na.rm = TRUE)
mode_val_alb <- Mode(hepatitis.dead$'albumin')
# Plot
ggplot(hepatitis.dead, aes(x = albumin)) +
  geom_density() +
  geom_vline(xintercept = c(mean_val_alb, median_val_alb, mode_val_alb), 
             linetype = "dashed", color = c("blue", "red", "green")) +
  scale_color_manual(name = "Statistics", 
                     values = c("blue" = "blue", "red" = "red", "green" = "green"),
                     labels = c("Mean", "Median", "Mode"))
#replacing bilirubin by the series mean of hepatitis.alive
hepatitis.dead$albumin[is.na(hepatitis.dead$albumin)] <- mean(mean_val_alb)


#protime
which(is.na(hepatitis.dead$protime))
row_numbers_with_null_in_protime <- which(is.na(hepatitis.dead$protime))


sum(is.na(hepatitis.dead))
clean_hepatitis.dead = kNN(hepatitis.dead,k=5)
sum(is.na(clean_hepatitis.dead))

clean_hepatitis <- rbind(clean_hepatitis.alive,clean_hepatitis.dead)
getwd()
setwd('C:/Users/Palash/Desktop/r/asmer')
write.csv(clean_hepatitis,'clean_hepatitis.csv')
