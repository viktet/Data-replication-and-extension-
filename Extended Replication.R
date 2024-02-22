load("main_did_dat_all_dal.Rdata")
data <- data.frame(main_did_dat_all_dal)

library(lfe)
library(stargazer)
library(dplyr)

#############################################
############ PART 2: REPLICATION ############
#############################################

# Creating 2 interaction terms: 
# for "after Uber/Lyft returns to Austin" 
# for "before Uber/Lyft exists Austin" 

data$did_afterreturn = (data$month >= "2017-05") *(data$location == "austin")
data$did_beforeexit = (data$month < "2016-05") * (data$location == "austin")

### Table 3 Regional Difference-in-Difference (Austin and San Antonio)  ###

# Running a linear regression with month and rest_id as FEs, clustering by rest_id

fixed_service = felm(neg_label_rate_service ~ did_afterreturn+did_beforeexit| month+rest_id|0|rest_id, data = data)
stargazer(fixed_service, header=FALSE, type = 'text')

### Table 4: Difference-in-Difference (Food) ###

fixed_food = felm(neg_label_rate_food ~ did_afterreturn+did_beforeexit| month+rest_id|0|rest_id, data = data)
stargazer(fixed_food, header=FALSE, type = 'text')

### Table 5 Difference-in-Difference ($ restaurant)

data$rest_dollar = as.factor(data$rest_dollar)

# Duplicating 2 times rest_dollar columns

data$rest_dollar2 <-data$rest_dollar 
data$rest_dollar3 <-data$rest_dollar 

# Assigning 2 and 3 dollar sign restaurants to high tier
# and 1 dollar sign restaurants to low tier

levels(data$rest_dollar2)[levels(data$rest_dollar2)%in%c("$$","$$$")] <- "high tier"
levels(data$rest_dollar2)[levels(data$rest_dollar2)%in%c("$")] <- "low tier"

# Creating a sub-set low tier consisting of low tier restaurants

low_tier <- filter(data, rest_dollar2 == "low tier") 

# Again creating 2 interaction terms, but this time for the "low tier" sub-set

low_tier$did_afterreturn = (low_tier$month >= "2017-05") * (low_tier$location == "austin") 
low_tier$did_beforeexit = (low_tier$month < "2016-05") * (low_tier$location == "austin") 

fixed_low_tier = felm(neg_label_rate_service ~did_afterreturn+did_beforeexit| month+rest_id|0|rest_id, data = low_tier)
stargazer(fixed_low_tier, header=FALSE, type = 'text')

### Table 6 Difference-in-Difference ($$ or $$$ restaurant)

# Creating a sub-set high tier consisting of high tier restaurants

high_tier <- filter(data, rest_dollar2 == "high tier") 

# Again creating 2 interaction terms, but this time for the "high tier" sub-set

high_tier$did_afterreturn = (high_tier$month >= "2017-05") * (high_tier$location == "austin") 
high_tier$did_beforeexit = (high_tier$month < "2016-05") * (high_tier$location == "austin")

fixed_high_tier = felm(neg_label_rate_service ~did_afterreturn+did_beforeexit| month+rest_id|0|rest_id, data = high_tier)
stargazer(fixed_high_tier, header=FALSE, type = 'text')


############################################################
############ PART 3: ADDITIONAL ROBUSTNESS TESTS ############
############################################################

### Robustness Test 1 ###

# Creating a sub-set for AUSTIN

austin <-filter(data, location == "austin") 

# Creating 2 interaction terms:
# between "after Uber/Lyft returns to Austin" and "low tier" 
# between "before Uber/Lyft exits Austin" and "low tier"

austin$low_afterreturn = (austin$month >= "2017-05") * (austin$rest_dollar2 == "low tier") 
austin$low_beforeexit = (austin$month < "2016-05") * (austin$rest_dollar2 == "low tier") 

rob_austin = felm(neg_label_rate_service ~low_afterreturn+low_beforeexit| month+rest_id|0|rest_id, data = austin)

# Creating a sub-set for DALLAS

dallas <-filter(data, location == "dallas") 

# Same hier

dallas$low_afterreturn = (dallas$month >= "2017-05") * (dallas$rest_dollar2 == "low tier") # same for Dallas
dallas$low_beforeexit = (dallas$month < "2016-05") * (dallas$rest_dollar2 == "low tier")

rob_dallas = felm(neg_label_rate_service ~low_afterreturn+low_beforeexit| month+rest_id|0|rest_id, data = dallas)

stargazer(list(rob_dallas,rob_austin), header=FALSE, type = 'text')

### Robustness test 2 ###

# Duplicate columns neg_label_rate_food and neg_label_rate_service

data$neg_label_rate_food2 <-data$neg_label_rate_food
data$neg_label_rate_service2<-data$neg_label_rate_service

# Creating a dummy taking value "1", if neg_label_rate_food is greater than 0,5 and "0" otherwise
# and another dummy taking value "1", if neg_label_rate_service is greater than 0,5 and "0" otherwise

data$neg_label_rate_food2 <- ifelse(data$neg_label_rate_food2 >= 0.5,1,0) 
data$neg_label_rate_service2<-ifelse(data$neg_label_rate_service2 >= 0.5,1,0) 

rob_fixed_service = felm(neg_label_rate_service2 ~ did_afterreturn+did_beforeexit| month+rest_id|0|rest_id, data = data)
rob_fixed_food = felm(neg_label_rate_food2 ~ did_afterreturn+did_beforeexit| month+rest_id|0|rest_id, data = data)

stargazer(list(rob_fixed_food,rob_fixed_service), header=FALSE, type = 'text')

### Robustness Test 3 ###

# Assigning 3 dollar sign restaurants to high tier
# and 1 and 2 dollar sign restaurants to low tier

levels(data$rest_dollar3)[levels(data$rest_dollar3)%in%c("$$$")] <- "high tier" 
levels(data$rest_dollar3)[levels(data$rest_dollar3)%in%c("$","$$")] <- "low tier" 

# Creating a new sub-set for low tier

low_tier2 <- filter(data, rest_dollar3 == "low tier") 

low_tier2$did_afterreturn = (low_tier2$month >= "2017-05") * (low_tier2$location == "austin")
low_tier2$did_beforeexit = (low_tier2$month < "2016-05") * (low_tier2$location == "austin")

rob_low_tier = felm(neg_label_rate_service ~did_afterreturn+did_beforeexit| month+rest_id|0|rest_id, data = low_tier2)

# Creating a new sub-set for high tier

high_tier2 <-filter(data, rest_dollar3 == "high tier") 

high_tier2$did_afterreturn = (high_tier2$month >= "2017-05") * (high_tier2$location == "austin")
high_tier2$did_beforeexit = (high_tier2$month < "2016-05") * (high_tier2$location == "austin")

rob_high_tier = felm(neg_label_rate_service ~did_afterreturn+did_beforeexit| month+rest_id|0|rest_id, data = high_tier2)
stargazer(rob_low_tier, rob_high_tier, header=FALSE, type = 'text')

############################################################
############ PART 4: DATASET EXTENSION ####################
############################################################


# Creating another sub-set for AUSTIN  

austin_ext <-filter(data, location == "austin")

# Aggregating variables "neg_label_rate_food2" and "neg_label_rate_service2" by month

monthly_aggregated <- austin_ext %>%
  group_by(month) %>%
  summarise(total_neg_food = sum(neg_label_rate_food2),total_neg_service = sum(neg_label_rate_service2)) 

### Working with Google trends data sets for AUSTIN ###

### Recipes ###

austin_recipes <-read.csv("aggregated_recipes_austin.csv", skip=2)

# Making variables "Easy.recipes...Austin.TX.","simple.recipes...Austin.TX.","quick.recipes...Austin.TX." numeric

austin_recipes[, c("Easy.recipes...Austin.TX.","simple.recipes...Austin.TX.","quick.recipes...Austin.TX.")] <- lapply(austin_recipes[, c("Easy.recipes...Austin.TX.","simple.recipes...Austin.TX.","quick.recipes...Austin.TX.")], as.numeric)

# Replacing NA with mean

austin_recipes <- as.data.frame(lapply(austin_recipes, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})) 

# Add up variables "Easy.recipes...Austin.TX.","simple.recipes...Austin.TX.","quick.recipes...Austin.TX." by row

austin_recipes$aggregated_recipes <-rowSums(as.matrix(austin_recipes[,c("Easy.recipes...Austin.TX.","simple.recipes...Austin.TX.","quick.recipes...Austin.TX.")]))

# Creating a sub set with only 2 variables "Week", "aggregated_recipes"

austin_recipes_aggregated <- austin_recipes[, c("Week", "aggregated_recipes")] 

### Poisoning ###

austin_poisoning <-read.csv("aggregated_poisoning_austin.csv", skip=2)

# Same here

austin_poisoning[, c("Food.poisoning...Austin.TX.","Diarrhea...Austin.TX.")] <- lapply(austin_poisoning[, c("Food.poisoning...Austin.TX.","Diarrhea...Austin.TX.")], as.numeric)

austin_poisoning <- as.data.frame(lapply(austin_poisoning, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
})) 

austin_poisoning$aggregated_poisoning <-rowSums(as.matrix(austin_poisoning[,c("Food.poisoning...Austin.TX.","Diarrhea...Austin.TX.")]))
austin_poisoning_aggregated <- austin_poisoning[, c("Week", "aggregated_poisoning")]

# Merging Google trends data sets for AUSTIN by Week 

merged_austin <- merge(austin_recipes_aggregated, austin_poisoning_aggregated, by = "Week", all = TRUE)

# Converting the 'Week' column to a Date type

merged_austin$Week <- as.Date(merged_austin$Week)

# Creating a new column for the month

merged_austin$month <- format(merged_austin$Week, "%Y-%m")

# Aggregating variables "aggregated_recipes" and "aggregated_poisoning" by "month"

monthly_merged_austin <- merged_austin %>%
  group_by(month) %>%
  summarise(total_recipes = sum(aggregated_recipes),total_poisoning = sum(aggregated_poisoning)) 


# Merging filtered aggregated data set and merged aggregated Google trend data set for AUSTIN based on the common column 'month' 

merged_data <- merge(monthly_aggregated, monthly_merged_austin, by = "month", all = TRUE)
merged_data$location <- "austin"
merged_data=merged_data[-1,] # deleting the first row

### Creating another sub-set for DALLAS

dallas_ext <-filter(data, location == "dallas")
monthly_aggregated_dallas <- dallas_ext %>%
  group_by(month) %>%
  summarise(total_neg_food = sum(neg_label_rate_food2),total_neg_service = sum(neg_label_rate_service2))

### Working with Google trends data sets for DALLAS ###

### Recipes ###

# Same here 
dallas_recipes <-read.csv("aggregated_recipes_dallas.csv", skip=2)
dallas_recipes[, c("Easy.recipes...Dallas.Ft..Worth.TX.","simple.recipes...Dallas.Ft..Worth.TX.","quick.recipes...Dallas.Ft..Worth.TX.")] <- lapply(dallas_recipes[, c("Easy.recipes...Dallas.Ft..Worth.TX.","simple.recipes...Dallas.Ft..Worth.TX.","quick.recipes...Dallas.Ft..Worth.TX.")], as.numeric)

dallas_recipes <- as.data.frame(lapply(dallas_recipes, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
}))

dallas_recipes$aggregated_recipes <-rowSums(as.matrix(dallas_recipes[,c("Easy.recipes...Dallas.Ft..Worth.TX.","simple.recipes...Dallas.Ft..Worth.TX.","quick.recipes...Dallas.Ft..Worth.TX.")]))
dallas_recipes_aggregated <- dallas_recipes[, c("Week", "aggregated_recipes")]

### Poisoning ###

dallas_poisoning <-read.csv("aggregated_poisoning_dallas.csv", skip=2)
dallas_poisoning[, c("Food.poisoning...Dallas.Ft..Worth.TX.","Diarrhea...Dallas.Ft..Worth.TX.")] <- lapply(dallas_poisoning[, c("Food.poisoning...Dallas.Ft..Worth.TX.","Diarrhea...Dallas.Ft..Worth.TX.")], as.numeric)

dallas_poisoning <- as.data.frame(lapply(dallas_poisoning, function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
}))

dallas_poisoning$aggregated_poisoning <-rowSums(as.matrix(dallas_poisoning[,c("Food.poisoning...Dallas.Ft..Worth.TX.","Diarrhea...Dallas.Ft..Worth.TX.")]))
dallas_poisoning_aggregated <- dallas_poisoning[, c("Week", "aggregated_poisoning")]

# Merging Google trends data sets for DALLAS by Week

merged_dallas <- merge(dallas_recipes_aggregated, dallas_poisoning_aggregated, by = "Week", all = TRUE)

# Converting the 'Week' column to a Date type

merged_dallas$Week <- as.Date(merged_dallas$Week)

# Create a new column for the month 

merged_dallas$month <- format(merged_dallas$Week, "%Y-%m")

monthly_merged_dallas <- merged_dallas %>%
  group_by(month) %>%
  summarise(total_recipes = sum(aggregated_recipes),total_poisoning = sum(aggregated_poisoning))

# Merging filtered aggregated data set and merged aggregated Google trend data set for DALLAS based on the common column 'month' 

merged_data_dallas <- merge(monthly_aggregated_dallas, monthly_merged_dallas, by = "month", all = TRUE)
merged_data_dallas$location <- "dallas"
merged_data_dallas = merged_data_dallas[-1,]

# Combing data for DALLAS and AUSTIN

combined_data <-rbind(merged_data,merged_data_dallas)


### CORRELATION TABLE ###

library(tidyverse)
library(corrplot)
library(xtable)

combined_data_corr <- combined_data[, c("total_neg_food", "total_neg_service","total_recipes","total_poisoning")]
cor_matrix <- cor(combined_data_corr, use = "complete.obs")
cor_matrix

print(xtable(cor_matrix, caption = "Correlation Matrix for Selected Variables"), include.rownames=TRUE)


### REGRESSIONS ###

# Analogically to the main analysis 
# 1) creating interaction terms 
# 2) running FE regressions with total_recipes and total_poisoning as outcomes variables 

combined_data$did_afterreturn = (combined_data$month >= "2017-05") *(combined_data$location == "austin")
combined_data$did_beforeexit = (combined_data$month < "2016-05") * (combined_data$location == "austin")

fixed_recipes = felm(total_recipes ~ did_afterreturn+did_beforeexit+as.numeric(location == "austin")| month |0|location, data = combined_data)
fixed_poison = felm(total_poisoning ~ did_afterreturn+did_beforeexit+as.numeric(location == "austin")| month |0|location, data = combined_data)


stargazer(list(fixed_recipes, fixed_poison), header=FALSE, type = 'text')

