
  # Load necessary libraries
library(readr)
library(dplyr)
library(sandwich)
library(lmtest)
library(ggplot2)
library(corrplot)
library(stargazer) #It's for our Regression Summary Table


# Load the dataset
smoking <- read.csv('/Users/efeuslu/Downloads/smoking.csv')

# Check the column names of the dataset
colnames(smoking)



# Data Cleaning

  ### Select rows where smoke is 'No'
no_smoke_rows <- smoking[smoking$smoke == 'No', ]

  ### Set amt_weekends and amt_weekdays to 0 for those rows
smoking$amt_weekends[smoking$smoke == 'No'] <- 0
smoking$amt_weekdays[smoking$smoke == 'No'] <- 0

  ### Replace 'type' values with 'Non-Smoker' for non-smokers
smoking$type[smoking$smoke == 'No'] <- 'Non-Smoker'

  ### Replace NaN values in the 'type' column with 'Non-Smoker' for non-smokers (if missing values are represented as NA)
smoking$type[is.na(smoking$type)] <- 'Non-Smoker'

  ### Replace missing values in 'type' column with 'Non-Smoker' for non-smokers (if missing values are represented as "")
smoking$type[smoking$smoke == 'No' & smoking$type == ""] <- 'Non-Smoker'

  ### Change levels in the 'type' column
smoking$type <- factor(smoking$type, levels = c("Packets", "Hand-Rolled", "Both/Mainly Packets", "Both/Mainly Hand-Rolled", "Non-Smoker"))

  ### Convert 'gender' variable to a binary variable (female = 1, male = 0)
smoking$female <- ifelse(smoking$gender == "Female", 1, 0)

  ### Convert 'smoke' variable to a binary numeric variable
smoking$smoke <- ifelse(smoking$smoke == 'Yes', 1, ifelse(smoking$smoke == 'No', 0, NA))

  ### Convert 'marital_status' to dummy variables
marital_status_dummies <- as.data.frame(model.matrix(~ factor(marital_status) - 1, data = smoking))
colnames(marital_status_dummies) <- paste0("marital_", levels(factor(smoking$marital_status)))
smoking <- cbind(smoking, marital_status_dummies)

  ### Check unique values in the 'smoke' column
print("Unique values in 'smoke' column:")
print(unique(smoking$smoke))

  ### Check for NA/NaN/Inf values in the dependent variable (smoke) and remove them
smoking <- smoking %>% filter(!is.na(smoke) & !is.infinite(smoke) & !is.nan(smoke))

  ### Convert 'gross_income' variable to a factor
smoking$gross_income <- factor(smoking$gross_income, levels = unique(smoking$gross_income))

  ### Print the structure of the dataframe
str(smoking)
summary(smoking)


# Data Exploration

print(paste("Dimensions of the dataframe: ", nrow(smoking), " rows and ", ncol(smoking), " columns"))
print("First few rows of the dataframe:")
print(head(smoking))
print("Last few rows of the dataframe:")
print(tail(smoking))
print("Structure of the dataframe:")
print(str(smoking))
print("Number of missing values in each column:")
print(colSums(is.na(smoking)))
print(paste("Number of duplicated rows in the dataframe: ", sum(duplicated(smoking))))

# Linear Probability Model
  ### Define the model formula
lpm_formula <- smoke ~ age + female + gross_income + . - gender - marital_status - type

  ### Fit the Linear Probability Model
lpm <- lm(lpm_formula, data = smoking)

  ### Calculate robust standard errors
robust_se <- coeftest(lpm, vcov = vcovHC(lpm, type = "HC1"))

  ### Print the summary of the model
summary(lpm)

  ### Print the results with robust standard errors
print(robust_se)


#Data Visualization

  ### Plot: Smoking Status by Marital Status
ggplot(smoking, aes(x = marital_status, fill = factor(smoke))) +
  geom_bar(position = "dodge") +
  labs(title = "Smoking Status by Marital Status",
       x = "Marital Status",
       y = "Count",
       fill = "Smoking Status (0: Non-Smoker, 1: Smoker)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ### Plot: Smoking Status by Gross Income
ggplot(smoking, aes(x = gross_income, fill = factor(smoke, labels = c("Non-Smoker", "Smoker")))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Non-Smoker" = "skyblue", "Smoker" = "salmon")) +
  labs(title = "Smoking Status by Gross Income",
       x = "Gross Income",
       y = "Count",
       fill = "Smoking Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

  ### Plot: Smoking Status by Marital Status
ggplot(smoking, aes(x = marital_status, fill = factor(smoke, labels = c("Non-Smoker", "Smoker")))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Non-Smoker" = "skyblue", "Smoker" = "salmon")) +
  labs(title = "Smoking Status by Marital Status",
       x = "Marital Status",
       y = "Count",
       fill = "Smoking Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
  ### Bin the age variable into age groups
smoking$age_group <- cut(smoking$age, breaks = c(0, 20, 30, 40, 50, 60, 70, Inf), 
                         labels = c("0-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71+"), right = FALSE)

  ### Plot: Smoking Status by Age Group
ggplot(smoking, aes(x = age_group, fill = factor(smoke, labels = c("Non-Smoker", "Smoker")))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Non-Smoker" = "skyblue", "Smoker" = "salmon")) +
  labs(title = "Smoking Status by Age Group",
       x = "Age Group",
       y = "Count",
       fill = "Smoking Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

  ### Plot: Smoking Status by Gender
ggplot(smoking, aes(x = gender, fill = factor(smoke, labels = c("Non-Smoker", "Smoker")))) +
  geom_bar(position = "dodge") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Non-Smoker" = "skyblue", "Smoker" = "salmon")) +
  labs(title = "Smoking Status by Gender",
       x = "Gender",
       y = "Count",
       fill = "Smoking Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


#Correlation Heatmap

### Select relevant columns for correlation matrix
relevant_columns <- smoking %>% select(smoke, age, female, marital_Divorced, marital_Married, marital_Separated, marital_Single, marital_Widowed)

### Compute the correlation matrix
cor_matrix <- cor(relevant_columns, use = "complete.obs")

### Plot the detailed and readable correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.7, col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Correlation Heatmap", mar = c(0, 0, 1, 0))



#Linear Probability Model and Regression Summary Table

### Fit the Linear Probability Model with interaction terms
lpm_interaction <- lm(smoke ~ age * female + age * gross_income + female * gross_income + age * marital_status + female * marital_status, data = smoking)

### Generate a detailed regression summary table and save it as HTML
stargazer(lpm_interaction, type = "html", title = "Regression Summary with Interaction Terms",
          covariate.labels = c("Age", "Female", "Gross Income", "Marital Status: Divorced", "Marital Status: Married", 
                               "Marital Status: Separated", "Marital Status: Single", "Marital Status: Widowed",
                               "Age x Female", "Age x Gross Income", "Female x Gross Income", "Age x Marital Status: Divorced", 
                               "Age x Marital Status: Married", "Age x Marital Status: Separated", "Age x Marital Status: Single", 
                               "Age x Marital Status: Widowed", "Female x Marital Status: Divorced", "Female x Marital Status: Married", 
                               "Female x Marital Status: Separated", "Female x Marital Status: Single", "Female x Marital Status: Widowed"),
          out = "regression_summary.html")

### To view the HTML in the RStudio Viewer or browser
browseURL("regression_summary.html")

