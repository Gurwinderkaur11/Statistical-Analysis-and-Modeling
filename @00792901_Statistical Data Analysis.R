# Install and load necessary libraries
install.packages(c("ggplot2", "dplyr", "corrplot", "GGally", "lmtest", "car", "readxl"))
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(lmtest)
library(car)
library(readxl)

# Load the dataset
data <- read_excel("C:\\Users\\gurwi\\statistic all task\\stats task 2\\concrete compressive strength.xlsx")

# Rename columns for easier reference
colnames(data) <- c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", 
                    "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", 
                    "Age", "Category", "Contains_Fly_Ash", "Compressive_Strength")

# View the structure of the data
str(data)

# Convert relevant columns to numeric
data$Cement <- as.numeric(data$Cement)
data$Blast_Furnace_Slag <- as.numeric(data$Blast_Furnace_Slag)
data$Fly_Ash <- as.numeric(data$Fly_Ash)
data$Water <- as.numeric(data$Water)
data$Superplasticizer <- as.numeric(data$Superplasticizer)
data$Coarse_Aggregate <- as.numeric(data$Coarse_Aggregate)
data$Fine_Aggregate <- as.numeric(data$Fine_Aggregate)
data$Age <- as.numeric(data$Age)
data$Compressive_Strength <- as.numeric(data$Compressive_Strength)

# 1. **Exploratory Data Analysis (EDA)**

# Descriptive statistics for each variable
summary(data)

# Visualizations: Histograms for key variables
par(mfrow = c(2, 4))  # Layout for multiple plots
hist(data$Cement, main = "Cement Distribution", xlab = "Cement (kg/m^3)", col = "lightblue")
hist(data$Blast_Furnace_Slag, main = "Blast Furnace Slag Distribution", xlab = "Blast Furnace Slag (kg/m^3)", col = "lightgreen")
hist(data$Fly_Ash, main = "Fly Ash Distribution", xlab = "Fly Ash (kg/m^3)", col = "lightcoral")
hist(data$Water, main = "Water Distribution", xlab = "Water (kg/m^3)", col = "lightyellow")
hist(data$Superplasticizer, main = "Superplasticizer Distribution", xlab = "Superplasticizer (kg/m^3)", col = "lightgray")
hist(data$Coarse_Aggregate, main = "Coarse Aggregate Distribution", xlab = "Coarse Aggregate (kg/m^3)", col = "lightpink")
hist(data$Fine_Aggregate, main = "Fine Aggregate Distribution", xlab = "Fine Aggregate (kg/m^3)", col = "lightgreen")
hist(data$Age, main = "Age Distribution", xlab = "Age (days)", col = "lightblue")

# Boxplots to check for outliers
par(mfrow = c(2, 4))
boxplot(data$Cement, main = "Cement Boxplot", col = "lightblue")
boxplot(data$Blast_Furnace_Slag, main = "Blast Furnace Slag Boxplot", col = "lightgreen")
boxplot(data$Fly_Ash, main = "Fly Ash Boxplot", col = "lightcoral")
boxplot(data$Water, main = "Water Boxplot", col = "lightyellow")
boxplot(data$Superplasticizer, main = "Superplasticizer Boxplot", col = "lightgray")
boxplot(data$Coarse_Aggregate, main = "Coarse Aggregate Boxplot", col = "lightpink")
boxplot(data$Fine_Aggregate, main = "Fine Aggregate Boxplot", col = "lightgreen")
boxplot(data$Age, main = "Age Boxplot", col = "lightblue")

# Correlation Matrix
correlation_matrix <- cor(data[, 1:8])  # Exclude non-numeric columns
corrplot(correlation_matrix, method = "circle")

# Pair plot to see relationships
ggpairs(data[, c("Cement", "Blast_Furnace_Slag", "Fly_Ash", "Water", 
                 "Superplasticizer", "Coarse_Aggregate", "Fine_Aggregate", 
                 "Age", "Compressive_Strength")])

# 2. **Correlation Analysis**

# Check the correlation of predictors with Compressive Strength
cor(data$Cement, data$Compressive_Strength)  # Correlation between Cement and Compressive Strength
cor(data$Fly_Ash, data$Compressive_Strength)  # Correlation between Fly Ash and Compressive Strength
cor(data$Water, data$Compressive_Strength)  # Correlation between Water and Compressive Strength
# Continue with other predictors similarly

# 3. **Regression Modeling**

# Fit a multiple linear regression model
model <- lm(Compressive_Strength ~ Cement + Blast_Furnace_Slag + Fly_Ash + Water + 
              Superplasticizer + Coarse_Aggregate + Fine_Aggregate + Age, data = data)

# Summary of the regression model
summary(model)

# 4. **Check Model Assumptions**

# Plot diagnostic plots to check assumptions
par(mfrow = c(2, 2))
plot(model)

# Breusch-Pagan Test for Homoscedasticity
bptest(model)

# Durbin-Watson Test for Autocorrelation
dwtest(model)

# Variance Inflation Factor (VIF) to check for multicollinearity
vif(model)

# 5. **Hypothesis Testing**

# Hypothesis 1: Does the amount of Cement affect the Compressive Strength?
# Null hypothesis: Cement does not affect Compressive Strength.
# Alternative hypothesis: Cement affects Compressive Strength.
t.test(data$Cement, data$Compressive_Strength)

# Hypothesis 2: Does the inclusion of Fly Ash affect Compressive Strength?
# Null hypothesis: Fly Ash inclusion does not affect Compressive Strength.
# Alternative hypothesis: Fly Ash inclusion affects Compressive Strength.
t.test(data$Compressive_Strength ~ data$Contains_Fly_Ash)

# 6. **Conclusion**

# Interpret the results from the regression, hypothesis tests, and model assumptions.
# Summarize key findings such as which components affect the compressive strength most significantly,
# and provide any practical recommendations for optimizing concrete mix composition.

