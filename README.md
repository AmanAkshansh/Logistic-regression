# Logistic-regression
Predicting customer churn at a wireless telecom company

# Dataset
There are 71,047 customers in the database and 75 potential predictors.
The Dataset combines the calibration and validation customers. 'Calibration' dataset consisting of 40,000 customers and 'Validation' dataset consisting of 31,047 customers.

# Business problem:
The objective of this project is to develop a model for predicting customer churn at a wireless telecom company, and use insights from the model to develop an incentive plan for enticing would-be churners to remain with the comapny.

# Techniques used:
1. Explanatory Data analysis
2. Data cleaning/preparation with missing value treatment and outlier treatment.
3. Variable reduction techniques:
   (i) Performed Chi square test to select the significant categorical variables.
   (ii) Performed Stepwise linear regression on the entire dataset with the previously selected categorical variables along with all the           continuous variables to further reduce the variables for final model building.
4. Built the final glm with all the significant variables.
5. Applied VIF function to check the Multicollinearity.
6. Applied Concordance function to calculate the concordance rate, discordance rate, somers_D and Gamma.
7. Predicted the response on calibration and validation datsets and created the Confusion matrix to see the accuracy and mis-         classification rate.
8. Performed Decile analysis on both datasets and obtained the KS-score.
9. From KS-score, obtained the cutoff probability for correctly classifying the churnes and non-churners.

Excel file(case study 3 - Logistic regression) contains descriptive stats for all the variables, Final model summary, Decile analysis along with all the graphs(Gains chart,Lift chart,Comparision between calibration and validation churn rate).
