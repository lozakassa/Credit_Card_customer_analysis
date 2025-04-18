# Predictive Analysis Project for credit card customer analysis

Welcome to my Predictive Analysis project! This repository showcases my skills in data processing, predictive modeling, and evaluating model performance. The goal of this project is to predict customer behavior (whether a customer will be labeled as "bad" or "good") using various financial features.

---

## What’s Inside

### 1. **Main Script**
   - The **`predictive_analysis.R`** script contains all the necessary steps, including:
     - Data cleaning and filtering
     - Feature engineering
     - Logistic Regression Model
     - Model validation with ROC and K-S statistics

### 2. **Packages Used**
   - **`ggplot2`**: For visualizations
   - **`caret`**: For data splitting and modeling
   - **`rpart` & `maptree`**: For decision trees (if applicable)
   - **`ROCR`**: For evaluating model performance with ROC and AUC
   - **`gains`**: For analyzing model performance with gains charts

---

## How to Run the Code

1. **Set your working directory**:
   Make sure to set your working directory where your dataset is located:

   ```r
   setwd("path/to/your/data")
