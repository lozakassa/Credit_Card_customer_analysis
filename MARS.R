#Libraries
library(ggplot2)
library(readr)
library(gains)
library(ROCR)
library(car)
library(caret)
library(rpart)
library(maptree)
library(rpart.plot)
library(tidyr)
library(dplyr)
library(MASS)

#Reading in Data 
setwd("C:/Users/Union Loaner #06/Desktop/Spring 2025/predictive analysis/midterm")
df <- read_csv("/Users/Union Loaner #06/Desktop/Spring 2025/predictive analysis/midterm/April Vintage 2025 edition.csv", 
               col_types = cols(
                 OpenDate = col_date(format = "%m/%d/%Y"),
                 DatePrevStmt = col_date(format = "%m/%d/%Y"),
                 StatementDt = col_date(format = "%m/%d/%Y")
               ))
df <- df %>% distinct()

#Filter Data to remove any MOB over 14 
data_filtered<- df %>%
  filter(MOB <= 14)

#Filtering the data for the analysis only grabbing the max MOB which is either 12 or the latest record if the account did not make 12 months and then the first 4 months 0-4 
data_filtered_2 <- data_filtered%>%
  group_by(ID) %>%
  filter(MOB == max(MOB) | MOB <= 5) %>%
  ungroup()

#Making Bad FLag with last obs month
last_obs <- data_filtered_2 %>%
  group_by(ID) %>%
  filter(StatementDt == max(StatementDt, na.rm = TRUE)) %>%
  ungroup()
last_obs <- last_obs %>%
  mutate(Bad = case_when(
    MOB == 14 & (DaysDeliq = 0 & ( is.na(ExtStatus) | (ExtStatus %in% c("", "C")) )) ~ 0,
    MOB < 14 & ExtStatus == "Z" ~ 1,
    ((ExtStatus == 'C'| is.na(ExtStatus)) & (IntStatus == 'N' | is.na(IntStatus))) ~ 0,
    TRUE ~ 1
  ))
#Replacing Na's
data_filtered_2 <- data_filtered_2 %>%
  mutate(
    ExtStatus = replace_na(ExtStatus, "ND"),
    IntStatus = replace_na(IntStatus, "ND")
  )
early_dead <- ifelse(last_obs$MOB <= 5, 1, 0)
last_obs$early_dead <- early_dead

month_5 <- ifelse(data_filtered_2$MOB == 5 & data_filtered_2$IntStatus == "ND" & data_filtered_2$ExtStatus == "ND" & data_filtered_2$DaysDeliq == 0, 1,0)
test_ <- data_fitered_2 %>%
  left_join(
    last_obs %>% dplyr::select(ID, Bad),
    by = "ID"
  )
View(test_)
data_filtered_2$month_5 = month_5

good_month5 <- data_filtered_2$ID[which(data_filtered_2$month_5 == 1)]

data_filtered_3 <- subset(data_filtered_2, ID %in% good_month5)

IDs <- unique(data_filtered_3$ID)

data_4 <- subset(data_filtered_3, MOB <= 5)

outcomes <- subset(data_filtered_3, MOB > 5)

outcomes <- outcomes[!duplicated(outcomes$ID),]

bad <- ifelse(outcomes$DaysDeliq > 0 | outcomes$ExtStatus != "ND" | outcomes$IntStatus != "ND", 1,0)

outcomes$bad <- bad

outcomes <- outcomes[,c(1,25)]

# Aggregate behavior up to month 5
agg_data <- data_4 %>%
  group_by(ID) %>%
  reframe(
    avg_days_deliq = mean(DaysDeliq),
    max_days_deliq = max(DaysDeliq),
    total_fees = sum(TotalFeesBilled),
    total_payments = sum(TotalNetPayments),
    total_purchases = sum(TotalNetPurchaseAndCash),
    ending_balance = last(EndingBalance),
    opening_balance = first(OpeningBalance),
    credit_limit = last(CreditLimit),
    change_credit_score = last(QuarterlyCreditScore) - nth(QuarterlyCreditScore,2),
    PaymentRatio = ifelse(OpeningBalance > 0, TotalNetPayments / OpeningBalance, 0),
    PurchaseToPaymentRatio = ifelse(TotalNetPayments > 0, TotalNetPurchaseAndCash / TotalNetPayments, 0),
    BalanceChange = EndingBalance - OpeningBalance,
    FeeBurden = ifelse(EndingBalance > 0, TotalFeesBilled / EndingBalance, 0),
    UtilizationRatio= ifelse(CreditLimit > 0, EndingBalance / CreditLimit, 1),
    MOB = MOB
    
  ) %>%
  ungroup()
agg_data_2 <- subset(agg_data, MOB == 5)

unique(agg_data_2$ID)
agg_data_2$ID[duplicated(agg_data_2$ID)]

duplicated(agg_data_2$ID)
agg_data_3 <- subset(agg_data_2, ID %in% agg_data_2$ID[duplicated(agg_data_2$ID)])

agg_data_2 <- agg_data_2[!duplicated(agg_data_2$ID),]

inputs <- subset(data_4, MOB == 5)

inputs <- inputs[!duplicated(inputs$ID),]


model_data <- merge(inputs,agg_data_2, by = "ID")

model_data <- merge(model_data, outcomes, by = "ID")



ggplot(model_data, aes(x = factor(bad))) + geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Bad Customers", x = "Bad (1=Yes, 0=No)")


# Data Split
set.seed(2025)
train_index <- createDataPartition(model_data$bad, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Formulating Logistic Regression Model 
logit_full1 <- glm(bad ~avg_days_deliq + max_days_deliq + total_fees + total_payments+total_purchases+ ending_balance +credit_limit+change_credit_score+PaymentRatio+BalanceChange+FeeBurden+UtilizationRatio
                   , data = train_data %>% dplyr::select(-ID), family = binomial)
summary(logit_full1)

logit_step1 <- stepAIC(logit_full1, direction = "both", trace = FALSE)
summary(logit_step1)

logit_final <- glm(formula = bad ~ max_days_deliq + total_fees + total_payments + total_purchases + UtilizationRatio,
  family = binomial,
  data = train_data %>% dplyr::select(-ID)
)
summary(logit_final)

logit_pred <- predict(logit_final, newdata = test_data %>% dplyr::select(-ID), type = "response")
library(ROCR)
pred <- prediction(logit_pred, test_data$bad)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
abline(a = 0, b = 1, lty = 2)  # Add diagonal line
auc <- performance(pred, "auc")
auc@y.values

logitgains <- gains(test_data$bad, logit_pred, 10)
plot(logitgains, cex = 0.5)
# Create prediction object
mypreds <- prediction(logit_pred, test_data$bad)

# Get performance metrics
roc_perf <- performance(mypreds, measure = "tpr", x.measure = "fpr")

# Calculate K-S statistic
tpr_values <- roc_perf@y.values[[1]]
fpr_values <- roc_perf@x.values[[1]]
ks_statistic <- max(abs(tpr_values - fpr_values))

# Plot the K-S Curve
plot(roc_perf, col = "blue", lwd = 2, main = "K-S Plot")
abline(h = 0, col = "red", lty = 2)  # Adding the horizontal line at 0
text(0.2, 0.9, paste("K-S Statistic =", round(ks_statistic, 3)), cex = 1)

# Optional: Show KS values
print(paste("K-S Statistic: ", round(ks_statistic, 3)))

