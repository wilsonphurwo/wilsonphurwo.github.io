setwd("C:/Users/wilso/Documents/2. UIUC/7. 2020 Spring/ASRM 499/Final Project")
load("LC.rda")

#######################################################
####################MODELS#############################
#######################################################

#logistic regression explanation
horseshoe = read.table("horseshoe.txt", header = TRUE)
summary(glm(y ~ width, family = "binomial", data = horseshoe))
library(ggplot2)
ggplot(horseshoe, aes(x = width, y = y)) + 
        geom_point() +
        labs(x = "Width", y = "Satellite") +
        stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

#knn explanation
library(caret)
horseshoe.1nn = knn3(y ~ width, data = horseshoe, k = 1)
horseshoe.5nn = knn3(y ~ width, data = horseshoe, k = 5)
horseshoe.20nn = knn3(y ~ width, data = horseshoe, k = 20)
horseshoe.50nn = knn3(y ~ width, data = horseshoe, k = 50)
horseshoe.sat.prob.1nn = predict(horseshoe.1nn, data.frame(width = seq(min(horseshoe$width), max(horseshoe$width), by = 0.1)), type = c("prob"))[,2]
horseshoe.sat.prob.5nn = predict(horseshoe.5nn, data.frame(width = seq(min(horseshoe$width), max(horseshoe$width), by = 0.1)), type = c("prob"))[,2]
horseshoe.sat.prob.20nn = predict(horseshoe.20nn, data.frame(width = seq(min(horseshoe$width), max(horseshoe$width), by = 0.1)), type = c("prob"))[,2]
horseshoe.sat.prob.50nn = predict(horseshoe.50nn, data.frame(width = seq(min(horseshoe$width), max(horseshoe$width), by = 0.1)), type = c("prob"))[,2]
horseshoe.knn.pred = data.frame(width = seq(min(horseshoe$width), max(horseshoe$width), by = 0.1), pred.1nn = horseshoe.sat.prob.1nn, pred.5nn = horseshoe.sat.prob.5nn, pred.20nn = horseshoe.sat.prob.20nn, pred.50nn = horseshoe.sat.prob.50nn)

ggplot(horseshoe.knn.pred, aes(x = width, y = pred.1nn)) + geom_point(color = "orange") + labs(x = "Width", y = "Probability")
ggplot(horseshoe.knn.pred, aes(x = width, y = pred.5nn)) + geom_point(color = "orange") + labs(x = "Width", y = "Probability")
ggplot(horseshoe.knn.pred, aes(x = width, y = pred.20nn)) + geom_point(color = "orange") + labs(x = "Width", y = "Probability")
ggplot(horseshoe.knn.pred, aes(x = width, y = pred.50nn)) + geom_point(color = "orange") + labs(x = "Width", y = "Probability")

#checking which variables are numerical and categorical
vars = unlist(lapply(data, class))
table(vars)
vars

#######################################################
#######################EDA#############################
#######################################################

#checking missing values
vars.na.count = data.frame(na.count = sort(colSums(is.na(data)), decreasing = TRUE)[1:20])
ggplot(data = vars.na.count, aes(x = reorder(rownames(vars.na.count), -na.count), y = na.count)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        geom_text(aes(label = na.count), hjust = 1.2, color = "white", size = 2.3) +
        labs(x = "Variable Name", y = "Number of Missing Values")

#removing variables with more than 800,000 missing values
data.trim = subset(data, select = -c(mths_since_last_record, mths_since_recent_bc_dlq, mths_since_last_major_derog, mths_since_recent_revol_delinq, mths_since_last_delinq, il_util, mths_since_rcnt_il, all_util, inq_last_12m, total_cu_tl, open_acc_6m, inq_fi, max_bal_bc, open_rv_24m, open_rv_12m, total_bal_il, open_il_24m, open_il_12m, open_act_il))

options(scipen = 999)
vars.trim.na.count = data.frame(na.count = sort(colSums(is.na(data.trim)), decreasing = TRUE)[1:20])
ggplot(data = vars.trim.na.count, aes(x = reorder(rownames(vars.trim.na.count), -na.count), y = na.count)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        geom_text(aes(label = na.count), hjust = 1.2, color = "white", size = 2.3) +
        labs(x = "Variable Name", y = "Number of Missing Values")

#removing all rows with missing value in at least one variable
data.clean = data.trim[complete.cases(data.trim),]
vars.clean = unlist(lapply(data.clean, class))
table(vars.clean)

#loan status
ggplot(data.clean, aes(x = loan_status)) +
        geom_bar(aes(y = ..count..)) +
        geom_text(stat = "count", aes(label = ..count..), vjust = -1, size = 5) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 20)) +
        labs(x = "Loan Status", y = "Count") + ylim(0, 850000)

data.clean$loan_status.b = ifelse(data.clean$loan_status %in% c("Current", "Fully Paid", "In Grace Period"), "Non-Defaulted", "Defaulted")

ggplot(data.clean, aes(x = loan_status.b)) +
        geom_bar(aes(y = ..count..)) +
        geom_text(stat = "count", aes(label = ..count..), vjust = 1.5, size = 5, color = "white") +
        labs(x = "Binary Loan Status", y = "Count")

#loan amount
ggplot(data.clean, aes(x = loan_amnt)) +
        geom_histogram(binwidth = 5000, colour = "black", fill = "white") +
        labs(x = "Loan Amount ($)", y = "Count")
summary(data.clean$loan_amnt)

#interest rate
ggplot(data.clean, aes(x = int_rate)) +
        geom_histogram(binwidth = 2, colour = "black", fill = "white") +
        labs(x = "Interest Rate (%)", y = "Count")
summary(data.clean$int_rate)

#loan grade
ggplot(data.clean, aes(x = grade)) +
        geom_histogram(stat = "count", colour = "black", fill = "white") +
        labs(x = "Loan grade assigned by Lending Club", y = "Count")

library(scattermore)
ggplot(data.clean) +
        geom_scattermore(aes(x = grade, y = int_rate, color = grade), pointsize = 2) +
        labs(x = "Loan grade assigned by Lending Club", y = "Interest Rate (%)") +
        theme(legend.position = "none")
table2 = table(data.clean$grade, data.clean$loan_status.b)
round(prop.table(table2, 2) * 100, 2)
table3 = aggregate(int_rate ~ loan_status.b + grade, data = data.clean, mean)
xtabs(int_rate ~ grade + loan_status.b, data = table3)

#purpose
ggplot(data.clean, aes(x = purpose)) +
        geom_histogram(stat = "count", colour = "black", fill = "white") +
        geom_text(stat = "count", aes(label = ..count..), vjust = -0.3, size = 5) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12)) +
        labs(x = "Purpose of loan", y = "Count")
table1 = table(data.clean$purpose, data.clean$loan_status.b)
round(prop.table(table1, 2) * 100, 2)

#verification status
ggplot(data.clean, aes(x = verification_status)) +
        geom_histogram(stat = "count", colour = "black", fill = "white") +
        geom_text(stat = "count", aes(label = ..count..), vjust = 1.2, size = 5) + 
        labs(x = "Verification Status", y = "Count")
table4 = table(data.clean$verification_status, data.clean$loan_status.b)
round(prop.table(table4, 2) * 100, 2)

#application type
ggplot(data.clean, aes(x = application_type)) +
        geom_histogram(stat = "count", colour = "black", fill = "white") +
        geom_text(stat = "count", aes(label = ..count..), vjust = 1.2, size = 5) + 
        labs(x = "Application Type", y = "Count")
table5 = table(data.clean$application_type, data.clean$loan_status.b)
round(prop.table(table5, 1) * 100, 2)

ggplot(data.clean, aes(x = grade, y = loan_amnt, fill = application_type)) +
        geom_boxplot() +
        labs(x = "Loan grade assigned by Lending Club", y = "Loan Amount ($)", fill = "Application Type")
ggplot(data.clean, aes(x = grade, y = int_rate, fill = application_type)) +
        geom_boxplot() +
        labs(x = "Loan grade assigned by Lending Club", y = "Interest Rate (%)", fill = "Application Type")

#employment length
table6 = table(data.clean$emp_length, data.clean$loan_status.b)
round(prop.table(table6, 2) * 100, 2)

#term
table7 = table(data.clean$term, data.clean$loan_status.b)
round(prop.table(table7, 1) * 100, 2)

#home ownership
table8 = table(data.clean$home_ownership, data.clean$loan_status.b)
round(prop.table(table8, 2) * 100, 2)

#######################################################
###################Relevel#############################
#######################################################

data.clean = data.clean[!(data.clean$home_ownership == "ANY"),]
data.clean = data.clean[!(data.clean$home_ownership == "NONE"),]
data.clean = data.clean[!(data.clean$home_ownership == "OTHER"),]

data.clean$disbursement_method = factor(data.clean$disbursement_method)
data.clean$hardship_flag = factor(data.clean$hardship_flag)
data.clean$application_type = factor(data.clean$application_type)
data.clean$initial_list_status = factor(data.clean$initial_list_status)
data.clean$purpose = factor(data.clean$purpose)
data.clean$pymnt_plan = factor(data.clean$pymnt_plan)
data.clean$verification_status = factor(data.clean$verification_status)
data.clean$home_ownership = factor(data.clean$home_ownership)
data.clean$emp_length = factor(data.clean$emp_length, levels = c("< 1 year", "1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10+ years"))
data.clean$grade = factor(data.clean$grade)
data.clean$term = factor(data.clean$term)
data.clean$loan_status.b = factor(data.clean$loan_status.b, levels = c("Non-Defaulted", "Defaulted"))

#######################################################
############Convert Categorical to Dummies#############
#######################################################

data.clean.d = fastDummies::dummy_cols(data.clean, select_columns = c("disbursement_method", "hardship_flag", "application_type", "initial_list_status", "purpose", "pymnt_plan", "verification_status", "home_ownership", "emp_length", "grade", "term"), remove_first_dummy = TRUE)
data.clean.d = subset(data.clean.d, select = -c(disbursement_method, hardship_flag, application_type, initial_list_status, purpose, pymnt_plan, verification_status, home_ownership, emp_length, grade, term, loan_status))
data.clean.d$loan_status.b = ifelse(data.clean.d$loan_status.b == "Non-Defaulted", 0, 1)

#######################################################
############Near Zero Var##############################
#######################################################

library(caret)

##### removing predictors that have near-zero variance
nearZeroVar(data.clean.d)
data.clean.d = data.clean.d[,-c(5,8,10,11,12,30,31,38,39,40,44,53)]


#######################################################
############Data Slicing###############################
#######################################################

library(caret)

set.seed(5396) #for reproducibility
train_idx <- createDataPartition(y = data.clean.d$loan_status.b, p = 0.7, list = FALSE)
train <- data.clean.d[train_idx,]
test <- data.clean.d[-train_idx,]

#as train data still consists of 1,171,456 rows, we need to partition more so computes faster
train_idx_more <- createDataPartition(y = train$loan_status.b, p = 0.01, times = 10)
test_idx_more <- createDataPartition(y = test$loan_status.b, p = 0.01)

#######################################################
############Penalized Est Implementation###############
#######################################################

library(glmnet)
library(plotmo)

train_more1 <- train[train_idx_more[[1]],]
test_more1 <- train[test_idx_more[[1]],]

x1 <- model.matrix(loan_status.b ~ ., train_more1)[,-1]
y1 <- train_more1$loan_status.b

x1.test <- model.matrix(loan_status.b ~ ., test_more1)[,-1]
y1.test <- test_more1$loan_status.b

model1.1<-glmnet(x=x1,y=y1,family="binomial", alpha=1)
model1.2<-glmnet(x=x1,y=y1,family="binomial", alpha=0)
model1.3<-glmnet(x=x1,y=y1,family="binomial", alpha=0.5)
print(model1.1)
print(model1.2)
print(model1.3)

options(scipen = 999)
cvmodel1.1<-cv.glmnet(x=x1,y=y1,family="binomial", alpha=1)
cvmodel1.2<-cv.glmnet(x=x1,y=y1,family="binomial", alpha=0)
cvmodel1.3<-cv.glmnet(x=x1,y=y1,family="binomial", alpha=0.5)

as.data.frame(as.matrix(cbind(coef(cvmodel1.1, s='lambda.min'), coef(cvmodel1.2, s='lambda.min'), coef(cvmodel1.3, s='lambda.min'))))

plot_glmnet(model1.1)
plot_glmnet(model1.2)
plot_glmnet(model1.3)

model1.1<-glmnet(x=x1,y=y1,family="binomial", alpha=1, lambda=cvmodel1.1$lambda.min)
model1.2<-glmnet(x=x1,y=y1,family="binomial", alpha=0, lambda=cvmodel1.2$lambda.min)
model1.3<-glmnet(x=x1,y=y1,family="binomial", alpha=0.5, lambda=cvmodel1.3$lambda.min)

model1.1.pred <- ifelse(predict(model1.1, x1.test, type = "response") > 0.5, 1, 0)
model1.2.pred <- ifelse(predict(model1.2, x1.test, type = "response") > 0.5, 1, 0)
model1.3.pred <- ifelse(predict(model1.3, x1.test, type = "response") > 0.5, 1, 0)

confusionMatrix(factor(y1.test), factor(model1.1.pred))
confusionMatrix(factor(y1.test), factor(model1.2.pred))
confusionMatrix(factor(y1.test), factor(model1.3.pred))

#######################################################
#################AIC elimination#######################
#######################################################

library(MASS)
library(dplyr)

model2.1 <- glm(loan_status.b ~ ., data = train_more1, family = "binomial") %>% stepAIC(trace = FALSE)

model2.2 <- glm(loan_status.b ~ total_bc_limit + pub_rec_bankruptcies + 
                        percent_bc_gt_75 + num_sats + num_accts_ever_120_pd + mths_since_recent_inq + 
                        mo_sin_rcnt_tl + bc_open_to_buy + last_credit_pull_d + last_pymnt_amnt + 
                        total_rec_int + total_rec_prncp + total_pymnt + out_prncp_inv + 
                        revol_util + pub_rec + open_acc + installment + int_rate + 
                        funded_amnt_inv + hardship_flag_Y + home_ownership_OWN + home_ownership_RENT,
                        data = train_more1, family = "binomial")
model2.3 <- glm(loan_status.b ~ total_bc_limit + pub_rec_bankruptcies + 
                        percent_bc_gt_75 + num_sats + num_accts_ever_120_pd + mths_since_recent_inq + 
                        mo_sin_rcnt_tl + bc_open_to_buy + last_credit_pull_d + last_pymnt_amnt + 
                        total_rec_int + total_rec_prncp + total_pymnt + out_prncp_inv + 
                        revol_util + pub_rec + open_acc + installment + int_rate + 
                        funded_amnt_inv + hardship_flag_Y + purpose_credit_card +
                        purpose_debt_consolidation + purpose_educational + purpose_home_improvement +
                        purpose_house + purpose_major_purchase + purpose_medical +
                        purpose_moving + purpose_other + purpose_renewable_energy + 
                        purpose_small_business + purpose_vacation + purpose_wedding +
                        home_ownership_OWN + home_ownership_RENT,
                        data = train_more1, family = "binomial")

model2.2.pred <- ifelse(predict(model2.2, test_more1, type = "response") > 0.5, 1, 0)
model2.3.pred <- ifelse(predict(model2.3, test_more1, type = "response") > 0.5, 1, 0)

confusionMatrix(factor(test_more1$loan_status.b), factor(model2.2.pred))
confusionMatrix(factor(test_more1$loan_status.b), factor(model2.3.pred))

#######################################################
######################KNN##############################
#######################################################

library(caret)

set.seed(5396)
trCtrl <- train(method = "repeatedcv", number = 1, repeats = 1)

model3.1 <- train(factor(loan_status.b) ~ total_bc_limit + pub_rec_bankruptcies + 
                percent_bc_gt_75 + num_sats + num_accts_ever_120_pd + mths_since_recent_inq + 
                mo_sin_rcnt_tl + bc_open_to_buy + last_credit_pull_d + last_pymnt_amnt + 
                total_rec_int + total_rec_prncp + total_pymnt + out_prncp_inv + 
                revol_util + pub_rec + open_acc + installment + int_rate + 
                funded_amnt_inv + hardship_flag_Y + home_ownership_OWN + home_ownership_RENT,
                data = train_more1, method = "knn", preProcess = c("center", "scale"),
                tuneLength = 30)
model3.2 <- train(loan_status.b ~ total_bc_limit + pub_rec_bankruptcies + 
                percent_bc_gt_75 + num_sats + num_accts_ever_120_pd + mths_since_recent_inq + 
                mo_sin_rcnt_tl + bc_open_to_buy + last_credit_pull_d + last_pymnt_amnt + 
                total_rec_int + total_rec_prncp + total_pymnt + out_prncp_inv + 
                revol_util + pub_rec + open_acc + installment + int_rate + 
                funded_amnt_inv + hardship_flag_Y + purpose_credit_card +
                purpose_debt_consolidation + purpose_educational + purpose_home_improvement +
                purpose_house + purpose_major_purchase + purpose_medical +
                purpose_moving + purpose_other + purpose_renewable_energy + 
                purpose_small_business + purpose_vacation + purpose_wedding +
                home_ownership_OWN + home_ownership_RENT,
                data = train_more1, method = "knn", preProcess = c("center", "scale"),
                tuneLength = 30)

model3.1.pred <- predict(model3.1, x1.test)
model3.2.pred <- ifelse(predict(model3.2, x1.test) > 0.5, 1, 0)

confusionMatrix(factor(model3.1.pred), factor(y1.test))
confusionMatrix(factor(model3.2.pred), factor(y1.test))

#######################################################
######################Question 2#######################
#######################################################

setwd("C:/Users/wilso/Documents/2. UIUC/7. 2020 Spring/ASRM 499/Final Project")
load("LCwithissuedate.rda")

datanew$issue_m <- format(as.yearmon(as.Date("2018-12-01")) - round(datanew$issue_d * 12) * (1/12), 
                        "%Y-%m")

accepted_m <- c("2012-12","2013-01","2013-02","2013-03","2013-04","2013-05",
                "2013-06","2013-07","2013-08","2013-09","2013-10")

datanew.a <- datanew[which(datanew$issue_m %in% accepted_m),]
datanew.a$loan_status.b <- factor(ifelse(datanew.a$loan_status %in% c("Fully Paid", "Current",
                                                               "In Grade Period", "Late (16-30 days)"),
                                  0,1), levels = c(0, 1))

datanew.b <- subset(datanew.a, select = c(loan_status.b, loan_amnt, issue_m, grade, term))

datanew.c <- datanew.b[which(datanew.b$loan_amnt >= 5000 & datanew.b$loan_amnt <= 20000),]

datanew.d <- datanew.c[which(datanew.c$term %in% "36 months"),]

datanew.d$D <- ifelse(datanew.d$loan_amnt >= 12000 & datanew.d$loan_amnt < 16000 & as.yearmon(datanew.d$issue_m) >= as.yearmon("2013-03"), 1,
                      ifelse(datanew.d$loan_amnt >= 10000 & datanew.d$loan_amnt < 12000 & as.yearmon(datanew.d$issue_m) >= as.yearmon("2013-07"), 1, 0))

datanew.d$bin <- cut(datanew.d$loan_amnt, breaks = 1000*c(5:20), labels = c(1:15), include.lowest = TRUE)

set.seed(5396) #for reproducibility
train_idx <- createDataPartition(y = datanew.d$loan_status.b, p = 0.7, list = FALSE)
train <- datanew.d[train_idx,]
test <- datanew.d[-train_idx,]

model4.1 <- glm(loan_status.b ~ factor(grade):factor(issue_m) + D + bin, family = "binomial", data = train)

test <- test[which(test$grade != "G"),]
model4.1.pred <- ifelse(predict(model4.1, test, type = "response") > 0.1, 1, 0)

confusionMatrix(factor(test$loan_status.b), factor(model4.1.pred))
