attach(Household_Pulse_data)

data.frame(EEDUC,GENID_DESCRIBE)
#Created a data frame to visualize the amount of male and females for having an education

#I chose to look at the subgroup gender,education and kids to see if there are any differences among the children whether their male or female in terms of edcuation if some score higher or lower.

#Null Hypothesis:That there are no differences among the gender of the children and the type of education being received.

#Alternative Hypothesis:That there are differences among the gender of the children and the type of education being received.


summary(GENID_DESCRIBE)#26493/45584 In a sample of only between male and females there is a percatage of 58% females. While males are only 42% that participated in the survey.
summary(EEDUC)#There are a total of 46801 participants that are adults that stated their education status. This data will be used to see how a parent education level affects their child.
summary(KIDS_LT5Y)
summary(KIDS_5_11Y)
summary(KIDS_12_17Y)




norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
}

gender <- as.factor(GENID_DESCRIBE)
kids <- norm_varb(Num_kids_homeschool+Num_kids_Priv_School+Num_kids_Pub_School)
educ <- (EEDUC=='less than hs') * 1 + (EEDUC=='some hs')*2+(EEDUC=='HS diploma')*3+(EEDUC=='some coll')*4+(EEDUC=='assoc deg')*5 + (EEDUC=='bach deg')*6+(EEDUC=='adv deg')*7

norm_edu <- norm_varb(educ)
norm_kids <- norm_varb(kids)

#Created norm varbs so the Knn classifier can run smoothly the code
#A knn classifier is machine learning algorithm that assumes similairty between data.


data_use_prelim <- cbind(educ,kids)
data_use_prelim <- data.frame(data_use_prelim)

good_obs_data_use <- complete.cases(data_use_prelim,gender)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(gender,good_obs_data_use)
gender0 <- complete.cases(data_use_prelim, gender)
summary(data_use_prelim)

#To see the minimum and maximum values of education and children

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]

summary(cl_data)
prop.table(summary(cl_data))
summary(train_data)
require(class)
for (indx in seq(1,17, by= 2)) {
  pred_gender <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_gender == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

#The knn classifier shows that it is 55% to 63% accurate in grouping data that are very similar

cl_data_n <- as.numeric(cl_data)

model_ols1 <- lm(cl_data_n ~  train_data$kids + train_data$educ)
summary(model_ols1)
y_hat <- fitted.values(model_ols1)

# it is demonstrated by the model ols regression that kids and education are not statisically significant. Therefore, we can state to reject the alternative hypothesis and we have to accept the null hypotheis as it was determined there are no differences.
#This shows the gender, education level of parents and the type of school children are enrolled does not affect the perfromance of the student.

mean(y_hat[cl_data_n == 1])
mean(y_hat[cl_data_n == 2])


cl_data_n1 <- as.numeric(cl_data_n == 1)
model_ols_v1 <- lm(cl_data_n1 ~ train_data$kids + train_data$educ)
y_hat_v1 <- fitted.values(model_ols_v1)
mean(y_hat_v1[cl_data_n1 == 1])
mean(y_hat_v1[cl_data_n1 == 0])

#









