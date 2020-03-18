library(leaps)
library(MASS)
getwd()
setwd('C:/Prashant/UTD Semesters/First/Stats/Project')
student <- read.csv('student-por.csv')
str(student)
student$Grade = student$Grade*5
colnames(student)
student1 <- student
student$absences <- log(student1$absences)
hist(student$absences)
student[which(student$absences == '-Inf'),'absences'] <- 0

#Pre-processing
plot(student$sex, student$Grade)
#wage1$female[wage1$female == 1]
nrow(student[which(student$sex == 'M'),])
nrow(student[which(student$sex == 'F'),])

#age
range(student$age)
nrow(student[which(student$age>19),])

nrow(student[which(student$age==21),])

#Dalc
nrow(student[which(student$Dalc==5),])
plot(student$Dalc, student$Grade)

#Walc
nrow(student[which(student$Walc==5),])
plot(student$Walc, student$Grade)

#address
nrow(student[which(student$address=='U'),])

#romantic
nrow(student[which(student$romantic=='yes'),])

nrow(student[which(student$sex == 'F' & student$Grade<27.5),'Grade'])
range(student[which(student$sex == 'M'),'Grade'])
summary(student[which(student$sex == 'F'),'Grade'])

length(student[student$sex=='M' & student$Grade <27.5,'Grade'])
length(student[student$sex=='M' & student$Grade >87.5,'Grade'])

student <- student[!(student$sex=='F' & student$Grade <20),]
student <- student[!(student$sex=='F' & student$Grade >90),]
student1 <- student
############
#Grades
hist(student[student$Grade>20,'Grade'], col = 'blue3', breaks = c(10,20,30,40,50,60,70,80,90,100),
     xlab = 'Grades', main = 'Frequency Distribution of Grades')

#Weekday alcohol
barplot(table(student$Dalc), col = 'purple3', xlab = 'Alcohol Consumption low to high',
        ylab = 'Frequency', main = 'Workdays Alcohol Consumption')
#Weekend alcohol
barplot(table(student$Walc), col = 'purple', xlab = 'Alcohol Consumption low to high',
        ylab = 'Frequency', main = 'Weekend Alcohol Consumption')

table(student$Dalc)
plot(student$age, student$Grade, main="Distribution before Outlier Removal", xlab='Age',
     ylab = 'Grades', col='blue3')
plot(student$address, student$Grade)
plot(as.factor(student$Dalc), student$Grade, col='dark red', main='Workday Alcohol Consumption Effect on Grades',
     xlab='Alcohol consumption low to high', ylab='Grades in Percentage')
plot(as.factor(student$Walc), student$Grade, col='red', main='Weekend Alcohol Consumption Effect on Grades',
     xlab='Alcohol consumption low to high', ylab='Grades in Percentage')

plot(as.factor(student$Walc), student$Grade, col='red')
hist(student$famrel)

#Model Selection
model1 <- lm(Grade~.,data = student)
m_full <- update(model1, .~. - Walc - Dalc)
c(AIC(m_full),BIC(m_full),olsrr::ols_mallows_cp(m_full, model1))
summary(model1)

m <- lm(Grade ~ ., data=student)
drop1(m, scope=smallest) # what is the best addition to make?

add1(m, scope = biggest)
m1_backwar_aic <- step(model2, direction = 'backward')
m1_backwar_bic <- step(model2, direction = 'backward', k=log(nrow(student)))
summary(m1_backwar_aic)
summary(m1_backwar_bic)
c(AIC(m1_backwar_aic),BIC(m1_backwar_aic),olsrr::ols_mallows_cp(m1_backwar_aic, model2))
c(AIC(m1_backwar_bic),BIC(m1_backwar_bic),olsrr::ols_mallows_cp(m1_backwar_bic, model2))

smallest <- lm(Grade ~ 1, data = student)
biggest <- formula(model2)
m1_forward_aic <- step(smallest, scope=biggest, direction = 'forward')
m1_forward_bic <- step(smallest, scope=biggest, direction = 'forward', k=log(nrow(student)))
summary(m1_forward_aic)
summary(m1_forward_bic)
c(AIC(m1_forward_aic),BIC(m1_forward_aic),olsrr::ols_mallows_cp(m1_forward_aic, model2))
c(AIC(m1_forward_bic),BIC(m1_forward_bic),olsrr::ols_mallows_cp(m1_forward_bic, model2))

model5 <- lm(Grade~(address+Medu+studytime+failures+higher+sex+age+romantic)^2,
             data = student)
m1_backwar_bic_int <- step(model5, direction = 'backward', k=log(nrow(student)))
summary(m1_backwar_bic_int)
model_final <- update(m1_backwar_bic_int, .~. + Walc + Dalc)
summary(model_final)
anova(m1_backwar_bic_int, model_final)
model_walc <- update(m1_backwar_bic_int, .~. + Walc)
model_dalc <- update(m1_backwar_bic_int, .~. + Dalc)
anova(m1_backwar_bic_int, model_dalc)
summary(model_dalc)
summary(model_walc)
cor(student$Dalc, student$Walc)

stepAIC()
summary(m1)
model2_ols <- olsrr::ols_mallows_cp(model2, model1)
model3 <- step(model2)
summary(model1)
summary(model3)
summary(model4)
c(AIC(model3),BIC(model3), olsrr::ols_mallows_cp(model3, model2))
model4 <- update(model4, .~. + goout)

c(AIC(model4),BIC(model4), olsrr::ols_mallows_cp(model4, model2))

model5 <- lm(Grade~(sex+age+address+Medu+studytime+failures+higher+
                      internet+romantic+health)^2, data = student)
summary(model5)
model6 <- step(model5)
c(AIC(model6),BIC(model6), olsrr::ols_mallows_cp(model6, model2))
summary(model7)

model7 <- update(model7, .~. - higher)

c(AIC(model7),BIC(model7), olsrr::ols_mallows_cp(model7, model2))

model8 <- lm(Grade~(sex+age+Medu+studytime+failures+
                      internet)^2, data = student)
summary(model8)

a<- log(student1$absences)
hist(a, xlab = 'Number of Absences', main = 'After Transformation',
     col = 'dark green')
hist(student1$absences)
hist(student$absences, xlab = 'Number of Absences', main = 'After Transformation',
     col = 'dark green')
hist(student$Dalc)
hist(student$Walc)
hist(student$Grade)
plot(student$absences, student$Grade)
plot(fitted.values(model1), residuals(model1))
