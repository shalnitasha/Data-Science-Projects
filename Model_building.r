#######################
####library loading####
#######################
library(tidyverse)
library(mice)
library(skimr)
library(caret)
library(MASS)
library(pROC)
library(ROCR)
library(glmnet)
library(car)
library(ggcorrplot)
library(DataExplorer)
library(reshape2)
library(broom)

####################
####data loading####
####################

setwd("~/kaggle_heartdisease")
raw<-read.csv('data.csv')
str(raw)
glimpse(raw)
summary(raw)

names(raw)<-c('age','sex','chest_pain_type','resting_blood_pressure','cholesterol','fasting_blood_sugar',
              'rest_ecg','max_heart_rate','exercise_induced_angina','st_depression', 'st_slope', 
              'num_major_vessels', 'thalassemia', 'target')
unique(raw)

raw<-raw[!duplicated(raw),]
#############################
####missing data analysis####
#############################
md.pattern(raw)
plot_missing(raw)


############################
####correlation analysis####
############################

cor_raw = cor(raw)
ggcorrplot(cor_raw,lab = T)
high_cor = melt(abs(cor(raw))>0.7)
head(high_cor)
ggplot(high_cor, aes(x = Var1, y=Var2, fill=as.numeric(value))) + geom_tile() +
  geom_text(aes(Var1, Var2, label=as.numeric(value)),color='white',size=2)+
  scale_color_gradient(low='blue',high='red') +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

###############
####outlier####
###############

# < 150 mg/dL: normal	150-199 mg/dL: borderline to high	200-499mg/dL: high	> 500 mg/dL: very high
boxplot(raw[,c('age','max_heart_rate','cholesterol','resting_blood_pressure')])

###########################
####feature engineering####
###########################

#recode to characters
raw = mutate(raw, sex = ifelse( sex == 1, 'Male', 'Female'))
raw = mutate(raw, chest_pain_type = ifelse(chest_pain_type == 0,'Typical Angina',ifelse(chest_pain_type == 1, 
                                                              "Atypical Angina", ifelse(chest_pain_type == 2, 'Non Anginal pain', 'Asymptomatic' ))))
raw = mutate(raw,fasting_blood_sugar = ifelse(fasting_blood_sugar == 1, '> 120 mg/dl', '< 120 mg/dl'))
raw = mutate(raw, rest_ecg = ifelse(rest_ecg == 0, 'Normal',
                                   ifelse(rest_ecg == 1, 'ST-T wave abnormal','probable or definite LVH')))
raw = mutate(raw, exercise_induced_angina = ifelse(exercise_induced_angina == 1, 'Yes', 'No'))
raw = mutate(raw, st_slope = ifelse( st_slope == 0, 'Upsloping', ifelse(st_slope == 1, 'Flat', 'Downsloping')))
raw = mutate(raw, thalassemia = ifelse( thalassemia == 3, 'Fixed defect',ifelse( thalassemia == 2, 'Irreversible defect','normal')))
raw = mutate(raw , target = (ifelse( target == 1, 'Yes', 'No')))


raw_var<-raw[,-c(14)]
chr_var <- raw_var[,sapply(raw_var,is.character)==TRUE]
int_var <- raw_var[,sapply(raw_var,is.character)==FALSE]
dummies <- dummyVars(~., raw_var[names(chr_var)],fullRank=T)
chr_var <- predict(dummies, raw_var[names(chr_var)])
raw<-cbind(int_var,chr_var,target=raw$target)

str(raw)
#change to character to factor variables
raw<-raw%>%mutate_if(is.character, as.factor)
skim(raw)





###########################
####make model datasets####
###########################

set.seed(123)
index <- createDataPartition(raw$target,p=0.8,list=F)
train <- raw[index,]
test <-  raw[-index,]


train_x <- train %>% dplyr::select(-target)
train_y <- train$target

test_x <- test %>% dplyr::select(-target)
test_y <- test$target


#####################
####model bulding####
#####################

#base model
mod.log<-glm(target~., data = train, family="binomial")
summary(mod.log)
prob.train <- mod.log %>% predict(train_x,type ="response")
predicted.train <- ifelse(prob.train>0.5,'Yes','No')
prob.test <- mod.log %>% predict(test_x,type ="response")
predicted.test <- ifelse(prob.test>0.5,'Yes','No')

ROCRpred.test = prediction(prob.train, train$target)
auc = as.numeric(performance(ROCRpred.test, 'auc')@y.values)
auc

mean(predicted.test==test_y)
table(test$target, prob.test>0.5)
ROCRpred = prediction(prob.test, test$target)
auc = as.numeric(performance(ROCRpred, 'auc')@y.values)
auc


mean(predicted.train==train_y)
table(train$target, prob.train>0.5)

mean(predicted.test==test_y)
table(test$target, prob.test>0.4)

  confusionMatrix(data = as.factor(predicted.train),
                reference = train_y, positive = "Yes")


confusionMatrix(data = as.factor(predicted.test),
                reference = test_y, positive = "Yes")

exp(coef(mod.log))



plot(performance(ROCRpred,"tpr","fpr"), colorize=TRUE,main='ROC Curve',print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
auc_glm=round(as.numeric(performance(ROCRpred, "auc")@y.values), digits = 3)
auc_glm
# library(lmtest)
# lrtest(mod.log, mod.log.train$finalModel)


#logistic model 2


aic.model <- mod.log %>% stepAIC(trace = T,direction="both")
step.model<-glm(target~age+max_heart_rate+st_depression+num_major_vessels+sexMale+`chest_pain_typeAtypical Angina`+
                  `chest_pain_typeTypical Angina`+`rest_ecgST-T wave abnormal`+exercise_induced_anginaYes+st_slopeFlat+`thalassemiaIrreversible defect`, data = train, family="binomial")
summary(step.model)
prob.step <- step.model %>% predict(test_x,type ="response")
predicted.class2 <- ifelse(prob.step>0.5,'Yes','No') 
mean(predicted.class2==test_y)
table(test$target, prob.step>0.5)
ROCRpred.step = prediction(prob.step, test$target)
auc.step = as.numeric(performance(ROCRpred.step, 'auc')@y.values)
auc.step
ROCRperf.step = performance(ROCRpred.step, 'tpr','fpr')
plot(ROCRperf.step, colorize=TRUE, main='ROC Curve',print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
vif(step.model)
varImp(step.model)
prob.train.step <- step.model %>% predict(train_x,type ="response")
predicted.train.step <- ifelse(prob.train.step>0.5,'Yes','No')
prob.test.step <- step.model %>% predict(test_x,type ="response")
predicted.test.step <- ifelse(prob.test.step>0.5,'Yes','No')


exp(coef(step.model))
# Train
confusionMatrix(data = as.factor(predicted.train.step),
                reference = train_y, positive = "Yes")

# Test
confusionMatrix(data = as.factor(predicted.test.step),
                reference = test_y, positive = "Yes")

#compare model
anova(mod.log,step.model, test ="Chisq")

#check linearity assumption

raw_test<-rbind(train_x,test_x)

raw_num <- raw_test %>% select_if(is.numeric)
predictors <- colnames(raw_num)
prob.raw.step <- step.model %>% predict(raw_test,type ="response")
raw_num <- raw_num%>% mutate(logit = log(prob.raw.step/(1-prob.raw.step))) %>%
  gather(key = "predictors",value = "predicted.value",-logit)

#plotting the graph for cheking the linearity
ggplot(raw_num,aes(x =logit,y =predicted.value)) + geom_point() + geom_smooth(method ="loess") + theme_classic() + theme_bw()+facet_wrap(~predictors,scale ="free_y")

#checking for the influential point
plot(step.model,which=4,id.n =5)

#checking for the studentized residuals error
model.data <- augment(step.model) %>% mutate(index =1:n())
top5<-model.data %>% top_n(5,.cooksd)
top5$.std.resid
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = target), alpha = .5) +
  theme_bw()


         