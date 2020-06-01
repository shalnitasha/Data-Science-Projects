library(tidyverse)
library(readr)
library(caret)
library(corrplot)
install.packages('ggcorrplot')
library(ggcorrplot)
library(ISLR)
library(glmnet)
library(ggplot2)
library(scales)
library(cluster)
library(factoextra)
library(openxlsx)
library(reshape2)
library(plyr)
library(scales)
library(ggplot2)
library(devtools)
library(grid)
options(scipen=999)


getwd()
list.files()
data = read.csv('heart_disease_data.csv')
col_names = c('Age','Sex','chest_pain','Resting_Bp','Cholestrol_Level','Fasting_Blood_Sugar',
              'Resting_ECG_Result','Max_heart_Rate','Excercise_induced_Angina',
              'Oldpeak_ST_depression_exercise_vs_rest','Slope_peak_of_ST_segment',
              '#Major_vessels_coloured_by_fluoroscopy','Thalassemia_status','Target')

colnames(data) = col_names
View(data)
str(data1)
# random row wise shuffling of the data
data = data[sample(nrow(data)),]

# data set with character variables
data1 = mutate(data, Sex  = ifelse( Sex == 1, 'Male', 'Female'))

data1 = mutate(data1, chest_pain = ifelse(chest_pain == 0,'Typical',ifelse(chest_pain == 1, 
        "Atypical", ifelse( chest_pain == 2, 'Non-Anginal', 'Asymptomatic' ))))

data1 = mutate(data1, Fasting_Blood_Sugar = ifelse(Fasting_Blood_Sugar == 1, '> 120 mg/dl', '< 120 mg/dl'))

data1 = mutate(data1, Resting_ECG_Result = ifelse(Resting_ECG_Result == 0, 'Normal',
          ifelse(Resting_ECG_Result == 1, 'ST-T wave abnormal','probable or definite LVH')))

data1 = mutate(data1, Excercise_induced_Angina = ifelse(Excercise_induced_Angina == 1, 'Yes', 'No'))

data1 = mutate(data1, Slope_peak_of_ST_segment  = ifelse(Slope_peak_of_ST_segment  == 0,'Up',
                                    ifelse(Slope_peak_of_ST_segment  == 1, 'Flat', 'Down')))

data1 = mutate(data1, Thalassemia_status  = ifelse( Thalassemia_status  == 3, 'Fixed',
                                      ifelse( Thalassemia_status  == 2, 'Irreversible','normal')))
data1 = mutate(data1 , Target = (ifelse( Target == 1, 'Yes', 'No')))
View(data1)

str(data1)

# feature engineering for data viz
data1 = mutate(data1, Age_brackets = ifelse((data1$Age >=18 & data1$Age <= 35), 'Young',
                                            ifelse((data1$Age >=36 & data1$Age <= 55), 'Middle Aged','older')))

data1 = mutate(data1,chol_range = ifelse(data1$Cholestrol_Level < 200, 'Desirable',
                                         ifelse((data1$Cholestrol_Level >= 200 & data1$Cholestrol_Level <= 239), 'borderline',
                                                'high')))
data1 = mutate(data1, Resting_Bp_range = ifelse((data1$Resting_Bp >= 90 & data1$Resting_Bp < 100),
                                                'low', ifelse((data1$Resting_Bp >= 100 & data1$Resting_Bp < 120), 'normal',
                                                              ifelse((data1$Resting_Bp >= 120 & data1$Resting_Bp < 139) ,'prehypertension',
                                                                     ifelse((data1$Resting_Bp >= 139 & data1$Resting_Bp < 159),'stage1_hy','stage2_hy')))))

# visualizations

install.packages("esquisse")  

esquisse::esquisser()
esquisse::esquisser(iris, viewer = 'browser')


library(ggplot2)


#target distribution
ggplot(data1) +
 aes(x = Target, fill = Target) +
 geom_bar() +
 scale_fill_hue() +
 labs(x = "Target", y = "Count", title = "Target Dstribution") +
 theme_minimal()

# AGE distribution
ggplot(data1) +
  aes(x = Age, fill = Target) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  labs(x = "Age", y = "Count", title = "Target distribution with respect to Age", fill = "Target") +
  theme_minimal()

ggplot(data1) +
  aes(x = Age_brackets, fill = Target) +
  geom_bar(position = "fill") +
  scale_fill_hue() +
  coord_flip() +
  theme_minimal()

# chest pain vs target
ggplot(data1) +
  aes(x = chest_pain, fill = Target) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  labs(x = "Type of Chest Pain", y = "Count", title = "Target Distribution wrt Chest Pain", fill = "Target") +
  theme_bw()

# gender vs target
ggplot(data1) +
  aes(x = Age, y = Max_heart_Rate, colour = Target) +
  geom_point(size = 2.78) +
  geom_smooth(span = 0.83) +
  scale_color_hue() +
  theme_minimal()

ggplot(data1) +
  aes(x = Oldpeak_ST_depression_exercise_vs_rest, fill = Target) +
  geom_bar(position = "fill") +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Age_brackets))


esquisse::esquisser()
esquisse::esquisser(iris, viewer = 'browser')

ggplot(data1) +
  aes(x = Target, y = Oldpeak_ST_depression_exercise_vs_rest, fill = Age_brackets) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal()


ggplot(data1) +
  aes(x = Slope_peak_of_ST_segment, fill = Target) +
  geom_bar(position = "fill") +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Age_brackets))

ggplot(data1) +
  aes(x = chest_pain, fill = Target) +
  geom_bar(position = "fill") +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Age_brackets))
ggplot(data1) +
  aes(x = chest_pain, fill = Target) +
  geom_bar() +
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(Sex))

data1 <- data1 %>%
  filter(!(chest_pain %in% "Asymptomatic"))

ggplot(data1) +
  aes(x = Age_brackets, fill = Target) +
  geom_bar(position = "fill") +
  scale_fill_hue() +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(chest_pain))


ggplot(data1) +
  aes(x = chest_pain, fill = Target) +
  geom_bar(position = "fill") +
  scale_fill_hue() +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(Sex))

# PCA analysis-------------------------------------------------------------------------------------------------------
data2 = data[, c(1,4,5,8, 10)]
pca_output = prcomp(data2, center = TRUE,scale. = TRUE)
pca_output
summary(pca_output)
ggbiplot(pca_output)
fviz_screeplot(pca_output)

# finding percentage contribution of each component for varaince capturing
pca_output_var = pca_output$sdev^2
var_per = pca_output_var/sum(pca_output_var)
var_per
plot(var_per)

pc1_contri = fviz_contrib(pca_output, choice="var", axes = 1 )
pc1_contri
pc2_contri = fviz_contrib(pca_output, choice="var", axes = 2 )
pc2_contri
pc3_contri = fviz_contrib(pca_output, choice="var", axes = 3 )
pc3_contri
pc4_contri = fviz_contrib(pca_output, choice="var", axes = 4 )
pc4_contri
pc5_contri = fviz_contrib(pca_output, choice="var", axes = 5 )
pc5_contri


library(devtools)
install_github("vqv/ggbiplot")
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")
library(ggbiplot)

data3 = data[, -c(14)]
pca_output1 = prcomp(data3, center = TRUE,scale. = TRUE)
pca_output1
summary(pca_output1)
ggbiplot(pca_output1)
ggbiplot(pca_output1, labels=rownames(data3$Age))
fviz_screeplot(pca_output1)

data_target = c(data1$Target)
data_age = c(data1$Age_brackets)
data_gender = c(data1$Sex)
as.vector(data_target)
as.vector(data_age)
as.vector(data_gender)
ggbiplot(pca_output1, ellipse=TRUE,labels = row.names(data3$Age), groups= data_target)
ggbiplot(pca_output1, ellipse=TRUE,labels = row.names(data3$Age), groups= data_age)
ggbiplot(pca_output1, ellipse=TRUE,labels = row.names(data3$Age), groups= data_gender)



# finding percentage contribution of each component for varaince capturing
pca_output_var1 = pca_output1$sdev^2
var_per1 = pca_output_var1/sum(pca_output_var1)
var_per1
plot(var_per1)

pc1_contri = fviz_contrib(pca_output1, choice="var", axes = 1 )
pc1_contri
pc2_contri = fviz_contrib(pca_output1, choice="var", axes = 2 )
pc2_contri
pc3_contri = fviz_contrib(pca_output1, choice="var", axes = 3 )
pc3_contri
pc4_contri = fviz_contrib(pca_output1, choice="var", axes = 4 )
pc4_contri
pc5_contri = fviz_contrib(pca_output1, choice="var", axes = 5 )
pc5_contri






























