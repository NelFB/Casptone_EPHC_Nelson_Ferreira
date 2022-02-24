# Data Preparation
# Loading packages
if(!require(caret)) install.packages("caret", repos="http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos="http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos="http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos="http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos="http://cran.us.r-project.org")
if(!require(ggpmisc)) install.packages("ggpmisc", repos="http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos="http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("MLmetrics", repos="http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos="http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos="http://cran.us.r-project.org")
if(!require(surveyr)) install.packages("survey", repos="http://cran.us.r-project.org")
if(!require(srvyr)) install.packages("srvyr", repos="http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos="http://cran.us.r-project.org")

##Load database
set.seed(1, sample.kind = "Rounding")
#2019 
ephc2019 <- read_sav("reg02_ephc2019.sav")

#2020
ephc2020 <- read_sav("reg02_ephc2020.sav")
glimpse(ephc2019)

## Wrangling
# The data from 2019 will be our train set
eph_2019 <- ephc2019 %>% 
  select(UPM, NVIVI, NHOGA, DPTOREP, AREA, P02, P03, P04A, P06, P08A, 
         E01A, ED01, ED02, ED0504, añoest, ED09, S01A, S01B, FEX, NMAD, NPAD, pobnopoi, TIC0510, quintili, decili)

# The data from 2020 will be our test set
eph_2020 <- ephc2020 %>% select(UPM, NVIVI, NHOGA, DPTOREP, AREA, P02, P03, P04A, P06, P08A, 
                                E01A, ED01, ED02, ED0504,añoest, ED09, S01A, S01B, FEX, NMAD, NPAD, pobnopoi, TIC0510, quintili, decili)


# Remove the NA values
eph_2019 <- eph_2019 %>%
  filter(!is.na(AREA)) %>%
  filter(!is.na(añoest)) %>%
  filter(!is.na(P02)) %>%
  filter(!is.na(pobnopoi)) %>%
  filter(!is.na(ED01))

eph_2020 <- eph_2020 %>%
  filter(!is.na(AREA)) %>%
  filter(!is.na(añoest)) %>%
  filter(!is.na(P02)) %>%
  filter(!is.na(pobnopoi)) %>%
  filter(!is.na(ED01))

rm(ephc2019, ephc2020)

## Create 'glyst_hs' variable
eph_2019<- eph_2019 %>% 
  filter(!is.na(añoest)) %>% 
  filter(!añoest %in% c("99")) %>% 
  mutate(glyst_hs = ifelse(añoest %in%
                             c("1","2","3","4","5","6", "7", "8", "9", "10", "11"), "<12",
                           ifelse(añoest %in%
                                    c("12", "13", "14", "15", "16", "17", "18"), ">=12", "<12")))
eph_2019 <- eph_2019 %>% mutate(glyst_hs = as.factor(glyst_hs))

eph_2020<- eph_2020 %>% 
  filter(!is.na(añoest)) %>% 
  filter(!añoest %in% c("99")) %>% 
  mutate(glyst_hs = ifelse(añoest %in%
                             c("1","2","3","4","5","6", "7", "8", "9", "10", "11"), "<12",
                           ifelse(añoest %in%
                                    c("12", "13", "14", "15", "16", "17", "18"), ">=12", "<12")))
eph_2020 <- eph_2020 %>% mutate(glyst_hs = as.factor(glyst_hs))

#Creating the 'graduate' variable
eph_2019 <- eph_2019 %>% 
  filter(!is.na(añoest)) %>% 
  mutate(graduate = ifelse(añoest %in% c("12"), "HSgrad",
                           ifelse(añoest %in% c("1","2","3","4","5","6"), "EEB_1_2",
                                  ifelse(añoest %in% c("7", "8", "9"), "EEB3",
                                         ifelse(añoest %in% c("10", "11"), "EM",
                                                ifelse(añoest %in% 
                                                         c("13", "14", "15","16", "17", "18"), "H_ED", 0))))))

eph_2020 <- eph_2020 %>% 
  filter(!is.na(añoest)) %>%
  mutate(graduate = ifelse(añoest %in% 
                             c("12"), "HSgrad",ifelse(añoest %in% 
                                                        c("1","2","3","4","5","6"), "EEB_1_2", 
                                                      ifelse(añoest %in% c("7", "8", "9"), "EEB3",
                                                             ifelse(añoest %in% c("10", "11"), "EM", 
                                                                    ifelse(añoest %in% c("13", "14", "15","16", "17", "18"), "H_ED", 0))))))

# Data exploration
## Characteristics of high-school graduates.
#Graduation per area 
eph_2019 %>% group_by(graduate) %>% ggplot(aes(as_factor(AREA)))+ geom_bar(aes(fill=graduate)) + labs(x = "Area", y ="Amount of graduates") + scale_fill_brewer(palette = "RdYlBu")+ theme_bw()
## Characteristics of  high-school graduates.
#High School graduation per area 
eph_2019 %>% group_by(graduate) %>% filter(añoest %in% c("12")) %>% ggplot(aes(as_factor(AREA)))+ geom_bar(aes(fill=graduate), show.legend = FALSE) + labs(x = "Area", y ="Amount of graduates") + scale_fill_brewer(palette = "RdYlBu")+ theme_bw()
#Poverty Level
poverty <- eph_2019 %>% group_by(as_factor(pobnopoi)) %>% filter(añoest ==  12) %>% count(pobnopoi) 
pvrty <-eph_2019 %>% group_by(as_factor(pobnopoi)) %>% filter(añoest %in% c("12")) %>% ggplot(aes(as_factor(pobnopoi))) + geom_bar(aes(fill= "#FC8C58"),show.legend = FALSE) + labs(x = "Poverty Level", y ="Amount of high school graduates") + scale_fill_brewer(palette = "RdYlBu") + theme_bw()
pvrty + annotate(geom = "table", x = 3, y = 1500, label = list(poverty))

#Area and language spoken
eph_2019 %>% filter(ED01 %in% c("1","2","3")) %>% ggplot(aes(x=as_factor(AREA)))+ geom_bar(aes(fill=as_factor(ED01))) + xlab("Language") + labs(fill="Language")  + stat_count(geom = "text", colour = "black", size = 3.5, aes(label = ..count..),position=position_stack(vjust=1.1))+ scale_fill_brewer(palette = "RdYlBu") + theme_bw()
#Last degree obtained and language spoken
eph_2019 %>% filter(ED01%in%c("1","2","3")) %>% ggplot(aes(graduate)) + geom_bar(aes(fill=as_factor(ED01)))+ labs(fill="Language") + xlab("Language") + theme_bw() + coord_flip()+ scale_fill_brewer(palette = "RdYlBu") + theme_bw()

#Years of study by language spoken
eph_2019 %>% filter(ED01 %in% c("1","2","3")) %>%
  filter(añoest%in%c("0","1","2","3","4","5","6","7","8","9","10", "11","12")) %>%
  group_by(ED01)%>%
  ggplot(aes(as_factor(ED01), as.numeric(añoest)))+
  geom_boxplot()+
  ylab("Studied years")+
  xlab("Language")+
  theme_economist()

# Modeling
## Preparation
set.seed(1, sample.kind = "Rounding")
eph_2019 <- eph_2019 %>% select(AREA, añoest, P02, pobnopoi, ED01, glyst_hs)
eph_2019 <- eph_2019 %>% mutate_at(
  vars("AREA", "añoest", "pobnopoi", "ED01", "glyst_hs"),
  funs(as_factor(.))
)
for (i in 1:length(eph_2019$añoest))
{
  eph_2019$añoest[i]<-ifelse(eph_2019$añoest[i]=="Sin instrucción", 0, eph_2019$añoest[i])
}
eph_2019$añoest<-as.numeric(eph_2019$añoest)
eph_2019$P02<-as.numeric(eph_2019$P02)
eph_2019<- eph_2019 %>%  # Create glyst_hs variable
  filter(!is.na(añoest))

eph_2019 <- eph_2019 %>% select(AREA, añoest, P02, pobnopoi, ED01, glyst_hs)
test_index <- createDataPartition(eph_2019$glyst_hs, times=1, p=0.2, list = F)
train_set <- eph_2019[-test_index,]
test_set <- eph_2019[test_index,]
model_weights <- ifelse(train_set$glyst_hs == "<12",
                        (1/table(train_set$glyst_hs)[1]) * 0.5,
                        (1/table(train_set$glyst_hs)[2]) * 0.5)
sum(model_weights)#The sum must equal 1
rm(test_index)

## SVM model (Support Vector Machine)
# SVM Model
svm.eph = svm(glyst_hs ~AREA+P02+pobnopoi+ED01, data = train_set)
test_set$pred.value = predict(svm.eph, newdata = test_set,type="response")
confusionMatrix(test_set$glyst_hs, test_set$pred.value)

results <- data.frame(
  Model="SVM (Support Vector Machine)",
  Accuracy=
    Accuracy(test_set$glyst_hs, test_set$pred.value),
  F1Score=
    F1_Score(test_set$glyst_hs, test_set$pred.value),
  Specificity=
    specificity(test_set$glyst_hs, test_set$pred.value),
  Sensitivity=
    sensitivity(test_set$glyst_hs, test_set$pred.value))
results

# Applying Decision Tree Model
detree <- rpart(glyst_hs ~
                  AREA + P02 + pobnopoi + ED01,
                data = train_set)
# Prediction of data and Confusion Matrix
test_set$pred.value2 = predict(detree, newdata = test_set, type="class")
confusionMatrix(test_set$glyst_hs, test_set$pred.value2)

results<- bind_rows(results,
                    data.frame(Model="Decision Tree",
                               Accuracy=
                                 Accuracy(test_set$glyst_hs, test_set$pred.value2),
                               F1Score=
                                 F1_Score(test_set$glyst_hs, test_set$pred.value2),
                               Specificity=
                                 specificity(test_set$glyst_hs, test_set$pred.value),
                               Sensitivity=
                                 sensitivity(test_set$glyst_hs, test_set$pred.value)))
results

# Validation
## Preparation
set.seed(1, sample.kind = "Rounding")
eph_2020 <- eph_2020 %>% select(AREA, añoest, P02, pobnopoi, ED01, glyst_hs)
eph_2020 <- eph_2020 %>% mutate_at(
  vars("AREA", "añoest", "pobnopoi", "ED01", "glyst_hs"),
  funs(as_factor(.))
)
for (i in 1:length(eph_2020$añoest))
{
  eph_2020$añoest[i]<-ifelse(eph_2020$añoest[i]=="Sin instrucción", 0, eph_2020$añoest[i])
}
eph_2020$añoest<-as.numeric(eph_2020$añoest)
eph_2020$P02<-as.numeric(eph_2020$P02)
eph_2020<- eph_2020 %>%  # Create glyst_hs variable
  filter(!is.na(añoest))

eph_2020 <- eph_2020 %>% select(AREA, añoest, P02, pobnopoi, ED01, glyst_hs)
model_weights <- ifelse(eph_2020$glyst_hs == "<12",
                        (1/table(eph_2020$glyst_hs)[1]) * 0.5,
                        (1/table(eph_2020$glyst_hs)[2]) * 0.5)
sum(model_weights)#The sum must equal 1

## SVM Validation
svm.eph = svm(glyst_hs ~
                AREA +  P02 + pobnopoi + ED01,
              data = eph_2020)
eph_2020$pred.value = predict(svm.eph, newdata = eph_2020,type="response")
ConfusionMatrix(eph_2020$glyst_hs, eph_2020$pred.value)

results<- bind_rows(
  results,
  data.frame(Model="Validation - SVM",
             Accuracy=
               Accuracy(eph_2020$glyst_hs, eph_2020$pred.value),
             F1Score=
               F1_Score(eph_2020$glyst_hs, eph_2020$pred.value),
             Specificity=
               specificity(test_set$glyst_hs, test_set$pred.value),
             Sensitivity=
               sensitivity(test_set$glyst_hs, test_set$pred.value)))
results

# Results
results






