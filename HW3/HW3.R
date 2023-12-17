##################################
## Homework 3.                  ##
## for Probability & Statistics ##
## Dounguk Kim                  ##
## 2023-12-10.                  ##
##################################


setRepositories(ind = 1:7)

getwd()

WORK_DIR <- "/Users/doungukkim/Desktop/workspace/KUS-R-class/HW3"
WORK_DIR
setwd(WORK_DIR)

library(Rstat)
library(ggVennDiagram)
library(animation)
library(dplyr)
library(data.table)

library(ggplot2)

#--------------------------------------------------------------------------------------------------data

data2020 <- read.table("Data1_PS_2020.txt", sep="\t")
data2020 <- data.frame(fread("Data1_PS_2020.txt", sep="\t", head=T, stringsAsFactors = T))

data2021 <- read.table("Data1_PS_2021.txt", sep="\t")
data2021 <- data.frame(fread("Data1_PS_2021.txt", sep="\t", head=T, stringsAsFactors = T))

data2022 <- read.table("Data1_PS_2022.txt", sep="\t")
data2022 <- data.frame(fread("Data1_PS_2022.txt", sep="\t", head=T, stringsAsFactors = T))

data2 <- read.table("Data2.txt", sep="\t")
data2 <- data.frame(fread("Data2.txt", sep="\t", head=T, stringsAsFactors = T))
data2

data3 <- read.table("Data3.txt", sep="\t")
data3 <- data.frame(fread("Data3.txt", sep="\t", head=T, stringsAsFactors = T))
data3

#----------------------------------------------------------------------------------------------function

#--------------------------------------------------------------------------------------------------quiz

#----------------------------------------------------------------------------------------------------Q1

# HW1, HW2, HW3, midterm

sel2020 <- data2020 %>% select(HW1,HW2,HW3,Midterm)
sel2021 <- data2021 %>% select(HW1,HW2,HW3,Midterm)
sel2022 <- data2022 %>% select(HW1,HW2,HW3,Midterm)

sel2020$group <-2020
sel2021$group <-2021
sel2022$group <-2022

# 데이터 결합
combined_data <- rbind(sel2020,sel2021,sel2022)
combined_data

# ANOVA 테스트 수행
anova_result <- aov(HW1 ~ group, data = combined_data)
summary(anova_result) #5.7e-07
anova_result <- aov(HW2 ~ group, data = combined_data)
summary(anova_result) #1.84e-07 ***
anova_result <- aov(HW3 ~ group, data = combined_data)
summary(anova_result) #8.05e-05 ***
anova_result <- aov(Midterm ~ group, data = combined_data)
summary(anova_result) #0.000155

# t.test 수행
t.test(sel2020$HW2,sel2022$HW2,alternative = "greater") #3.852e-08
t.test(sel2020$HW2,sel2022$HW2,alternative = "less") #p 1
t.test(sel2020$HW2,sel2022$HW2) # 7.7703e-08

t.test(sel2021$HW2,sel2022$HW2,alternative = "greater") #p 0.0005605
t.test(sel2021$HW2,sel2022$HW2,alternative = "less") #p 0.9994
t.test(sel2021$HW2,sel2022$HW2) # p 0.001121


# t.test로 2020,2021의 그룹과 2022 그룹 테스트
# t.test 수행 (2020+2021), (2022)

# HW1, HW2, HW3, midterm
v2020 <- data2020 %>% select(HW1,HW2,HW3,Midterm)
v2021 <- data2021 %>% select(HW1,HW2,HW3,Midterm)
v2022 <- data2022 %>% select(HW1,HW2,HW3,Midterm)

v2020$group <- 202021
v2021$group <- 202021
v2022$group <- 2022

combined_test <- rbind(v2020,v2021,v2022)
combined_test
combined202021 <- subset(combined_test,group == 202021)
combined202021
combined2022 <- subset(combined_test,group == 2022)
combined2022

t.test(combined202021$HW1,combined2022$HW1, alternative = "greater") #p-value = 1.017e-07
t.test(combined202021$HW2,combined2022$HW2, alternative = "greater") #p-value = 2.754e-06
t.test(combined202021$HW3,combined2022$HW3, alternative = "greater") #p-value = 1.925e-05
t.test(combined202021$Midterm,combined2022$Midterm, alternative = "greater") #p-value = 0.008447

#----------------------------------------------------------------------------------------------------Q2

# 각 그룹의 데이터 분포를 시각화
boxplot(HW2 ~ group, data = combined_data)
boxplot(HW2 ~ group, data = combined_test)

#----------------------------------------------------------------------------------------------------Q3

# ANOVA 테스트 수행
anova_result <- aov(HW1 ~ group, data = combined_data)
summary(anova_result) # 5.7e-07
anova_result <- aov(HW2 ~ group, data = combined_data)
summary(anova_result) # 1.84e-07
anova_result <- aov(HW3 ~ group, data = combined_data)
summary(anova_result) # 8.05e-05
anova_result <- aov(Midterm ~ group, data = combined_data)
summary(anova_result) # 0.000155

#----------------------------------------------------------------------------------------------------Q4

boxplot(HW1 ~ group, data = combined_data)
boxplot(HW2 ~ group, data = combined_data)
boxplot(HW3 ~ group, data = combined_data)
boxplot(Midterm ~ group, data = combined_data)

#----------------------------------------------------------------------------------------------------Q5

cor.test(data2020$HW3, data2020$Final) #p-value = 2.021e-06
cor.test(data2020$Midterm, data2020$Final) #p-value = 0.0001334

cor.test(data2021$HW3, data2021$Final) #p-value = 1.917e-06
cor.test(data2021$Midterm, data2021$Final) #p-value = 0.05228


# 2020년 데이터를 사용한 산점도
ggplot(data2020, aes(x = HW3, y = Final)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between HW3 and Final in 2020", x = "HW3 Score", y = "Final Score")

# 2021년 데이터를 사용한 산점도
ggplot(data2021, aes(x = HW3, y = Final)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between HW3 and Final in 2021", x = "HW3 Score", y = "Final Score")

# 2021년 데이터를 사용한 산점도
ggplot(data2021, aes(x = Midterm, y = Final)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Midterm and Final in 2021", x = "Midterm Score", y = "Final Score")


#----------------------------------------------------------------------------------------------------Q6

# use data2 as a data
data2

FM <- subset(data2, Gender == "Female")
M <- subset(data2, Gender == "male")

FM
M

mean_male <- mean(M$numTardy)
mean_male # 4.888889

mean_female <- mean(FM$numTardy)
mean_female # 2.444444

sd_male <- sd(M$numTardy) 
sd_male # 2.1475

sd_female <- sd(FM$numTardy)
sd_female # 1.333333

kimStat <- abs(mean_male - mean_female) / ((sd_male + sd_female) / 8)
kimStat # 5.61831

## Permutation t-test

kimStat <- abs(mean_male - mean_female) / ((sd_male + sd_female) / 8)


null_kimStat <- c()
numOfRepeat <- 1000

for(i in 1:numOfRepeat){
  null_kimStat[i] <- as.numeric(t.test(numTardy ~ sample(Gender),data2)$statistic)
}

Pval <- (sum(abs(null_kimStat) >= abs(kimStat)) + 1) / numOfRepeat
Pval #0.001


#----------------------------------------------------------------------------------------------------Q7

wilcox.test(numTardy ~ Gender, data2, exact = FALSE)$p.value

#----------------------------------------------------------------------------------------------------Q8

t.test(FM$numTardy,M$numTardy)$p.value

#----------------------------------------------------------------------------------------------------Q10
t.test(Age ~ Gender,data3)$p.value
t.test(Height_CM ~ Gender,data3)$p.value
t.test(Weight_KG ~ Gender,data3)$p.value
t.test(sysBP ~ Gender,data3)$p.value
t.test(HR ~ Gender,data3)$p.value
t.test(Resting_SaO2 ~ Gender,data3)$p.value
t.test(BMI ~ Gender,data3)$p.value
t.test(FEV1pp_utah ~ Gender,data3)$p.value
t.test(FVCpp_utah ~ Gender,data3)$p.value
t.test(FEV1_FVC_utah ~ Gender,data3)$p.value
#----------------------------------------------------------------------------------------------------Q11

wilcox.test(Age ~ Gender,data3)$p.value
wilcox.test(Height_CM ~ Gender,data3)$p.value
wilcox.test(Weight_KG ~ Gender,data3)$p.value
wilcox.test(sysBP ~ Gender,data3)$p.value
wilcox.test(HR ~ Gender,data3)$p.value
wilcox.test(Resting_SaO2 ~ Gender,data3)$p.value
wilcox.test(BMI ~ Gender,data3)$p.value
wilcox.test(FEV1pp_utah ~ Gender,data3)$p.value
wilcox.test(FVCpp_utah ~ Gender,data3)$p.value
wilcox.test(FEV1_FVC_utah ~ Gender,data3)$p.value

#----------------------------------------------------------------------------------------------------Q12

data3
summary(aov(Age ~ Severity_Group, data = data3))[[1]][1,5]
summary(aov(Height_CM ~Severity_Group, data = data3))[[1]][1,5]
summary(aov(Weight_KG ~Severity_Group, data = data3))[[1]][1,5]
summary(aov(sysBP ~Severity_Group, data = data3))[[1]][1,5]
summary(aov(HR ~Severity_Group, data = data3))[[1]][1,5]
summary(aov( Resting_SaO2~Severity_Group, data = data3))[[1]][1,5]
summary(aov(BMI ~Severity_Group, data = data3))[[1]][1,5]
summary(aov(FEV1pp_utah ~Severity_Group, data = data3))[[1]][1,5]
summary(aov(FVCpp_utah ~Severity_Group, data = data3))[[1]][1,5]
summary(aov(FEV1_FVC_utah ~Severity_Group, data = data3))[[1]][1,5]

chisq.test(data3$Severity_Group,data3$Gender)$p.value # chisq.test(독립, 범주)
chisq.test(data3$Severity_Group,data3$Race)$p.value
chisq.test( data3$Severity_Group,data3$HaveCough)$p.value


