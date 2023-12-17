##################################
## final exam.                  ##
## for Probability & Statistics ##
## Dounguk Kim                  ##
## 2023-12-18.                  ##
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

data <- read.table("[data_here].txt", sep="\t")
data <- data.frame(fread("[data_here].txt", sep="\t", head=T, stringsAsFactors = T))

data2 <- read.table("[data_here].txt", sep="\t")
data2 <- data.frame(fread("[data_here].txt", sep="\t", head=T, stringsAsFactors = T))

data2 <- read.table("[data_here].txt", sep="\t")
data2 <- data.frame(fread("[data_here].txt", sep="\t", head=T, stringsAsFactors = T))

#--------------------------------------------------------------------------------------------------test

## t.test | parametric | two group | continuous
t.test(sel2020$HW2,sel2022$HW2,alternative = "greater") 

## wilcoxon | non-parametric | two group | continuous
wilcox.test(Age ~ Gender,data3)$p.value

## anova | parametric | multi group | continuous
aov(HW1 ~ group, data = combined_data)

## kruskal | non-parametric | multi group | continuous
kruskal.test(len ~ dose, data = ToothGrowth)$p.value

## chi-squared | parametric | two group | categorical
chisq.test(data3$Severity_Group,data3$Gender)$p.value # chisq.test(독립, 범주)

## fisher | non-parametric | two group | categorical
fisher.test(data$treatment, data$improvement)$p.value

## Pearson's | parametric 
cor.test(data2020$HW3, data2020$Final) 

## Permutation t-test | non-parametric | two group | continuous
obs_t_stat <- as.numeric(t.test(Height ~ Gender, Data)$statistic)

null_t_stat <- c()
numOfRepeat <- 1000

for(i in 1:numOfRepeat){
  null_t_stat[i] <- as.numeric(t.test(Height ~ sample(Gender), Data)$statistic)
}

Pval <- (sum(abs(null_t_stat) >= abs(obs_t_stat)) + 1) / numOfRepeat
Pval

#----------------------------------------------------------------------------------------------------Q1
#----------------------------------------------------------------------------------------------------Q2
#----------------------------------------------------------------------------------------------------Q3
#----------------------------------------------------------------------------------------------------Q4
#----------------------------------------------------------------------------------------------------Q5
#----------------------------------------------------------------------------------------------------Q6
#----------------------------------------------------------------------------------------------------Q7
#----------------------------------------------------------------------------------------------------Q8
#----------------------------------------------------------------------------------------------------Q9
#----------------------------------------------------------------------------------------------------Q10
