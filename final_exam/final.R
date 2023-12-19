##################################
## final exam.                  ##
## for Probability & Statistics ##
## Dounguk Kim                  ##
## 2023-12-18.                  ##
##################################

setRepositories(ind = 1:7)

getwd()

WORK_DIR <- "/Users/doungukkim/Desktop/workspace/KUS-R-class/final_exam"
WORK_DIR
setwd(WORK_DIR)

library(Rstat)
library(ggVennDiagram)
library(animation)
library(dplyr)
library(data.table)
library(ggplot2)

#--------------------------------------------------------------------------------------------------data

data <- read.table("Q1_Data.txt", sep="\t")
data <- data.frame(fread("Q1_Data.txt", sep="\t", head=T, stringsAsFactors = T))
data

data2 <- read.table("Q6_Data.txt", sep="\t")
data2 <- data.frame(fread("Q6_Data.txt", sep="\t", head=T, stringsAsFactors = T))
data2


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

group_statistician <- subset(data, Job == "Statistician")

group_computer_scientist <- subset(data, Job == "ComputerScientist")
t.test(group_statistician$numCOVID,group_computer_scientist$numCOVID) # 0.02406

numCOVID_statistician <- group_statistician$numCOVID
numCOVID_computer_scientist <- group_computer_scientist$numCOVID

# Perform the Wilcoxon rank-sum test
wilcox_test_result <- wilcox.test(numCOVID_statistician, numCOVID_computer_scientist)

# Display the results
wilcox_test_result #0.06507


data


#----------------------------------------------------------------------------------------------------Q2
# 상자 그림 생성
ggplot(data, aes(x = Job, y = numCOVID, fill = Job)) +
  geom_boxplot() +
  labs(title = "Distribution of COVID-19 Cases by Job", x = "Job", y = "Number of Cases")

#----------------------------------------------------------------------------------------------------Q3
#----------------------------------------------------------------------------------------------------Q4

# Test statistic observed in real data (Step 1)
idxStat <- which(data$Job == "Statistician")
idxComp <- which(data$Job == "ComputerScientist")
idxComp

seoStatObs <- abs(median(data$numCOVID[idxStat]) - median(data$numCOVID[idxComp]))

# Finding the distribution of a test statistic empirically under the null hypothesis (Step 2)
nTimes <- 1000
# nullDist_seoStat <- c()
  
for(i in 1:nTimes){
  nullGroup <- sample(data$Job)
  idxStat <- which(nullGroup == "Statistician")
  idxComp <- which(nullGroup == "ComputerScientist")
  
  nullDist_seoStat[i] <- abs(median(data$numCOVID[idxStat]) - median(data$numCOVID[idxComp]))
}

hist(nullDist_seoStat)

# P-value calculation (Step 3)
pValSeoStat <- sum(nullDist_seoStat >= seoStatObs) / nTimes
pValSeoStat # 0.088


#----------------------------------------------------------------------------------------------------Q5
#----------------------------------------------------------------------------------------------------Q6
data2

group_healthy <- subset(data2, Disease == "Healthy")
group_healthy
group_cancer <- subset(data2, Disease == "Cancer")
group_cancer


# 암 그룹과 정상 그룹 분리
cancer_group <- subset(data2, Disease == 'Cancer')
normal_group <- subset(data2, Disease == 'Healthy')

# DNA 및 RNA 마커 추출 (DNA 또는 Gene으로 시작하는 열)
dna_rna_markers <- grep("^DNA|^Gene", names(data2), value = TRUE)

# 유의미한 마커를 저장할 리스트 초기화
significant_markers <- list()
# 각 마커에 대해 t-검정 수행
for (marker in dna_rna_markers) {
  cancer_values <- as.numeric(as.character(cancer_group[[marker]]))
  normal_values <- as.numeric(as.character(normal_group[[marker]]))
  
  if (sum(!is.na(cancer_values)) > 1 & sum(!is.na(normal_values)) > 1) {
    t_test_result <- t.test(cancer_values, normal_values, var.equal = FALSE)
    
    if (t_test_result$p.value < 0.01) { # 1% 유의수준
      significant_markers[[marker]] <- t_test_result$p.value
    }
  }
}

# 유의미한 마커의 개수 및 목록 출력
length(significant_markers)
significant_markers


#----------------------------------------------------------------------------------------------------Q7


# 데이터 전처리: 'Severity'를 factor로 변환
data2$Severity <- as.factor(data2$Severity)

# DNA 및 RNA 마커 선택
dna_rna_markers <- grep("^DNA|^Gene", names(data2), value = TRUE)

# 유의미한 마커를 저장할 리스트 초기화
significant_markers <- list()

# 각 마커에 대해 일원 분산 분석 수행
for (marker in dna_rna_markers) {
  # 마커 데이터가 수치형인지 확인
  if (is.numeric(data2[[marker]])) {
    anova_result <- aov(data2[[marker]] ~ data2$Severity, data = data2)
    
    # p-값 추출
    anova_summary <- summary(anova_result)
    p_value <- anova_summary[[1]]["Pr(>F)", marker]
    
    # 1% 유의수준에서 ANOVA 결과 확인
    if (!is.null(p_value) && p_value < 0.01) {
      significant_markers[[marker]] <- p_value
    }
  }
}

# 유의미한 마커의 개수 출력
length(significant_markers)





#----------------------------------------------------------------------------------------------------Q8

# DNA 및 RNA 마커 선택
dna_rna_markers <- grep("^DNA|^Gene", names(data2), value = TRUE)

# 유의미한 마커를 저장할 리스트 초기화
significant_markers <- list()

# 각 마커에 대해 키와의 관계를 검정
for (marker in dna_rna_markers) {
  marker_values <- as.numeric(as.character(data2[[marker]]))
  
  if (sum(!is.na(marker_values)) > 1) {
    t_test_result <- t.test(marker_values, data2$Height, var.equal = FALSE)
    
    if (t_test_result$p.value < 0.01) { # 1% 유의수준
      significant_markers[[marker]] <- t_test_result$p.value
    }
  }
}

# 유의미한 마커의 개수 출력
length(significant_markers)

#----------------------------------------------------------------------------------------------------Q9

# 유의미한 마커 중 일부 선택 (예: 첫 5개 마커)
selected_markers <- names(significant_markers)[1:5]

# 선택된 마커들에 대한 상자 그림 그리기
for (marker in selected_markers) {
  ggplot(data2, aes_string(x = "Disease", y = marker, fill = "Disease")) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", marker, "by Disease Group"),
         x = "Disease Group",
         y = marker) +
    theme_minimal() +
    print()
}



#----------------------------------------------------------------------------------------------------Q10
