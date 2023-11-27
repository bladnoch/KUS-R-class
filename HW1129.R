
setRepositories(ind = 1:7)

getwd()

WORK_DIR <- "/Users/doungukkim/Desktop/workspace/KUS-R-class/HW_until_1129"
WORK_DIR
setwd(WORK_DIR)


library(Rstat)
library(ggVennDiagram)
library(animation)
library(dplyr)

library(data.table)


#data--------------------------------------------------------------------------------------------------

data <- read.table("Data.txt", sep="\t")  # your_file.txt 부분을 불러올 파일의 이름으로 변경해주세요.
data <- data.frame(fread("Data.txt", sep="\t", head=T, stringsAsFactors = T))
dim(data)
summary(data)
head(data)

#function----------------------------------------------------------------------------------------------

#(Z-test)
# Implemenation to calculate Z-statistic
z.test <- function(input, mu, var){
  z <- (mean(input) - mu) / (sqrt(var / length(input)))
  return(z)
}

# Let's generate Men's Height
Height_2020yr <- rnorm(100, mean = 173.6, sd = 5.62)

# Get Z-statistic
Zstat <- z.test(Height_2020yr, mu=169.3, var=5.4^2)

2*pnorm(-abs(Zstat)) # two-sided Z-test
1-pnorm(Zstat) # one-sided Z-test (2020 Height >= 169.3)
pnorm(Zstat) # one-sided Z-test (2020 Height <= 169.3)


#quiz--------------------------------------------------------------------------------------------------
summary(data)

# Q1
covid_data <- subset(data,Disease == "COVID19") #subset for covid19 positive
covid_data
covid19_sysBP <- mcovid_data$sysBP #mean sysBP those with covid19
covid19_sysBP

Q2_Zstat <- z.test(covid19_sysBP, mu=128.65, var=16.5^2) 
Q2_Zstat

2*pnorm(-abs(Q2_Zstat))

# Q2
covid_data <- subset(data,Disease == "COVID19") #subset for covid19 positive
covid_data

covid19_sysBP <- (covid_data$sysBP) #mean sysBP those with covid19
covid19_sysBP

mean(covid19_sysBP)
length(covid19_sysBP)


1-pnorm(Q2_Zstat)

#Q3

gender_data <- subset(data,Gender=="1")
gender_data
male_HR <- gender_data$HR
mean(male_HR)
male_HR

Q3_Zstat <- z.test(male_HR,mu=74.62, var=11.1)
Q3_Zstat

2*pnorm(-abs(Q3_Zstat)) # two-sided Z-test
1-pnorm(Q3_Zstat) # one-sided Z-test (male_HR >= 74.62)
pnorm(Q3_Zstat) # one-sided Z-test (male_HR <= 74.62)

#Q5
rest_data <- data$Resting_SaO2
rest_data
mean(rest_data)

Q5_Zstat <- z.test(rest_data,mu=96.0245,var=8.995)
Q5_Zstat

1-pnorm(Q5_Zstat)
2*pnorm(-abs(Q5_Zstat))

