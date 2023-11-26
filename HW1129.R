
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

data <- read.table("Data.txt", sep="\t")  # your_file.txt 부분을 불러올 파일의 이름으로 변경해주세요.
data <- data.frame(fread("Data.txt", sep="\t", head=T, stringsAsFactors = T))
dim(data)
summary(data)
head(data)
