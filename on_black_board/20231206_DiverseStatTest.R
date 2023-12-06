##################################
## Lecture Material             ##
## for Probability & Statistics ##
## Minseok Seo                  ##
## 2023-12-06                   ##
##################################


## ANOVA and kruskal test for Multiple group comparisons
data("ToothGrowth")
View(ToothGrowth)
str(ToothGrowth)

ToothGrowth$dose <- factor(ToothGrowth$dose) #0.5, 1 , 2 3가지 경우만 있기 때문에 factor로 변환한다.ㅏ
 #이렇게 3가지 경우를 비교하고 싶기 때문에 2가지를 비교하는 t test는 불가능
#이론을 완벽하게 이용하기 보단 p value를 알 수 있으면 장떙

hist(ToothGrowth$dose)
summary(ToothGrowth$supp)

summary(aov(len~dose, data = ToothGrowth))
#[[1]][1,5]는 전체 대이터 서머리중 일부만 보기위해 사용 
#Anova
summary(aov(len~dose, data = ToothGrowth))[[1]][1,5]

#KS test
kruskal.test(len ~ dose, data = ToothGrowth)$p.value

boxplot(len~dose, data = ToothGrowth)$pvalue
## ANOVA and kruskal test for Multiple group comparisons


#sample 문제 
#3 groups of random sample data
GroupA <- rnorm(20,100,sd=4)
GroupB <- rnorm(20,100,sd=4)
GroupC <- rnorm(20,100,sd=4)


combinValue <- c(GroupA,GroupB,GroupC)
testData <- data.frame(combinValue,
                      Group = factor(rep(c("phase1","phase2","phase3"),each=20)))

boxplot(combinValue ~ Group, data = testData)
summary(aov(combinValue~Group, data = testData))[[1]][1,5]
#sample 문제 끝



## Association test for two continuous Random variables
data(mtcars)

A <- mtcars$mpg
B <- mtcars$wt


cor.test(mtcars$mpg, mtcars$wt, method = "pearson")$p.value
cor.test(mtcars$mpg, mtcars$wt, method = "spearman")$p.value




summary(lm(mpg~wt, data = mtcars))$coef[2,4]



## Association test for two cateogorical Random variables
data <- read.csv("https://goo.gl/j6lRXD")  #Reading CSV
table(data$treatment, data$improvement)

chisq.test(data$treatment, data$improvement)$p.value
fisher.test(data$treatment, data$improvement)$p.value


## Examples

Disease <- factor(c(rep("Covid-19", 50), rep("Normal", 50)))
Vaccine <- factor(c(rep("Shot", 50), rep("None", 50)))
Gender <- factor(rep(c(rep("Female", 25), rep("Male", 25)), 2))

table(Disease, Vaccine)
chisq.test(Disease, Vaccine)$p.value
fisher.test(Disease, Vaccine)$p.value

table(Disease, Gender)
chisq.test(Disease, Gender)$p.value
fisher.test(Disease, Gender)$p.value

