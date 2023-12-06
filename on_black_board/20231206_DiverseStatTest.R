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

ToothGrowth$dose <- factor(ToothGrowth$dose)


hist(ToothGrowth$dose)
summary(ToothGrowth$supp)

summary(aov(len~dose, data = ToothGrowth))[[1]][1,5]
kruskal.test(len ~ dose, data = ToothGrowth)$p.value


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

