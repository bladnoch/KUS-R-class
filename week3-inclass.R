setRepositories(ind = 1:7)
getwd()
WORK_DIR <- Users/doungukkim/Desktop/workspace/KUS-R-class

setwd(WORK_DIR)


library(Rstat)
library(ggVennDiagram)
library(animation)
library(dplyr)

nsides <-6
times <-2

temp <- list()

for (i in 1:times){
  temp[[i]] <- 1:nsides
}

s <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)

