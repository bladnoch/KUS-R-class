print(1+1)
print(2/2)





setRepositories(ind = 1:7)

install.packages("devtools")
library(devtools)

install_github("jhk0530/Rstat")
install.packages("ggVennDiagram")
install.packages("animation")

library(ggVennDiagram)
library(animation)
library(dplyr)

## set two fair die (self-checking 1)

nsides <-6
times <-2

temp <-list()

for (i in 1:times){
  temp[[i]] <- 1:nsides
}

s<-expand.grid(temp, KEEP.OUT.ATTRS = F)


s
##sample space s