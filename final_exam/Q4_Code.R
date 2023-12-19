
# Test statistic observed in real data (Step 1)
idxStat <- which(data$Job == "Statistician")
idxComp <- which(data$Job == "ComputerScientist")

seoStatObs <- abs(median(data$numCOVID[idxStat]) - median(data$numCOVID[idxComp]))

# Finding the distribution of a test statistic empirically under the null hypothesis (Step 2)
nTimes <- 1000

for(i in 1:nTimes){
  nullGroup <- sample(data$Job)
  idxStat <- which(nullGroup == "Statistician")
  idxComp <- which(nullGroup == "ComputerScientist")
  
  nullDist_seoStat[i] <- abs(median(data$numCOVID[idxStat]) - median(data$numCOVID[idxComp]))
}

hist(nullDist_seoStat)

# P-value calculation (Step 3)
pValSeoStat <- sum(nullDist_seoStat >= seoStatObs) / nTimes
pValSeoStat

