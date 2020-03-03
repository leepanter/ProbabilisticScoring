# phq9DataSubsetImport.R

# Imports PHQ9 Data Subst from original data set

WD="/Users/lee/Documents/GitHub/ProbabilisticScoring/Data"
setwd(WD)

phq9=read.csv(file = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Data/PHQ9subset.csv")

save(phq9, file = "phq9.Rdata")
