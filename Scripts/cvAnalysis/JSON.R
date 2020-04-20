# JSON.r
# Create .json files for deployment


rsconnect::writeManifest(appDir = "/Users/lee/Documents/GitHub/ProbabilisticScoring/Scripts/cvAnalysis",
                         appFiles = c("functionsProbScoreCalc.R",
                                      "functionsScoringAnalysis.R",
                                      "functionsWeightCalculations.R",
                                      "MasterCVanalysis.R",
                                      "MasterCVanalysis2.R",
                                      "test.Rmd",
                                      "phq9Subset.Rdata",
                                      "functionsDataImportProcessing.R")
                         )
