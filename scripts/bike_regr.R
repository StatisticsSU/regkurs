# Load bike share data to replicate SAS-output on Slide nr 8 here: https://github.com/mattiasvillani/Regression/raw/master/Slides/Regression_L4.pdf
#install.packages("remotes") # uncomment if not installed
library(remotes)
install_github("mattiasvillani/progdataanalys")
library(progdataanalys)
lmfit = lm(nRides ~ temp + hum + windspeed, data = bike)
lmsummary(lmfit, anova = T, conf_intervals = T, vif_factors = T)
?lmsummary
?bike
