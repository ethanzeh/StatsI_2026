#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(getwd())

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

# keep only 3 variables
inc.sub <- inc.sub[complete.cases(inc.sub[, c("voteshare", "difflog", "presvote")]), ]

# 1a
model_q1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model_q1)

# 1b
png("q1_scatter.png", width = 700, height = 500)
plot(inc.sub$difflog, inc.sub$voteshare,
     xlab = "difflog incumbent - challenger spending, log",
     ylab = "voteshare (incumbent)",
     main = "incumbent vote share vs. spending difference",
     pch = 20, col = rgb(0, 0, 0, 0.3))
abline(model_q1, col = "red", lwd = 2)
dev.off()

# 1c
resid_q1 <- residuals(model_q1)

# 1d
coef(model_q1)

# 2a
model_q2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model_q2)

# 2b
png("q2_scatter.png", width = 700, height = 500)
plot(inc.sub$difflog, inc.sub$presvote,
     xlab = "difflog incumbent - challenger spending, log",
     ylab = "presvote (incumbent party's candidate)",
     main = "presidential vote share vs. spending difference",
     pch = 20, col = rgb(0, 0, 0, 0.3))
abline(model_q2, col = "red", lwd = 2)
dev.off()

# 2c
resid_q2 <- residuals(model_q2)

# 2d
coef(model_q2)

# 3a
model_q3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model_q3)

# 3b
png("q3_scatter.png", width = 700, height = 500)
plot(inc.sub$presvote, inc.sub$voteshare,
     xlab = "presvote (incumbent party's candidate)",
     ylab = "voteshare (incumbent)",
     main = "incumbent vote share vs. presidential vote share",
     pch = 20, col = rgb(0, 0, 0, 0.3))
abline(model_q3, col = "red", lwd = 2)
dev.off()

# 3c
coef(model_q3)

# 4a
model_q4 <- lm(resid_q1 ~ resid_q2)
summary(model_q4)

# 4b
png("q4_scatter.png", width = 700, height = 500)
plot(resid_q2, resid_q1,
     xlab = "residuals from Q2",
     ylab = "residuals from Q1",
     main = "add variable plot",
     pch = 20, col = rgb(0, 0, 0, 0.3))
abline(model_q4, col = "red", lwd = 2)
dev.off()

# 4.c
coef(model_q4)

# 5.a
model_q5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model_q5)

# 5.b
coef(model_q5)

# 5.c
# compare the pv coefficient  with the resid_q2 slope
cat(sprintf("Q4 slope: %.6f | Q5 presvote coef: %.6f\n",
    coef(model_q4)["resid_q2"], coef(model_q5)["presvote"]))
