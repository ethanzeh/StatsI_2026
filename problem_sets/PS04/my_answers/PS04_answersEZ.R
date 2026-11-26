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
    install.packages(new.pkg,  dependencies = TRUE, repos = "https://cloud.r-project.org")
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
lapply(c("car", "stargazer"),  pkgTest)

# set wd for current folder
setwd(getwd())

# read in data
library(car)
data(Prestige)

# 1a
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
table(Prestige$type, Prestige$professional, useNA = "ifany")

# 1b
model_q1 <- lm(prestige ~ income * professional, data = Prestige)
summary(model_q1)
stargazer(model_q1, type = "latex", header = FALSE, table.placement = "H",
        title = "prestige on income, professional and their interaction",
        dep.var.labels = "Prestige",
        covariate.labels = c("Income", "Professional", "Income $\\times$ Professional"),
        digits = 4, omit.stat = c("f", "ser"))

# 1c
coef(model_q1)

# 1f
b <- coef(model_q1)
me_income_prof <- (b["income"] + b["income:professional"]) * 1000
me_income_prof

# 1g
me_prof_6000 <- b["professional"] + b["income:professional"] * 6000
me_prof_6000

# 2 - df = n -k - 1
df_q2 <- 131 - 2 - 1

# 2a
t_signs <- 0.042 / 0.016
p_signs <- 2 * pt(abs(t_signs), df = df_q2, lower.tail = FALSE)
cat(sprintf("t = %.3f | p = %.4f | critical t = %.3f\n",
    t_signs, p_signs, qt(0.975, df = df_q2)))

# 2b
t_adj <- 0.042 / 0.013
p_adj <- 2 * pt(abs(t_adj), df = df_q2, lower.tail = FALSE)
cat(sprintf("t = %.3f | p = %.4f | critical t = %.3f\n",
    t_adj, p_adj, qt(0.975, df = df_q2)))
