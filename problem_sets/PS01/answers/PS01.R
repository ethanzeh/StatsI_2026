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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

iq_sample <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# 1.1

# Using T-distrubution because population SD is not given (although IQ inherently has SD of 15)
# Population mean not given... will use 100 and go with T-dist because sample size < 30?

mean_iqs <- mean(iq_sample)
sd_iqs <- sd(iq_sample)

# 24 degrees of freedom T value for 2 tails (source: https://www.sjsu.edu/faculty/gerstman/StatPrimer/t-table.pdf)
t_value_iqs = 1.711
se_iqs =  sd_iqs / sqrt(length(iq_sample))

ci90_lower_bound_t_iqs <- mean_iqs - (t_value_iqs * se_iqs)
ci90_upper_bound_t_iqs <- mean_iqs + (t_value_iqs * se_iqs)

cat("90 percent confidence interval lower bound:", ci90_lower_bound_t_iqs, "\n")
cat("90 percent confidence interval upper bound:", ci90_upper_bound_t_iqs, "\n")

# --------------------------------------------------------------------------------
# 1.2

# Null Hypothesis is iq = 100
# Alternate Hypothesis is iq > 100
# Right tailed test

test_statistic_iqs = (mean_iqs - 100) / se_iqs

#.95 right tailed critical T value
t_value_95_one_tailed <- 1.711

# is the test_stat greater than the t_value
test_statistic_iqs > t_value_95_one_tailed

# Accept the null


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
expenditure$Region <- factor(expenditure$Region)

# 2.1
pairs(expenditure[, c("Y", "X1", "X2", "X3")])

# 2.2
ggplot(expenditure, aes(x = Region, y = Y)) +
  geom_boxplot(alpha = 0.4, width = 0.5) +
  geom_jitter(width = 0.1, size = 2.5) +
  labs(
    x = "Region",
    y = "Y",
    title = "Relationship Between Y and Region"
  ) +
  theme_minimal()


# 2.3
ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Y and X1",
    x = "X1",
    y = "Y"
  ) +
  theme_minimal()


ggplot(expenditure, aes(x = X1, y = Y, 
               color = Region, shape = Region)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = TRUE,
    color = "black"
  ) +
  labs(
    title = "Relationship Between Y and X1 by Region",
    x = "X1",
    y = "Y",
    color = "Region",
    shape = "Region"
  ) +
  theme_minimal()