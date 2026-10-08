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
# Population mean not given... will use 100 and go with T-dist because sample size < 30

mean_iqs <- mean(iq_sample)
sd_iqs <- sd(iq_sample)

# 24 degrees of freedom T value for 2 tails (source: https://www.sjsu.edu/faculty/gerstman/StatPrimer/t-table.pdf)
t_value_iqs <- 1.711
se_iqs <-  sd_iqs / sqrt(length(iq_sample))

ci90_lower_bound_t_iqs <- mean_iqs - (t_value_iqs * se_iqs)
ci90_upper_bound_t_iqs <- mean_iqs + (t_value_iqs * se_iqs)

cat("90 percent confidence interval lower bound:", round(ci90_lower_bound_t_iqs, digits = 2), "\n")
cat("90 percent confidence interval upper bound:", round(ci90_upper_bound_t_iqs, digits = 2), "\n")

# --------------------------------------------------------------------------------
# 1.2

# Null Hypothesis is iq = 100
# Alternate Hypothesis is iq > 100
# Right tailed test

test_statistic_iqs <- (mean_iqs - 100) / se_iqs

#.95 right tailed critical T value
t_value_95_one_tailed <- 1.711

# is the test_stat greater than the t_value
test_statistic_iqs > t_value_95_one_tailed

# fail to reject the null
iqs_p_value <- pt(q = test_statistic_iqs, df = length(iq_sample - 1), lower.tail = FALSE)
cat("Test Statistic:", round(test_statistic_iqs, digits = 3), "\n")
cat("P-value:", round(iqs_p_value, digits = 2), "\n")

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)
expenditure$Region <- factor(expenditure$Region, levels = c(1, 2, 3, 4), labels = c("Northeast", "North Central", "South", "West"))

# 2.1
install.packages("GGally")
install.packages("ggplot2")
library(GGally)
library(ggplot2)

pdf("problem_sets/PS01/my_answers/graphs/graph2-1.pdf", width = 12, height = 12)

ggpairs(
  expenditure[, c("Y", "X1", "X2", "X3")],
  upper = list(
    continuous = wrap("cor", size = 5)
  ),
  lower = list(
    continuous = wrap(
      "smooth",
      method = "lm",
      se = FALSE,
      alpha = 0.5
    )
  ),
  diag = list(
    continuous = wrap("densityDiag")
  ),
  columnLabels = c(
    "Y (Per Capita Housing Assistance Expenditure)",
    "X1 (Per Capita Personal Income)",
    "X2 (Financially Insecure Residents per 100k)",
    "X3 (Urban Residents per 1k)"
  )
)
dev.off()

# 2.2

pdf("problem_sets/PS01/my_answers/graphs/graph2-2.pdf", width = 10, height = 10)

ggplot(expenditure, aes(x = Region, y = Y)) +
  geom_boxplot(alpha = 0.4, width = 0.5) +
  geom_jitter(width = 0.05, size = 2.5) +
  stat_summary(fun = mean, geom = "point", color = "orange", size = 3) +
  labs(
    x = "Region",
    y = "Per Capita Housing Assistance Expenditure",
    title = "Per Capita Housing Assistance Expenditure by Region"
  ) +
  theme_minimal()

dev.off()
means_region <- tapply(expenditure$Y, expenditure$Region, mean, na.rm = TRUE)
cat(
  "Means by region:", paste(names(means_region), round(means_region, 2), sep = " = "),
  sep = "\n"
)

# 2.3

pdf("problem_sets/PS01/my_answers/graphs/graph2-3-1.pdf", width = 10, height = 10)

ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Per Capita Housing Assistance Expenditure vs. Per Capita Personal Income",
    x = "Per Capita Personal Income",
    y = "Per Capita Housing Assistance Expenditure"
  ) +
  theme_minimal()
dev.off()

# Region

pdf("problem_sets/PS01/my_answers/graphs/graph2-3-2.pdf", width = 10, height = 10)

ggplot(expenditure, aes(x = X1, y = Y, color = Region, shape = Region)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = TRUE,
    color = "black"
  ) +
  labs(
    title = "Per Capita Housing Assistance Expenditure vs. Personal Income by Region",
    x = "Per Capita Personal Income",
    y = "Per Capita Housing assistance Expenditure",
    color = "Region",
    shape = "Region"
  ) +
  theme_minimal()
dev.off()