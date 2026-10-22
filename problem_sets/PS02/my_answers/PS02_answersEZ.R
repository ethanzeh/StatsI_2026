
# 1.a
poly_sci_matrix <- matrix(c(14, 7, 6, 7, 7, 1), 2, 3)

grand_total_psm <- sum(poly_sci_matrix)
total_sum_psm <- 0

# i is col index
for (i in 1:ncol(poly_sci_matrix)){

    # j is row index
    for (j in 1:nrow(poly_sci_matrix)){
        row_total <- sum(poly_sci_matrix[j, ])
        column_total <- sum(poly_sci_matrix[, i])
        expected <- (row_total / grand_total_psm) * column_total
        
        squared_part_sum <- ((poly_sci_matrix[j, i] - expected) ^ 2) / expected
        total_sum_psm <- total_sum_psm + squared_part_sum
    }
}
poly_sci_test_stat <- total_sum_psm
print(poly_sci_test_stat)


# 1.b
df <- (length(poly_sci_matrix[1, ]) - 1) * (length(poly_sci_matrix[, 1]) - 1)
pchisq(total_sum_psm, df = df, lower.tail = FALSE)

# 1.c

# i is col index
for (i in 1:ncol(poly_sci_matrix)){

    # j is row index
    for (j in 1:nrow(poly_sci_matrix)){

        row_total <- sum(poly_sci_matrix[j, ])
        column_total <- sum(poly_sci_matrix[, i])
        expected <- (row_total / grand_total_psm) * column_total
        se <- sqrt(expected *
            (1 - (row_total / grand_total_psm)) * 
            (1 - (column_total / grand_total_psm)))
        z <- (poly_sci_matrix[j, i] - expected) / se

        cat(sprintf("Row: %d | Column: %d | Value: %.4f | Z (std res.): %.4f\n",
            j, i, poly_sci_matrix[j, i], z))
    }
}





#2.b
economics <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

# we use reserved, not women, because we want to see the effect of the policy
model <- lm(water ~ reserved, data = economics)

reserve_mean <- mean(economics$reserved)
water_mean <- mean(economics$water)
# sum of x deviations
numer <- sum((economics$reserved - reserve_mean) * (economics$water - water_mean))

# sum of squared deviations of x
denom <- sum((economics$reserved - reserve_mean)^2)
slope <- numer / denom
print(slope)

# compare and get p-value
summary(model)

# 9.252 is coef. estimate