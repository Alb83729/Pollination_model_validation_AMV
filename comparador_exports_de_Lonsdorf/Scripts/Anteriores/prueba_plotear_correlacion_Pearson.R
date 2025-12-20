#Import data into R or create some example
set.seed(150)
data <- data.frame(x = rnorm(50, mean = 50, sd = 10),
                   random = sample(c(-10:10), 50, replace = TRUE))
data$y <- data$x + data$random

#Calculate the Pearson’s correlation of x and y in data
correlation <- cor(data$x, data$y, method = 'pearson')

