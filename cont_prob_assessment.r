## Assessment: ACT probabilities

set.seed(16, sample.kind = "Rounding")

act_scores <- rnorm(n = 10000, mean = 20.9, sd = 5.7)

# Question 1A What is the mean of act_scores?

mean(act_scores)

# 1b What is the standard deviation of act_scores?

sd(act_scores)

# 1c How many perfect scores (>36) are there in act_scores?

perfect <- act_scores[act_scores >= 36]
length(perfect)

## Official solution:

sum(act_scores >= 36)

# 1d Probability of ACT score greater than 30.

mean(act_scores > 30)

# 1e Probability of ACT score less than or equal to 10

mean(act_scores <= 10)

# 2 Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the 
# value of the probability density function over x given a mean of 20.9 and 
# standard deviation of 5.7; save the result as f_x. Plot x against f_x.

a <- 1:36

f_1 <- dnorm(1, 20.9, 5.7)
f_2 <- dnorm(2, 20.9, 5.7)

f_a <- function(a) dnorm(a, 20.9, 5.7)


plot(a, f_a(a))

## Official solution: (seems to me that my code was shorter and more direct...)

x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line()

# 3  Convert act_scores to Z-scores

act_Z_scores <- function(x) {
 Z <- (x - mean(act_scores))/sd(act_scores)
 print(Z)
}

# (Z*sd)+ mean = x
# For Z = 2

x <- 2*5.7 + 20.9

1-pnorm(32.3, mean = 20.9, sd = 5.7)

## Official solution:

z_scores <- (act_scores - mean(act_scores))/sd(act_scores)
mean(z_scores > 2)

# 3b What ACT score corresponds to 2 sd's above the mean (Z = 2)?

2*sd(act_scores) + mean(act_scores)

# 3c 

qnorm(0.975, mean = 20.9, sd = 5.7)

# 4 Write a function that takes a value and produces the probability of an ACT
# score less than or equal to that value (the CDF). Apply this function to the
# range of 1 to 36. (isn't this the pnorm function?)

cdf <- function(x) {
  pnorm(x, mean(act_scores), sd(act_scores))
}

df <- cdf(1:36)

# 4a
qnorm(0.95, mean(act_scores), sd(act_scores))

# Official solution:
  
cdf2 <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))
  
# 4b 

qnorm(0.95, 20.9, 5.7)

# 4c

p <- seq(0.01, 0.99, 0.01)

sample_quantiles <- function(p) {
  quantile(x = act_scores, probs = p)}
sq_table <- data.frame(p, sample_quantiles)
View(sq_table)

# Official solution:

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])

# 4d 

theoretical_quantiles <- qnorm(p, 20.9, 5.7)
plot(theoretical_quantiles, sample_quantiles)

# Official solution:

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()

# end