# Continuous Probability

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

F <- function(a) mean(x <= a)

1 - F(70)    # probability of male taller than 70 inches

## Code: Using pnorm to calculate probabilities
 # Given male heights x:
  
  library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# We can estimate the probability that a male is taller than 70.5 inches using:
  
  1 - pnorm(70.5, mean(x), sd(x))

# Code: Discretization and the normal approximation
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

## Code: Generating normally distributed random numbers for Monte Carlo simulations
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should 
# have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

## Code: Monte Carlo simulation of probability of tallest person being over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height 
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

## Code: Plotting the normal distribution with dnorm

# Use d to plot the density function of a continuous distribution. Here is the 
# density function for the normal distribution (abbreviation norm):
  
  x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x,f)) +
  geom_line()


# DataCamp Assessment -----------------------------------------------------

# 1 # Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate the probability that a
# randomly selected female is shorter than 5 feet. Print this value to the console.
 
under_5 <- pnorm(60, mean = 64, sd = 3)
under_5

#2 # Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate the probability that a 
# randomly selected female is 6 feet or taller. Print this value to the console.

over_6 <- 1 - pnorm(72, mean = female_avg, sd = female_sd) 
over_6

# 3 # Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# Using variables 'female_avg' and 'female_sd', calculate the probability that a 
# randomly selected female is between the desired height range. Print this value 
# to the console.

h67 <- pnorm(67, female_avg, female_sd)
h61 <- pnorm(61, female_avg, female_sd)

h67 - h61

# 4 # Assign a variable 'female_avg' as the average female height. Convert this 
# value to centimeters.
female_avg <- 64*2.54

# Assign a variable 'female_sd' as the standard deviation for female heights. 
# Convert this value to centimeters.
female_sd <- 3*2.54

# Using variables 'female_avg' and 'female_sd', calculate the probability that a
# randomly selected female is between the desired height range. Print this value
# to the console.

c67 <- 67 * 2.54
c61 <- 61 * 2.54

h67 <- pnorm(c67, female_avg, female_sd)
h61 <- pnorm(c61, female_avg, female_sd)

h67 - h61

#  5 Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

# To a variable named 'taller', assign the value of a height that is one SD 
# taller than average.

taller <- pnorm(female_avg + female_sd, female_avg, female_sd)
shorter <- pnorm(female_avg - female_sd, female_avg, female_sd)

taller - shorter

# 6 # Assign a variable 'male_avg' as the average male height.
male_avg <- 69

# Assign a variable 'male_sd' as the standard deviation for male heights.
male_sd <- 3

# Determine the height of a man in the 99th percentile of the distribution.

tall_guy <- qnorm(p = 0.99, mean = 69, sd = 3)

#  7  #The variable `B` specifies the number of times we want the simulation to run.
B <- 1000

# Use the `set.seed` function to make sure your answer matches the expected 
# result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ score from 
# each random distribution of 10,000 people.

avg <- 100
s <- 15
highestIQ <-  replicate(B, {
  simulated_data <- rnorm(10^5, avg, s)    
  max(simulated_data)    
})

# Make a histogram of the highest IQ scores.
hist(highestIQ)