## Section 3 Random Variables, Sampling Models, and the Central Limit Theorem

## Code: Modeling a random variable
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse((sample(beads, 1) == "blue"), 1, 0)
# demonstrate that the random variable is different every time
ifelse((sample(beads, 1) == "blue"), 1, 0)
ifelse((sample(beads, 1) == "blue"), 1, 0)
ifelse((sample(beads, 1) == "blue"), 1, 0)

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
 # 1000 draws from urn, -1 if red, else +1
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)   
X[1:10]    # first 10 outcomes

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

# We use the sampling model to run a Monte Carlo simulation and use the results 
# to estimate the probability of the casino losing money.

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 roulette spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

# We can plot a histogram of the observed values of S as well as the normal 
# density curve based on the mean and standard deviation of S.

library(tidyverse)
# sequence of 100 values across range of S
s <- seq(min(S), max(S), length = 100)    
# generate normal density for S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) 
# make data frame of S for histogram
data.frame (S = S) %>%    
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

## Central Limit Theorem
# with outcomes a and b, with probabilities of p and 1-p, respectively

# Expected value of a random variable
ap + b(1-p)

# Expected value of the sum of n draws of a random variable
n * (a*p + b(1-p))

# Standard deviation of an urn with two values
abs(b-a) * sqrt(p*(1-p))

# Standard error of the sum of n draws of a random variable
sqrt(n) * abs(b-a) * sqrt(p*(1-p))

#end


# DataCamp Assessment -----------------------------------------------------

# Ex. 1

# Assign a variable `p_green` as the probability of the ball landing in a 
# green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing 
# in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the 
# ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the 
# probability that you win money betting on green 100 times.

# n * (a*p + b(1-p))

pnorm(0, avg, se, lower.tail = FALSE)

# Ex. 2

# Assign a variable `p_green` as the probability of the ball landing in a green 
# pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in 
# a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. 
# Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected 
# result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations 
# and sums the outcomes.

S <- replicate(B, {
  x <- sample(c(17, -1), 100, replace = TRUE, prob = (c(p_green, p_not_green)))
sum(x)})


# Compute the average value for 'S'
mean(S)

# Calculate the standard deviation of 'S'
sd(S)

# Ex. 3

# Calculate the proportion of outcomes in the vector `S` that exceed $0

mean(S > 0)

# Ex. 5
# Use the `set.seed` function to make sure your answer matches the expected 
# result after random sampling.
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 10000

# Assign a variable `p_green` as the probability of the ball landing in a green 
# pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in 
# a green pocket
p_not_green <- 1 - p_green

# Create a vector called `X` that contains the outcomes of `n` bets

X <- sample(c(17, -1), n, replace = TRUE, prob = (c(p_green, p_not_green)))
  
# Define a variable `Y` that contains the mean outcome per bet. Print this mean 
# to the console.

Y <- mean(X)
Y

# Ex. 6

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1 - p_green

# Calculate the expected outcome of `Y`, the mean outcome per bet in 10,000 bets
# n * (a*p + b(1-p))

 17*p_green + (-1)*p_not_green

 # Ex. 7
 
 # Define the number of bets using the variable 'n'
 n <- 10000
 
 # Assign a variable `p_green` as the probability of the ball landing in a green pocket
 p_green <- 2 / 38
 
 # Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
 p_not_green <- 1 - p_green
 
 # Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
 
 (abs(p_green - p_not_green) * sqrt(p_green * p_not_green)) / sqrt(n)
 
 
 # end