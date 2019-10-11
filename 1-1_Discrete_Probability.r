#### Data Science: Probability

# Section 1.1  Introduction to Discrete Probability

# Monte Carlo Simulations

# The rep function and the sample function

beads <- rep(c("red", "blue"), times = c(2, 3)) # create an urn with 2 red, 3 blue
beads   # view beads object

sample(beads, 1)    # sample 1 bead at random

# Monte Carlo simulation

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)  # make a table of outcome events
tab     # view count table
prop.table(tab)     # view table of outcome proportions

#### Assessment Exercises

# Exercise 1 Probability of drawing cyan ball

cyan <- 3
magenta <- 5
yellow <- 7

# Assign a variable `p` as the probability of choosing a cyan ball from the box
p <- cyan/(cyan + magenta + yellow)

# Print the variable `p` to the console
p

# Exercise 2 Probability of drawing non-cyan ball

p <- (magenta + yellow )/(cyan + magenta + yellow)

p

# Exercise 3  Sampling without replacement

cyan <- 3
magenta <- 5
yellow <- 7

# The variable `p_1` is the probability of choosing a cyan ball from the box on
# the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable `p_2` as the probability of not choosing a cyan ball on the 
# second draw without replacement.
p_2 <- (magenta + yellow)/(magenta + yellow + cyan - 1)
p_2
# Calculate the probability that the first draw is cyan and the second draw is 
# not cyan using `p_1` and `p_2`.
p_1 * p_2

# Exercise 4

cyan <- 3
magenta <- 5
yellow <- 7

# The variable 'p_1' is the probability of choosing a cyan ball from the box on 
# the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable 'p_2' as the probability of not choosing a cyan ball on the 
# second draw with replacement.
p_2 <- (magenta + yellow)/(cyan + magenta + yellow)

# Calculate the probability that the first draw is cyan and the second draw is 
# not cyan using `p_1` and `p_2`.
p_1 * p_2


