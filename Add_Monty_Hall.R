#### Addition Rule and Monty Hall

### The Monty Hall Problem

# Same Door Simulation ----------------------------------------------------

B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door 
  #with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

# Monte Carlo Simulation of switch strategy --------------------------------------------------

switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door 
  #with no prize that isn't chosen
  switch <- doors[!doors %in% c(my_pick, show)]    # switch to the door that 
  #wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching


# DataCamp Assessment -----------------------------------------------------

# Ex. 1

## Assign a variable 'n' as the number of remaining games.
n <- 6

## Assign a variable `outcomes` as a vector of possible game outcomes, where 0 
# indicates a loss and 1 indicates a win for the Cavs.
outcomes <- c(0, 1)

## Assign a variable `l` to a list of all possible outcomes in all remaining 
# games. Use the `rep` function on `list(outcomes)` to create list of length `n`.

l <- rep(list(outcomes), n) 

## Create a data frame named 'possibilities' that contains all combinations of 
# possible outcomes for the remaining games.

possibilities <- expand.grid(l)
possibilities
## Create a vector named 'results' that indicates whether each row in the data 
# frame 'possibilities' contains enough wins for the Cavs to win the series.

results1 <- (apply(possibilities, MARGIN = 1, FUN = sum)) >= 4

mean(results1)

# Ex. 2

# The variable `B` specifies the number of times we want the simulation to run. 
# Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected 
# result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a 
# simulated series and determines whether that series contains at least four 
# wins for the Cavs.

results2 <- replicate(B, {
  sum(sample(c(0,1), 6, replace = TRUE)) >= 4
})


# Calculate the frequency out of `B` iterations that the Cavs won at least four 
# games in the remainder of the series. Print your answer to the console.

mean(results2)

# Ex. 3
# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team 
# B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A 
# will win to determine the probability that team B will win. Call this object 'Pr'.

Pr <- sapply(p, prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.

plot(p, Pr)

# Ex. 4

# Given a value 'p', the probability of winning the series for the underdog team 
# B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

# Assign the variable 'N' as the vector of series lengths. Use only odd numbers 
# ranging from 1 to 25 games.
N <- seq(1, 25, 2)

# Apply the 'prob_win' function across the vector of series lengths to determine 
# the probability that team B will win. Call this object `Pr`.
Pr <- sapply(N, prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N, Pr)
# end