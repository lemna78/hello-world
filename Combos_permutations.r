############ Probability

# 1.2 Combinations and Permutations

# paste joins two strings and inserts a space in between.

# expand.grid gives the combinations of 2 vectors or lists.

# permutations(n,r) from the gtools package lists the different ways that r 
# number of items can be selected from a set of n options when order matters.

# combinations(n,r) from the gtools package lists the different ways that r 
# number of items can be selected from a set of n options when order does not
# matter.

options(digits = 10)
# Code: Introducing paste and expand.grid

# joining strings with paste
number <- "Three"
hearts <- "Hearts"
paste(number, "of", hearts)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

## Code: Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", 
             "Nine", "Ten", "Jack", "Queen", "King")
# create the suit/number combinations, as a matrix or table
deck <- expand.grid(number = numbers, suit = suits)
# create the deck as a character vector, length 52
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

## Code: Permutations and Combinations

library(gtools)
permutations(n = 5, r = 2) # ways to choose 2 numbers in order from 1:5

permutations(n = 3, r = 2)
combinations(n =3, r = 2)
             
## Code: Probability of drawing a second king given that one king is drawn

hands <- permutations(n = 52, r = 2, v = deck)
first_card <- hands[, 1]
second_card <- hands[, 2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

## Code: Probability of a natural 21 in blackjack (Ace + face card)
#define Aces
aces <- paste("Ace", suits)

# define face card
facecard <- c("King", "Queen", "Jack", "Ten")
# generate the possible combinations
facecard <- expand.grid(number = facecard, suit = suits)
#generate the character vector
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v = deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in 'combinations'
mean(hands[, 1] %in% aces & hands[ ,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second

mean((hands[ , 1] %in% aces & hands[ , 2] %in% facecard)| 
       (hands[,2] %in% aces & hands[,1] %in% facecard))

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

## Code: Monte Carlo simulation of natural 21 in blackjack
# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | 
    (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

##### The Birthday Problem

### Code: The birthday problem

# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays

# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)


# Element-wise operations over vectors and sapply -------------------------

x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

#Computing birthday problem probabilities with sapply

# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

### Code: Estimating a practical value of B
B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 



# DataCamp Assessment -----------------------------------------------------


## Ex. 2

cyan <- 3
magenta <- 5
yellow <- 7

# Assign the variable 'p_yellow' as the probability that a yellow ball is drawn
# from the box.
p_yellow <- yellow/(yellow + magenta + cyan)

# Using the variable 'p_yellow', calculate the probability of drawing a yellow 
#ball on the sixth draw. Print this value to the console.

## Ex. 3 Rolling a die
# Assign the variable 'p_no6' as the probability of not seeing a 6 on a single roll.
p_no6 <- 5/6

# Calculate the probability of not seeing a 6 on six rolls using `p_no6`. 
#Print your result to the console: do not assign it to a variable.
p_no6^6

## Ex. 4
# Assign the variable `p_cavs_win4` as the probability that the Cavs will win 
# the first four games of the series.
p_cavs_win4 <- 0.6^4

# Using the variable `p_cavs_win4`, calculate the probability that the Celtics 
#win at least one game in the first four games of the series.
1- p_cavs_win4

## Ex. 5

# This line of example code simulates four independent random games where the 
# Celtics either lose or win. Copy this example code to use within the `replicate` 
# function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. 
# Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected 
# result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that replicates two steps for B 
# iterations: (1) generating a random four-game series `simulated_games` using 
# the example code, then (2) determining whether the simulated series contains at 
# least one win for the Celtics.

celtic_wins <- replicate(B, {
 simulated_games  <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
any("win" %in% simulated_games)})


# Calculate the frequency out of B iterations that the Celtics won at least one 
# game. Print your answer to the console.

mean(celtic_wins)