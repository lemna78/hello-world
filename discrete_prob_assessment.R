library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits
# 1A
nrow(permutations(n = 8, r = 3))

#official solution

# 1A official solution ----------------------------------------------------

library(gtools)
medals <- permutations(8,3)
nrow(medals)

# 1B ----------------------------------------------------------------------

jmedals <- permutations(3, 3)
nrow(jmedals)

#1C
nrow(jmedals)/nrow(medals)

#1D
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", 
             "France", "South Africa")
non_j_runners <- runners[4:8]
set.seed(1)

B <- 10000
is.jamaica <- function(x) ifelse(x == "Jamaica", TRUE, FALSE)
  
mc_model <- replicate(B, {
    medals <- sample(runners, 3, replace = FALSE)})

mc_modelj <- is.jamaica(mc_model)

result <- apply(mc_modelj, MARGIN = 2, FUN = "all")
mean(result)



# 1D Official solution ----------------------------------------------------

set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", 
             "France", "South Africa")
B <- 10000
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)


# Question 2a --------------------------------------------------------------

entrees <- paste("entree", 1:6)
combinations(6, 2)
sides <- paste("side", 1:15)

drinks <- paste("drink", 1:2)

options <- expand.grid(entrees, sides, drinks)
nrow(unique(options))


# Question 2b -------------------------------------------------------------

entrees <- paste("entree", 1:6)
combinations(6, 2)
sides <- paste("side", 1:15)

drinks <- paste("drink", 1:3)

options <- expand.grid(entrees, sides, drinks)
nrow(unique(options))


# Question 2c -------------------------------------------------------------

entrees <- paste("entree", 1:6)
side_combos <- combinations(6, 3)
sides <- paste("side", 1:nrow(side_combos))

drinks <- paste("drink", 1:3)

options <- expand.grid(entrees, sides, drinks)
nrow(unique(options))


# Question 2d -------------------------------------------------------------


}
choices <- function(x) {
  entrees <- paste("entree", 1:x)
  side_combos <- combinations(6, 2)
  sides <- paste("side", 1:nrow(side_combos))
  drinks <- paste("drink", 1:3)
  options <- expand.grid(entrees, sides, drinks)
  nrow(unique(options))}

sapply(1:12, FUN = "choices")


# Question 2e -------------------------------------------------------------

choices <- function(x) {
  entrees <- paste("entree", 1:6)
  side_combos <- combinations(x, 2)
  sides <- paste("side", 1:nrow(side_combos))
  drinks <- paste("drink", 1:3)
  options <- expand.grid(entrees, sides, drinks)
  nrow(unique(options))}

sapply(2:12, FUN = "choices")


# Question 3 --------------------------------------------------------------

data(esoph)
head(esoph)
library(tidyverse)

# 3a How many groups in study?
nrow(esoph)

# 3b How many cases?
all_cases <- sum(esoph$ncases)
all_cases

# 3c How many controls?

all_controls <- sum(esoph$ncontrols)
all_controls

# Question 4 --------------------------------------------------------------

# 4a What is the probability that a subject in the highest alcohol consumption
# group is a cancer case?

levels(esoph$alcgp)
hi_alc <- esoph %>% filter(alcgp == "120+") 
hi_alc_prob <- sum(hi_alc$ncases) / (sum(hi_alc$ncases) + sum(hi_alc$ncontrols))

# Solution:

esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# 4b What is the probability that a subject in the lowest alcohol consumption
# group is a cancer case?

lo_alc <- esoph %>% filter(alcgp == "0-39g/day")
lo_alc_prob <- sum(lo_alc$ncases) / (sum(lo_alc$ncases) + sum(lo_alc$ncontrols))

# Solution:

esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

# 4c Given that a person is a case, what is the probability that they
# smoke 10g or more a day?

esoph %>% 
  group_by(tobgp) %>%
  summarize(sumcases = sum(ncases), sumcontrols = sum(ncontrols), sumall = 
              sumcases + sumcontrols) 

# Solution:

tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()

tob_cases/all_cases

# 4d Given that a person is a control, what is the probability that they smoke
# 10g or more per day?

tob_control <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()

  tob_control/all_controls
  
# Question 5a For cases, what is the probability of being in the highest alcohol
# group?
  
alc_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()

alc_cases/all_cases

# Question 5b For cases, what is the probability of being in the highest tobacco
# group?
levels(esoph$tobgp)

hitob_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

hitob_cases/all_cases

# Question 5c For cases, what is the probability of being in the highest alcohol
# group and the highest tobacco group?

hi_cases <- esoph %>%
  filter(tobgp == "30+" & alcgp == "120+" ) %>%
  pull(ncases) %>%
  sum()

hi_cases/all_cases

# 5d For cases, what is the probability of being in the highest alcohol group 
# or the highest tobacco group?

med_cases <- esoph %>%
  filter(tobgp == "30+" | alcgp == "120+" ) %>%
  pull(ncases) %>%
  sum()

med_cases/all_cases

# 6a For controls, what is the probability of being in the highest alcohol group?

drunk_controls <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()

drunk_controls/all_controls

# 6b How many times more likely are cases than controls to be in the highest
# alcohol group?

drunk_cases <- esoph %>%
  filter(alcgp == "120+") %>%
  pull(ncases) %>%
  sum()

pdc <- drunk_cases/all_cases

pdcon <- drunk_controls/all_controls

pdc/pdcon


# 6c For controls, what is the probability of being in the highest tobacco group?

hi_tob_controls <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>% 
  sum()

hi_tob_controls/all_controls

# 6d For controls, what is the probability of being in the highest alcohol group
# and the highest tobacco group?

hi_controls <- esoph %>%
  filter(tobgp == "30+" & alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()

hi_controls/all_controls

# 6e For controls, what is the probability of being in the highest alcohol group 
# or the highest tobacco group?

med_controls <- esoph %>%
  filter(tobgp == "30+" | alcgp == "120+") %>%
  pull(ncontrols) %>%
  sum()

med_controls/all_controls

# 6f How many times more likely are cases than controls to be in the highest
# alcohol group or the highest tobacco group?

pbmedcases <- med_cases/all_cases
pbmedcontrols <- med_controls/all_controls

pbmedcases/pbmedcontrols
