#
# Loads data from CSV's, and constants.
#

# TODO refactor so that nothing uses these as global variables
candidates <- read.csv("data/candidate-support-by-race.csv")
preferences <- read.csv("data/racial-preference-by-year.csv")

## constants
# standard order of races
races <- c('White', 'Black', 'Hispanic', 'Asian')
