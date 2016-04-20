#
# Calculates boosts: the difference between a candidate's actual score (their ratio of the vote)
# to a generic candidate's expected score (their ratio of the vote)
#

# given a row of the candidates table, returns a vector (White, Black, Hispanic, Asian)
# of the minority candidate's boost,
# where boost is (actual score for candidate) - (expected score for year)
minority_boost <- function(row) {
  return (minority_scores(row) - expected_scores(row))
}

# Given the combined race / boosts matrix and a candidate race, returns the median
# boost by voter race
boosts_of_race <- function(combined, race) {
  # filter out only the elections with candidates of that race
  elections_of_race <- combined[combined[,'MinorityRace'] == race,]
  # extract just the boosts now, and convert it back into numbers
  boosts <- elections_of_race[,2:5]
  class(boosts) <- "numeric"

  # find the median boost per race
  medians <- apply(boosts, 2, function(col) { return(median(col, na.rm=TRUE))})
  return (medians)
}

# for every election, returns the race of the minority candidate
# and their boosts for every racial group.
# this is the jumping-off point for every analysis.
full_boost_data <- function() {
    # map boost over all candidates
    boosts <- apply(candidates, 1, minority_boost)

    # get list of minorities per election
    minorities <- apply(candidates, 1, minority_race_of_election)

    # merge minority and boost data to get one big matrix
    # each row = candidate, W boost, B boost, H boost, A boost
    # note that this turns all the numbers into strings... can extract/numerify later
    combined <- cbind(matrix(minorities), t(boosts))
    colnames(combined) <- list("MinorityRace", "WhiteBoost", "BlackBoost", "HispanicBoost", "AsianBoost")

    return (combined)
}

# Returns a table of racial boosts
racial_boosts <- function() {
    combined <- full_boost_data()

    # calculate the boosts for every race
    racial_boosts <- rbind(
      boosts_of_race(combined, "White"),
      boosts_of_race(combined, "Black"),
      boosts_of_race(combined, "Hispanic"),
      boosts_of_race(combined, "Asian")
    )
    rownames(racial_boosts) <- races

    return(racial_boosts)
}
