# TODO refactor so that nothing uses these as global variables
candidates <- read.csv("data/candidate-support-by-race.csv")
preferences <- read.csv("data/racial-preference-by-year.csv")

# better version of paste() that concatenates strings without putting a space in betwen
p <- function(..., sep='') {
  paste(..., sep=sep, collapse=sep)
}

# maps a vector/list to a vector
# capply(vector, function)
capply <- function(v, f) {
    return(unlist(lapply(v, f)))
}

# standard order of races
races <- c('White', 'Black', 'Hispanic', 'Asian')

# given a row, returns TRUE if the minority is a democrat, else false.
is_minority_democrat <- function(row) {
  return((row['R.Race'] == "White" & row['D.Race'] == "White") | row['D.Race'] != "White")
}

# returns the race of the minority candidate in a particular election
minority_race_of_election <- function(row) {
    # our elections are all white vs. minority and white vs. white
    # so if one is a minority, they must be the only minority
    # and if no minorities, both must be white
    if (row[['D.Race']] != 'White') {
        return (row[['D.Race']])
    }
    else if (row[['R.Race']] != 'White') {
        return (row[['R.Race']])
    }
    else {
        return ('White')
    }
}

# returns a candidate's score: their ratio of support in an election.
# this is the ratio of people supporting them over the total number of people
# polled. this standardizes for polls where the total amount of support
# for the 2 major candidates is less than 100%.
candidate_score <- function(for_candidate, against_candidate) {
    # here, we give the percent of the vote they captured, scaled to 100%
    # e.g. if you had 60% for and 30% against, your score is 66%
    return (for_candidate / (for_candidate + against_candidate))
}

# given a row of the candidates table, returns a vector of the minority's scores
# for each race
# vector contains (White, Black, Hispanic, Asian)
# in white-white elections, the democrat (arbitrarily chosen) is the minority
minority_scores <- function(row) {
    # column names are like `R.White` and `D.Asian` so we have to prepend the
    # party label before the race
    for_party <- if (is_minority_democrat(row)) 'D.' else 'R.'
    against_party <- if (is_minority_democrat(row)) 'R.' else 'D.'

    # for every race, get the candidate's score for that race
    map_fn <- function(race){
        for_candidate <- as.numeric(row[[p(for_party, race)]])
        against_candidate <- as.numeric(row[[p(against_party, race)]])
        return (candidate_score(for_candidate, against_candidate))
    }
    return (capply(races, map_fn))
}

# given a year, finds the expected scores (White, Black, Hispanic, Asian)
# for a generic democratic candidate
expected_democratic_scores <- function(year) {
    year_data <- preferences[preferences$Year == year,]
    if (nrow(year_data) > 0) {
# for every race, get a generic democrat's score for that race
        # we only track the democratic% since the data given to us
        # have democratic% + republican% = 100%, so we can just subtract
        # the D% from 100% to get R%
        map_fn <- function(race){
            for_candidate <- as.numeric(year_data[[p(race, '.D.')]])
            against_candidate <- 100 - for_candidate
            return (candidate_score(for_candidate, against_candidate))
        }
        return (capply(races, map_fn))
    }
    else {
        # this year isn't in the dataset but interpolate from the surrounding years
        average <- (expected_democratic_scores(year + 1) + expected_democratic_scores(year - 1)) / 2
        return (average)
    }
}

# finds the expected score for a generic candidate of the party of the minority candidate
# in the given election
expected_scores <- function(row) {
    democrat_expected <- expected_democratic_scores(row[['Year']])
    expected <- if (is_minority_democrat(row)) democrat_expected else 1 - democrat_expected
    return (expected)
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

# given a row of the candidates table, returns a vector (White, Black, Hispanic, Asian)
# of the minority candidate's boost,
# where boost is (actual score for candidate) - (expected score for year)
minority_boost <- function(row) {
  return (minority_scores(row) - expected_scores(row))
}

# Returns a table of racial boosts
racial_boosts <- function() {
    # map boost over all candidates
    boosts <- apply(candidates, 1, minority_boost)

    # get list of minorities per election
    minorities <- apply(candidates, 1, minority_race_of_election)

    # merge minority and boost data to get one big matrix
    # each row = candidate, W boost, B boost, H boost, A boost
    # note that this turns all the numbers into strings... can extract/numerify later
    combined <- cbind(matrix(minorities), t(boosts))
    colnames(combined) <- list("MinorityRace", "WhiteBoost", "BlackBoost", "HispanicBoost", "AsianBoost")

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

boosts <- racial_boosts()
