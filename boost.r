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
get_minority_in_election <- function(row) {
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

# returns a subset of elections by the race of the minority candidate involved
elections_by_race <- function(race) {
    if (race == "White") {
        # "White" elections have white-on-white
        return(candidates[candidates$R.Race==race & candidates$D.Race==race,])
    }
    else {
        # other elections have white-on-race
        return(candidates[candidates$R.Race==race | candidates$D.Race==race,])
    }
}

# returns a candidate's score (margin of victory, i.e. percent of vote) in an election
get_candidate_score <- function(for_candidate, against_candidate) {
    # here, we give the percent of the vote they captured, scaled to 100%
    # e.g. if you had 60% for and 30% against, your score is 66%
    return (for_candidate / (for_candidate + against_candidate))
}

# given a row of the candidates table, returns a vector of the minority's scores
# for each race
# vector contains (White, Black, Hispanic, Asian)
# in white-white elections, the democrat (arbitrarily chosen) is the minority
get_minority_racial_scores <- function(row) {

    # column names are like `R.White` and `D.Asian` so we have to prepend the
    # party label before the race
    for_party <- if (is_minority_democrat(row)) 'D.' else 'R.'
    against_party <- if (is_minority_democrat(row)) 'R.' else 'D.'

    # for every race, get the candidate's score for that race
    map_fn <- function(race){
        for_candidate <- as.numeric(row[[p(for_party, race)]])
        against_candidate <- as.numeric(row[[p(against_party, race)]])
        return (get_candidate_score(for_candidate, against_candidate))
    }
    return (capply(races, map_fn))
}

# given a year, finds the expected scores (White, Black, Hispanic, Asian)
# for a generic democratic candidate
get_expected_democratic_scores <- function(year) {
    year_data <- preferences[preferences$Year == year,]
    if (nrow(year_data) > 0) {
        # our csv stores percentage points (e.g. 45), we need to convert to percent (e.g. 0.45)
        # TODO refactor to use get_candidate_score (similarly to above fn)
        return (c(year_data$White.D., year_data$Black.D., year_data$Hispanic.D., year_data$Asian.D.) / 100)
    }
    else {
        # this year isn't in the dataset but interpolate from the surrounding years
        average <- (get_expected_democratic_scores(year + 1) + get_expected_democratic_scores(year - 1)) / 2
        return (average)
    }
}

# Given the combined race / boosts matrix and a candidate race, returns the median
# boost by voter race
boosts_by_race <- function(combined, race) {
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
get_minority_boost <- function(row) {
  # we calculate expected score for democrats... if minority is republican we need to take
  # 1 - that
  democrat_expected <- get_expected_democratic_scores(row[['Year']])
  expected <- if (is_minority_democrat(row)) democrat_expected else 1 - democrat_expected

  return (get_minority_racial_scores(row) - expected)
}


# calculate how many elections we have for every race
elections_per_race <- lapply(races, {function(r) nrow(elections_by_race(r))})
# print(elections_per_race)

# e.g. show cory booker's scores
# print(get_minority_racial_scores(candidates[1,]))
# print(get_minority_boost(candidates[1,]))

# map boost over all candidates
boosts <- apply(candidates, 1, function(row) {
  z <- noquote(row)
  return (get_minority_boost(z))
})

# get list of minorities per election
minorities <- apply(candidates, 1, get_minority_in_election)

# merge minority and boost data to get one big matrix
# each row = candidate, W boost, B boost, H boost, A boost
# note that this turns all the numbers into strings... can extract/numerify later
combined <- cbind(matrix(minorities),t(boosts))
colnames(combined) <- list("MinorityRace", "WhiteBoost", "BlackBoost", "HispanicBoost", "AsianBoost")

# calculate the boosts for every race
racial_boosts <- rbind(
  boosts_by_race(combined, "White"),
  boosts_by_race(combined, "Black"),
  boosts_by_race(combined, "Hispanic"),
  boosts_by_race(combined, "Asian")
)
rownames(racial_boosts) <- races
