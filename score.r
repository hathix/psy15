#
# Calculates scores: a candidate's ratio of the vote. See candidate_score()
# for more information about scores.
#

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
