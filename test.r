candidates <- read.csv("data/candidate-support-by-race.csv")
preferences <- read.csv("data/racial-preference-by-year.csv")

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

# returns a candidate's score (margin of victory or percent of vote) in an election
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
    scores <- c()
    if ((row$R.Race == "White" & row$D.Race == "White") | row$D.Race != "White") {
        # democrat is minority
        scores <- c(get_candidate_score(row$D.White, row$R.White),
            get_candidate_score(row$D.Black, row$R.Black),
            get_candidate_score(row$D.Hispanic, row$R.Hispanic),
            get_candidate_score(row$D.Asian, row$R.Asian))
    }
    else {
        # republican is minority
        scores <- c(get_candidate_score(row$R.White, row$D.White),
            get_candidate_score(row$R.Black, row$D.Black),
            get_candidate_score(row$R.Hispanic, row$D.Hispanic),
            get_candidate_score(row$R.Asian, row$D.Asian))
    }

    return(scores)
}

# given a year, finds the expected scores (White, Black, Hispanic, Asian)
# for a generic democratic candidate
get_expected_democratic_scores <- function(year) {
    # TODO interpolate for years like 2013 - take average of surrounding years
    year_data <- preferences[preferences$Year == year]
    if (nrow(year_data) > 0) {
        # TODO finish
    }
    else {
        # this year isn't in the dataset but interpolate from the surroundings
        average <- (get_expected_democratic_scores(year + 1) + get_expected_democratic_scores(year - 1)) / 2
        return (average)
    }
}



# calculate how many elections we have for every race
races = list("White", "Black", "Hispanic", "Asian")
elections_per_race = lapply(races, {function(r) nrow(elections_by_race(r))})
print(elections_per_race)

# e.g. show cory booker's scores
print(get_minority_racial_scores(candidates[1,]))
