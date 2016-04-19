candidates <- read.csv("data/candidate-support-by-race.csv")
lpreferences <- read.csv("data/racial-preference-by-year.csv")

# given a row, runs `if_democrat()` if the minority candidate in that election
# is a democrat, otherwise runs `if_republican()`
minority_logic_test <- function(row, if_democrat, if_republican) {
  if ((row$R.Race == "White" & row$D.Race == "White") | row$D.Race != "White") {
    # democrat is minority
    return (if_democrat())
  }
  else {
    # republican is minority
    return (if_republican())
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
    # how to extract scores based on whether democrat or republican is minority
    if_democrat <- function() {
      return (c(get_candidate_score(row$D.White, row$R.White),
        get_candidate_score(row$D.Black, row$R.Black),
        get_candidate_score(row$D.Hispanic, row$R.Hispanic),
        get_candidate_score(row$D.Asian, row$R.Asian)))
    }
    if_republican <- function() {
      return (c(get_candidate_score(row$R.White, row$D.White),
        get_candidate_score(row$R.Black, row$D.Black),
        get_candidate_score(row$R.Hispanic, row$D.Hispanic),
        get_candidate_score(row$R.Asian, row$D.Asian)))
    }
    
    scores <- minority_logic_test(row, if_democrat, if_republican)
    return (scores)
}

# given a year, finds the expected scores (White, Black, Hispanic, Asian)
# for a generic democratic candidate
get_expected_democratic_scores <- function(year) {
    year_data <- preferences[preferences$Year == year,]
    if (nrow(year_data) > 0) {
        # our csv stores percentage points (e.g. 45), we need to convert to percent (e.g. 0.45)
        return (c(year_data$White.D., year_data$Black.D., year_data$Hispanic.D., year_data$Asian.D.) / 100)
    }
    else {
        # this year isn't in the dataset but interpolate from the surrounding years
        average <- (get_expected_democratic_scores(year + 1) + get_expected_democratic_scores(year - 1)) / 2
        return (average)
    }
}

# given a row of the candidates table, returns a vector (White, Black, Hispanic, Asian)
# of the minority candidate's boost,
# where boost is (actual score for candidate) - (expected score for year)
get_minority_boost <- function(row) {
  # we calculate expected score for democrats... if minority is republican we need to take the inverse
  democrat_expected <- get_expected_democratic_scores(row$Year)
  expected <- minority_logic_test(row, function(){ return(democrat_expected)}, function(){return (1 - democrat_expected)})
  
  return (get_minority_racial_scores(row) - expected)
}


# calculate how many elections we have for every race
races = list("White", "Black", "Hispanic", "Asian")
elections_per_race = lapply(races, {function(r) nrow(elections_by_race(r))})
print(elections_per_race)

# e.g. show cory booker's scores
print(get_minority_racial_scores(candidates[1,]))
print(get_minority_boost(candidates[1,]))
