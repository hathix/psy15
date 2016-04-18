candidates <- read.csv("~/Documents/Git/psy15/data/candidate-support-by-race.csv")

# returns a subset of elections by the race of the minority candidate involved
elections_by_race <- function(candidates, race) {
    if (race == "White") {
        # "White" elections have white-on-white
        return(candidates[candidates$R.Race==race & candidates$D.Race==race,])
    }
    else {
        # other elections have white-on-race
        return(candidates[candidates$R.Race==race | candidates$D.Race==race,])
    }
}

# calculate how many elections we have for every race
races = list("White", "Black", "Hispanic", "Asian")
elections_per_race = lapply(races, {function(r) nrow(elections_by_race(candidates, r))})
print(elections_per_race)
