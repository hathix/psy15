#
# Utility functions for finding out who's the minority.
#

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

# given a certain minority race, returns a vector of the other
# minority races
# e.g. other_minorities("Black") == c("Hispanic", "Asian")
# if the given race is White, this just returns all minorities
other_minorities <- function(race) {
    if (race == "Black") {
        return(c("Hispanic", "Asian"))
    }
    else if (race == "Hispanic") {
        return(c("Black", "Asian"))
    }
    else if (race == "Asian"){
        return(c("Black", "Hispanic"))
    }
    else {
        return(c("Black", "Hispanic", "Asian"))
    }
}
