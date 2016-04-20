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
