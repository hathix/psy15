#
# Contains t-tests.
#

#
# Tests the hypothesis that a candidate of a particular race gets a
# greater boost from his own race than from whites.
# In other words,
#   H_a = B_own > B_white
#   H_0 = B_own = B_white
#
test_same_race_boosts <- function() {
    data <- full_boost_data()
    # do this for every race
    # return a p-value
    map_fn <- function(race) {
        boosts <- boosts_of_race(data, race)

        # consider only WhiteBoost and `Race`Boost
        main_boosts <- boosts[,c('WhiteBoost', p(race, 'Boost'))]

        # disregard any rows (elections) where the race boost is N/A
        # (namely, we don't have data on how voters of the candidate's race
        # voted for them)
        # filtered_boosts <- main_boosts[
            # !is.na(main_boosts[,1]) & !is.na(main_boosts[,2]),]

        ttest <- t.test(main_boosts[,2], main_boosts[,1], alternative="greater")
        return (ttest[['p.value']])
    }

    return (capply(races, map_fn))
}

#
# Tests the hypothesis that a candidate of a particular minority race gets a
# greater boost from his own race than from other minority races.
# In other words,
#   H_a = B_own > B_others
#   H_0 = B_own = B_others
#
test_minority_boosts <- function(){
    data <- full_boost_data()
    minorities <- c("Black", "Hispanic", "Asian")

    # do this for every minority
    map_fn <- function(race) {
        boosts <- boosts_of_race(data, race)
        own_boosts <- boosts[, p(race, 'Boost')]

        # get boosts from other minorities.
        # this contains "Black", "Asian", etc.
        others <- other_minorities(race)
        other_boosts <- capply(others, function(race) {
            return(boosts[, p(race, 'Boost')])
        })

        # now run t-test to compare these
        ttest <- t.test(own_boosts, other_boosts, alternative="greater")
        return (ttest[['p.value']])
    }

    return (capply(minorities, map_fn))
}
