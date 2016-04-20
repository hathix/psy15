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
    # do this for every race
    # return a p-value
    map_fn <- function(race) {
        data <- full_boost_data()
        boosts <- boosts_of_race(data, race)

        # consider only WhiteBoost and `Race`Boost
        main_boosts <- boosts[,c('WhiteBoost', p(race, 'Boost'))]

        # disregard any rows (elections) where the race boost is N/A
        # (namely, we don't have data on how voters of the candidate's race
        # voted for them)
        filtered_boosts <- main_boosts[
            !is.na(main_boosts[,1]) & !is.na(main_boosts[,2]),]

        # paired=TRUE here since each white/race boost is paired
        # (both come from the same election)
        ttest <- t.test(filtered_boosts[,2], filtered_boosts[,1],
            paired=TRUE, alternative="greater")
        return (ttest[['p.value']])
    }

    return (capply(races, map_fn))
}
