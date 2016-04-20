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
        # compare WhiteBoost to [Race]Boost
        white_boosts <- boosts[,p('WhiteBoost')]
        race_boosts <- boosts[,p(race, 'Boost')]

        # white and race boosts are both lists where the
        # corresponding elements

        print(race)
        print(white_boosts)
        print(race_boosts)

        # paired=TRUE here since each white/race boost is paired
        # (both come from the same election)
        ttest <- t.test(race_boosts, white_boosts, paired=TRUE, alternative="greater")
        return (ttest[['p.value']])
    }

    return (capply(races, map_fn))
}
