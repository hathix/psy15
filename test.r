#
# Contains data analysis and t-tests
#

# Returns summary statistics: for every minority, the mean and SD of their
# own-race boosts, white boosts, and other-minority boosts
summary_stats <- function() {
    data <- full_boost_data()

    minorities <- c("Black", "Hispanic", "Asian")

    # do this for every race
    # return a p-value
    map_fn <- function(race) {
        boosts <- boosts_of_race(data, race)

        # there are 3 types of boosts we're interested in:
        # - own race (self)
        # - white (white)
        # - other minority (other)
        # and for each, calculate the mean and SD

        self_boosts <- boosts[,p(race, 'Boost')]
        white_boosts <- boosts[,'WhiteBoost']
        others <- other_minorities(race)
        other_boosts <- capply(others, function(race) {
            return(boosts[,p(race, 'Boost')])
        })

        # also give the p-values of the self vs. white test
        # and the self vs. other test
        #
        # Self vs. white test:
        # Tests the hypothesis that a candidate of a particular race gets a
        # greater boost from his own race than from whites.
        # In other words,
        #   H_a = B_own > B_white
        #   H_0 = B_own = B_white
        #
        # Self vs. other test:
        # Tests the hypothesis that a candidate of a particular minority race gets a
        # greater boost from his own race than from other minority races.
        # In other words,
        #   H_a = B_own > B_others
        #   H_0 = B_own = B_others
        #
        self_white.p <- (t.test(self_boosts, white_boosts, alternative="greater"))[['p.value']]
        self_other.p <- (t.test(self_boosts, other_boosts, alternative="greater"))[['p.value']]

        return(c(
            mean(self_boosts, na.rm=TRUE),
            sd(self_boosts, na.rm=TRUE),
            mean(white_boosts, na.rm=TRUE),
            sd(white_boosts, na.rm=TRUE),
            mean(other_boosts, na.rm=TRUE),
            sd(other_boosts, na.rm=TRUE),
            self_white.p,
            self_other.p
        ))
    }

    raw_table <- sapply(minorities, map_fn)
    rownames(raw_table) <- c("self.mean", "self.sd", "white.mean", "white.sd", "other.mean", "other.sd", "self_white.p", "self_other.p")
    return (t(raw_table))
}
