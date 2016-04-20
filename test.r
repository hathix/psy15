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

#
# Returns a table of own-race vs. other-minority boosts for all candidates
# of the given race. This data is not paired.
#
own_other_boosts <- function(race) {
    # data <- full_boost_data()
    # minorities <- c("Black", "Hispanic", "Asian")
    #
    # # do this for every minority
    # map_fn <- function(race) {
        boosts <- boosts_of_race(data, race)
        own_boosts <- boosts[, p(race, 'Boost')]

        # get boosts from other minorities.
        # this contains "Black", "Asian", etc.
        others <- other_minorities(race)
        other_boosts <- capply(others, function(race) {
            return(boosts[, p(race, 'Boost')])
        })

        # make them the same length
        equalize(own_boosts, other_boosts)

        return(cbind(own_boosts, other_boosts))

        # # now run t-test to compare these
        # ttest <- t.test(own_boosts, other_boosts, alternative="greater")
        # return (ttest[['p.value']])
    # }
    #
    # return (capply(minorities, map_fn))

    # data <- full_boost_data()
    #
    # # filter out whites
    # minority_data <- data[data[,'MinorityRace'] != 'White',]
    #
    # # for each row, gather only the own-race boost and the other-minority boost
    # # currently, we have boosts for every race
    # # so map every row
    # row_map <- function(row) {
    #     # e.g. "Black"
    #     minority <- row['MinorityRace']
    #
    #     # convert rest of row to numeric data
    #     boosts <- row[2:5]
    #     class(boosts) <- "numeric"
    #
    #     # own boost = just for this race
    #     own_boost <- boosts[p(race, 'Boost')]
    #
    #     # other boost = average of boosts for other minorities
    #     others <- other_minorities(race)
    #     other_boost_vector <- capply(others, function(race) {
    #         return(boosts[p(race, 'Boost')])
    #     })
    #     # now take the mean of this vetor
    #     other_boost <- mean(other_boost_vector, na.rm=TRUE)
    # }
    #
    # # do this for every minority
    # map_fn <- function(race) {
    #     boosts <- boosts_of_race(data, race)
    #     own_boosts <- boosts[, p(race, 'Boost')]
    #
    #     # get boosts from other minorities.
    #     # this contains "Black", "Asian", etc.
    #     others <- other_minorities(race)
    #     other_boosts <- capply(others, function(race) {
    #         return(boosts[, p(race, 'Boost')])
    #     })
    #
    #     # now run t-test to compare these
    #     ttest <- t.test(own_boosts, other_boosts, alternative="greater")
    #     return (ttest[['p.value']])
    # }
    #
    # return (capply(minorities, map_fn))
}

# Returns summary statistics: for every minority, the mean and SD of their
# own-race boosts, white boosts, and other-minority boosts
# TODO run t-tests and put p's in here too
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
