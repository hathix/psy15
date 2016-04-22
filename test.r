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
            length(na.omit(self_boosts)),
            mean(white_boosts, na.rm=TRUE),
            sd(white_boosts, na.rm=TRUE),
            length(na.omit(white_boosts)),
            mean(other_boosts, na.rm=TRUE),
            sd(other_boosts, na.rm=TRUE),
            length(na.omit(other_boosts)),
            self_white.p,
            self_other.p
        ))
    }

    raw_table <- sapply(minorities, map_fn)
    rownames(raw_table) <- c(
      "self.mean", "self.sd", "self.n",
      "white.mean", "white.sd", "white.n",
      "other.mean", "other.sd", "other.n",
      "self_white.p", "self_other.p")
    return (raw_table)
}

voter_stats_by_race <- function(race) {
  data <- as.data.frame(full_boost_data())
  # get just the boosts for this race
  race_boosts <- data[,c("MinorityRace", p(race, "Boost"))]
  
  # filter apart white-white elections
  white_boosts <- race_boosts[race_boosts[,'MinorityRace'] == 'White',]
  minority_boosts <- race_boosts[race_boosts[,'MinorityRace'] != 'White',]
  
  # get just the boosts for this race
  white_race_boosts <- as.vector(na.omit(white_boosts[, p(race, "Boost")]))
  class(white_race_boosts) <- "numeric"
  minority_race_boosts <-  as.vector(na.omit(minority_boosts[, p(race, "Boost")]))
  class(minority_race_boosts) <- "numeric"
  
  return(c(
    mean(white_race_boosts),
    sd(white_race_boosts),
    length(white_race_boosts),
    mean(minority_race_boosts),
    sd(minority_race_boosts),
    length(minority_race_boosts),
    t.test(minority_race_boosts, white_race_boosts, alternative="greater")[['p.value']]
  ))
}

voter_stats <- function() {
  minorities <- c("Black", "Hispanic", "Asian")
  raw_table <- sapply(minorities, voter_stats_by_race)
  
  rownames(raw_table) <- c(
    "white.mean",
    "white.sd",
    "white.n",
    "minorities.mean",
    "minorities.sd",
    "minorities.n",
    "p"
  )
  
  return (raw_table)
}
