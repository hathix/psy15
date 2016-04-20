source("util.r")
source("data.r")
source("minority.r")
source("score.r")
source("boost.r")
source("test.r")

boosts <- all_median_boosts()
res <- test_same_race_boosts()

