source("util.r")
source("data.r")
source("minority.r")
source("score.r")
source("boost.r")
source("test.r")

stats <- summary_stats()
write.csv(stats, "out.csv")
