source("util.r")
source("data.r")
source("minority.r")
source("score.r")
source("boost.r")
source("test.r")

ss <- summary_stats()
write.table(ss, "test.csv")
