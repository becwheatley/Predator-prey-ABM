#----------------------------------------------------------------------------------------------------------------
# 'Habitat features and performance interact to determine the outcomes of terrestrial predator-prey pursuits'
# Wheatley R, Pavlic TP, Levy O, & Wilson RS
# Case study: cheetah and impala in open savanna and acacia thicket
# Code by Rebecca Wheatley
# Last Modified 10 June 2020
#-----------------------------------------------------------------------------------------------

#---------------------------------------------
# I. OPEN SAVANNA
#---------------------------------------------

# Load open savanna data
os <- read.csv(".../Data/Cheetah vs impala experiment/predator-prey-model-revised-cheetah_vs_impala-open_savanna.csv", header=T, skip = 6)

# Because stats are for "successful" hunts and pursuits, we are only interested in the impala's probability of escaping if
# detected (as pursuits only occur when the cheetah detects the impala)
os.detected <- subset(os, prey.detected == 1)

# Calculate the impala's probability of escaping if detected
open.savanna <- ave(os.detected$prey.win, FUN = mean)
p.os <- open.savanna[1]

# Calculate 95% confidence interval
n.os <- nrow(os.detected)
CI.os <- 1.96 * sqrt((p.os * (1 - p.os))/n.os)
CI.os.pos <- p.os + CI.os
CI.os.neg <- p.os - CI.os


#---------------------------------------------
# II. ACACIA THICKET
#---------------------------------------------

# Load acacia thicket data
os <- read.csv(".../Data/Cheetah vs impala experiment/predator-prey-model-revised-cheetah_vs_impala-acacia_thicket.csv", header=T, skip = 6)


# Because stats are for "successful" hunts and pursuits, we are only interested in the impala's probability of escaping if
# detected (as pursuits only occur when the cheetah detects the impala)
at.detected <- subset(at, prey.detected == 1)

# Calculate the impala's probability of escaping if detected
acacia.thicket <- ave(at.detected$prey.win, FUN = mean)
p.at <- acacia.thicket[1]
p.at

# Calculate 95% confidence interval
n.at <- nrow(at.detected)
CI.at <- 1.96 * sqrt((p.at * (1 - p.at))/n.at)
CI.at
CI.at.pos <- p.at + CI.at
CI.at.neg <- p.at - CI.at

#---------------------------------------------
# III. TEST DIFFERENCE
#---------------------------------------------

n.os.win <- nrow(subset(os.detected, prey.win == 1))
n.at.win <- nrow(subset(at.detected, prey.win == 1))

prop.test(x = c(n.os.win, n.at.win), n = c(n.os, n.at), correct = FALSE)
