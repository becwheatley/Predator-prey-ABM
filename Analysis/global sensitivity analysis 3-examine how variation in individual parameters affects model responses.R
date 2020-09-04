#----------------------------------------------------------------------------------------------------------------
# 'Habitat features and performance interact to determine the outcomes of terrestrial predator-prey pursuits'
# Wheatley R, Pavlic TP, Levy O, & Wilson RS
# Global sensitivity analysis
# Step 3:  Examining how variation in individual parameters affects model responses
# Code by Rebecca Wheatley
# Last Modified 15 July 2020
#----------------------------------------------------------------------------------------------------------------

# Load raw data
sum.data <- read.csv(".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results.csv", header = TRUE,
                     colClasses = c("NULL", NA, NA, NA, NA, NA, NA, NA, NA, NA,                    ## Columns 1-10,
                     colClasses = c(NA, NA, "NULL", NA, NA, NA, NA, NA, NA, NA,                    ## Columns 1-10
                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,                        ## Columns 11-20
                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,                        ## Columns 21-30
                                    NA, NA, NA, NA, NA, NA, NA)))                                  ## Columns 31-37

# Load required packages
library(tidyverse)

#-------------------------------------------------------------------------------------
# IV. LOOKING AT HOW VARIATION IN TOP 4 PARAMETERS AFFECTS RESPONSES
#-------------------------------------------------------------------------------------

# Subset to obtain data set containing only default parameter settings
def.data <- sum.data %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

#--------------------------------
# Chance of avoiding detection
#--------------------------------

# Default
100*(1 - mean(def.data$prey.detected)) ## 2.4%

# PO varies
PO.data <- sum.data %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  #filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

PO.0 <- PO.data %>% filter(obstacle.proportion == 0)
PO.20 <- PO.data %>% filter(obstacle.proportion == 0.2)
100*(1 - mean(PO.0$prey.detected)) ## 0.3%
100*(1 - mean(PO.20$prey.detected)) ## 35.6%

# Predator VD varies
predVD.data <- sum.data %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  #filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

predVD.5 <- predVD.data %>% filter(predator.vision.distance == 5)
predVD.30 <- predVD.data %>% filter(predator.vision.distance == 30)
100*(1 - mean(predVD.5$prey.detected)) ## 45.2%
100*(1 - mean(predVD.30$prey.detected)) ## 0%

# Predator LL varies
predLL.data <- sum.data %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  #filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

predLL.01 <- predLL.data %>% filter(predator.limb.length == 0.1)
predLL.10 <- predLL.data %>% filter(predator.limb.length == 1.0)
100*(1 - mean(predLL.01$prey.detected)) ## 2.9%
100*(1 - mean(predLL.10$prey.detected)) ## 6.7%

# OR varies
OR.data <- sum.data %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  #filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

OR.05 <- OR.data %>% filter(obstacle.radius == 0.5)
OR.2 <- OR.data %>% filter(obstacle.radius == 2.0)
100*(1 - mean(OR.05$prey.detected)) ## 7.6%
100*(1 - mean(OR.2$prey.detected)) ## 1.3%


#--------------------------------
# Chance of escaping if detected
#--------------------------------

data2 <- sum.data %>% filter(prey.detected == 1)
# Default
def.data2 <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

100 * mean(def.data2$prey.win) ## 17.2%

# KD varies
KD.data2 <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  #filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

KD.05.2 <- KD.data2 %>% filter(kill.distance == 0.5)
KD.50.2 <- KD.data2 %>% filter(kill.distance == 5.0)
100 * mean(KD.05.2$prey.win) ## 22.4%
100 * mean(KD.50.2$prey.win) ## 0.7%

# Predator's ED varies
predED.data2 <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  #filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

predED.10.2 <- predED.data2 %>% filter(predator.exhaustion.distance == 10)
predED.1010.2 <- predED.data2 %>% filter(predator.exhaustion.distance == 1010)
100 * mean(predED.10.2$prey.win) ## 66.3
100 * mean(predED.1010.2$prey.win) ## 15.2%


# Prey's ED varies
preyED.data2 <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  #filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

preyED.10.2 <- preyED.data2 %>% filter(prey.exhaustion.distance == 10)
preyED.1010.2 <- preyED.data2 %>% filter(prey.exhaustion.distance == 1010)
100 * mean(preyED.10.2$prey.win) ## 1.1%
100 * mean(preyED.1010.2$prey.win) ## 22.5%


# NR varies
NR.data2 <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  #filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

NR.0.2 <- NR.data2 %>% filter(number.of.refuges == 0)
NR.5.2 <- NR.data2 %>% filter(number.of.refuges == 5)
100 * mean(NR.0.2$prey.win) ## 2.3%
100 * mean(NR.5.2$prey.win) ## 42.1%

#--------------------------------
# Overall survival
#--------------------------------

# Default
100*(mean(def.data$prey.win)) ## 19.2%

# PO varies
100*mean(PO.0$prey.win) ## 17.1%
100*mean(PO.20$prey.win) ## 65.8%

# Predator's ED varies
predED.data <- sum.data %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  #filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

predED.10 <- predED.data %>% filter(predator.exhaustion.distance == 10)
predED.1010 <- predED.data %>% filter(predator.exhaustion.distance == 1010)
100*mean(predED.10$prey.win) ## 66.999%
100*mean(predED.1010$prey.win) ## 18%

# Predator's VD varies
predVD.data <- sum.data %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  #filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

predVD.5 <- predVD.data %>% filter(predator.vision.distance == 5)
predVD.30 <- predVD.data %>% filter(predator.vision.distance == 30)
100*mean(predVD.5$prey.win) ## 57.6%
100*mean(predVD.30$prey.win) ## 14.8%

# KD varies
KD.data <- sum.data %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  #filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

KD.05 <- KD.data %>% filter(kill.distance == 0.5)
KD.5 <- KD.data %>% filter(kill.distance == 5.0)
100*mean(KD.05$prey.win) ## 24%
100*mean(KD.5$prey.win) ## 3.2%

#------------------------------------------
# Median time to detection (if detected)
#------------------------------------------

# Default
median(def.data2$detect.time) ## 144.5 s

# Predator's VD varies
predVD.data <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  #filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

predVD.5 <- predVD.data %>% filter(predator.vision.distance == 5)
predVD.15 <- predVD.data %>% filter(predator.vision.distance == 15)
predVD.30 <- predVD.data %>% filter(predator.vision.distance == 30)
median(predVD.5$detect.time) ## 342.025 s
median(predVD.30$detect.time) ## 44.35 s

# PO varies
PO.data2 <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  #filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

PO.0.2 <- PO.data2 %>% filter(obstacle.proportion == 0)
PO.20.2 <- PO.data2 %>% filter(obstacle.proportion == 0.2)
median(PO.0.2$detect.time) ## 106.175 s
median(PO.20.2$detect.time) ## 350.6 s

# Predator's LL varies
predLL.data2 <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  #filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

predLL.01.2 <- predLL.data2 %>% filter(predator.limb.length == 0.1)
predLL.10.2 <- predLL.data2 %>% filter(predator.limb.length == 1.0)
median(predLL.01.2$detect.time) ## 156.55 s
median(predLL.10.2$detect.time) ## 196.15 s

# OR varies
OR.data2 <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  #filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

OR.05.2 <- OR.data2 %>% filter(obstacle.radius == 0.5)
OR.20.2 <- OR.data2 %>% filter(obstacle.radius == 2.0)
median(OR.05.2$detect.time) ## 202.7 s
median(OR.20.2$detect.time) ## 126.7 s

#------------------------------------------
# Median pursuit time (if detected)
#------------------------------------------

# Default
median(def.data2$pursuit.time) ## 51.25 s

# KD varies
KD.data <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  #filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

KD.05 <- KD.data %>% filter(kill.distance == 0.5)
KD.5 <- KD.data %>% filter(kill.distance == 5.0)
median(KD.05$pursuit.time) ## 103.125 s
median(KD.5$pursuit.time) ## 0.0 s

# FID varies
FID.data <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  #filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

FID.5 <-  FID.data %>% filter(flight.initiation.distance == 5)
FID.30 <- FID.data %>% filter(flight.initiation.distance == 30)
median(FID.5$pursuit.time) ## 51.25 s
median(FID.30$pursuit.time) ## 99.35 s

# Prey's VA varies
preyVA.data <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  #filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

preyVA.30 <-  preyVA.data %>% filter(prey.vision.angle == 30)
preyVA.330 <- preyVA.data %>% filter(prey.vision.angle == 330)
median(preyVA.30$pursuit.time) ## 1.55 s
median(preyVA.330$pursuit.time) ## 63.85 s

# Prey's ED varies
preyED.data <- data2 %>%
  filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  #filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

preyED.10 <-   preyED.data %>% filter(prey.exhaustion.distance == 10)
preyED.1010 <- preyED.data %>% filter(prey.exhaustion.distance == 1010)
median(preyED.10$pursuit.time) ## 2.35 s
median(preyED.1010$pursuit.time) ## 55.05 s

#------------------------------------------
# Median escape path length (if detected)
#------------------------------------------

# Default
median(def.data2$prey.escape.length) ## 132.4648 m

# KD varies
median(KD.05$prey.escape.length) ## 264.2005 m
median(KD.5$prey.escape.length)  ## 0.0 m

# FID varies
median(FID.5$prey.escape.length)  ## 132.4648 m
median(FID.30$prey.escape.length) ## 257.4304 m

# Prey's VA varies
median(preyVA.30$prey.escape.length)  ## 2.154143 m
median(preyVA.330$prey.escape.length) ## 159.7482 m

# Prey's LL varies
preyLL.data <- data2 %>%
  #filter(prey.limb.length == 0.5) %>%
  filter(prey.vision.distance == 15) %>%
  filter(prey.vision.angle == 180) %>%
  filter(freeze.distance == 10) %>%
  filter(flight.initiation.distance == 5) %>%
  filter(prey.exhaustion.distance == 510) %>%
  filter(time.to.turn == 3) %>%
  filter(time.spent.circling == 1) %>%
  filter(predator.limb.length == 0.5) %>%
  filter(predator.vision.distance == 15) %>%
  filter(predator.vision.angle == 180) %>%
  filter(predator.exhaustion.distance == 510) %>%
  filter(kill.distance == 1) %>%
  filter(obstacle.proportion == 0.05) %>%
  filter(obstacle.radius == 1) %>%
  filter(prey.obstacle.sensitivity == 0.99) %>%
  filter(predator.obstacle.sensitivity == 0.99) %>%
  filter(number.of.refuges == 1) %>%
  filter(number.of.target.patches == 0)

preyLL.01 <- preyLL.data %>% filter(prey.limb.length == 0.1)
preyLL.10 <- preyLL.data %>% filter(prey.limb.length == 1.0)
median(preyLL.01$prey.escape.length) ## 145.241 m
median(preyLL.10$prey.escape.length) ## 47.5349 m

#------------------------------------------
# Median pursuit path length (if detected)
#------------------------------------------

# Default
median(def.data2$predator.pursuit.length) ## 15.92552 m

# KD varies
median(KD.05$predator.pursuit.length) ## 27.98662 m
median(KD.5$predator.pursuit.length)  ## 0.0 m

# Pred VD varies
median(predVD.5$predator.pursuit.length)  ## 3.292194 m
median(predVD.30$predator.pursuit.length) ## 30.65014 m

# FID varies
median(FID.5$predator.pursuit.length)  ## 15.92552 m
median(FID.30$predator.pursuit.length) ## 27.15583 m

# Prey's VA varies
median(preyVA.30$predator.pursuit.length)  ## 2.535923 m
median(preyVA.330$predator.pursuit.length) ## 23.73459 m
