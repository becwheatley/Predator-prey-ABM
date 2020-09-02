#----------------------------------------------------------------------------------------------------------------
# 'Habitat features and performance interact to determine the outcomes of terrestrial predator-prey pursuits'
# Wheatley R, Pavlic TP, Levy O, & Wilson RS
# Global sensitivity analysis
# Step 2: Analyse summarised data
# Code by Rebecca Wheatley
# Last Modified 30 June 2020
#-----------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# I. Prerequirements
#-------------------------------------------------------------------------------------

# Load data
sum.data <- read.csv(".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", header = TRUE,
                     colClasses = c("NULL", NA, NA, NA, NA, NA, NA, NA, NA, NA,                    ## Columns 1-10
                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,                        ## Columns 11-20
                                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                    NA, NA, NA, NA, NA, NA, NA))

# Load packages
library(multisensi)

#-------------------------------------------------------------------------------------
# II. Make sure our model responses fit the assumptions of anova/manova
#-------------------------------------------------------------------------------------

# Probability of overall survival
m1 <- lm(asin(sqrt(prob.escape)) ~ prey.limb.length, data = sum.data)
hist(resid(m1), main = "Histogram of ASIN SQRT prob.escape residuals")
plot(m1)
sum.data$trans.prob.escape <- asin(sqrt(sum.data$prob.escape))

# Probability of avoiding detection
m1 <- lm(asin(sqrt(prob.avoid.detection)) ~ prey.limb.length, data = sum.data)
hist(resid(m1), main = "Histogram of ASIN SQRT prob.avoid.detection residuals")
plot(m1)
sum.data$trans.prob.avoid.detection <- asin(sqrt(sum.data$prob.avoid.detection))

# Probability of escaping if detected
m1 <- lm(asin(sqrt(prob.escape.if.detected)) ~ prey.limb.length, data = sum.data)
hist(resid(m1), main = "Histogram of ASIN SQRT prob.escape.if.detected residuals")
plot(m1)
sum.data$trans.prob.escape.if.detected <- asin(sqrt(sum.data$prob.escape.if.detected))

# Median time to detection
m2 <- lm(sqrt(median.detect.time) ~ prey.limb.length, data = sum.data)
hist(resid(m2), main = "Histogram of SQRT median.detect.time residuals")
plot(m2)
sum.data$trans.median.detect.time <- sqrt(sum.data$median.detect.time)

# Median pursuit time
m3 <- lm(sqrt(median.pursuit.time) ~ prey.limb.length, data = sum.data)
hist(resid(m3), main = "Histogram of median.pursuit.time residuals")
plot(m3)
sum.data$trans.median.pursuit.time <- sqrt(sum.data$median.pursuit.time)

# Median prey escape length
m4 <- lm(sqrt(median.prey.escape.length) ~ prey.limb.length, data = sum.data)
hist(resid(m4), main = "Histogram of median.prey.escape.length residuals")
plot(m4)
sum.data$trans.median.prey.escape.length <- sqrt(sum.data$median.prey.escape.length)

# Median predator pursuit length
m5 <- lm(sqrt(median.predator.pursuit.length) ~ prey.limb.length, data = sum.data)
hist(resid(m5), main = "Histogram of SQUARE ROOT median.predator.pursuit.length residuals")
plot(m5)
sum.data$trans.median.predator.pursuit.length <- sqrt(sum.data$median.predator.pursuit.length)

#-------------------------------------------------------------------------------------
# III. Calculate sensitivity indices
#-------------------------------------------------------------------------------------

# Extract our input parameters and output parameters from the data file
input <- sum.data[, 1:19]
output <- sum.data[, 37:43]

# Fit a multisensi model using our model inputs and outputs
## This model analyses our output using aov, including both the main effects and the two-way interactive effects
sum.data.seq <- multisensi(design = input, model = output, 
                           reduction = NULL, center = FALSE)
# View the main and total sensitivity indices (note that GSI is a generalized sensitivity index)
print(sum.data.seq, digits= 3)