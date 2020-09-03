#----------------------------------------------------------------------------------------------------------------
# 'Habitat features and performance interact to determine the outcomes of terrestrial predator-prey pursuits'
# Wheatley R, Pavlic TP, Levy O, & Wilson RS
# Global sensitivity analysis
# Step 1: calculate mean probability of escape and medians for the other response variables
# Code by Rebecca Wheatley
# Last Modified 30 June 2020
#-----------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# I. Load data sets
#-------------------------------------------------------------------------------------

# NOTE BEFOREHAND: Data files written through BehaviorSpace in NetLogo append a bunch of environment-related information to the
# start of each .csv file. This causes issues when trying to load the complete .csv file, as it messes up the number of columns. 
# To get around this, we skip the first 6 rows of data when loading each .csv file - this will only load the file from the 
# parameter headers onwards (this step is unecessary if data files have been written via R/RNetLogo).

# NOTE BEFOREHAND 2: MUST divide the files up by the leading parameter that varies. E.g. Do parameter sets 1-18 together, then 
# sets 19-35, then 36-51, etc.

# Create a function to import our data files
import.multiple.csv.files<-function(mypath,mypattern,...)
{
  tmp.list.1    <- list.files(mypath, pattern=mypattern)
  tmp.list.2  <- list(length=length(tmp.list.1))
  for (i in 1:length(tmp.list.1)){tmp.list.2[[i]]<-read.csv(tmp.list.1[i],...)}
  names(tmp.list.2)<-tmp.list.1
  tmp.list.2
}

#-------------------------------------------------------------------------------------
# I. Expand single parameter sets, and calculate mean and median response variables
#-------------------------------------------------------------------------------------

# Create an empty table with parameter name headings
heading.names <- list("prey.limb.length", 
                      "prey.vision.distance", 
                      "prey.vision.angle", 
                      "freeze.distance",
                      "flight.initiation.distance", 
                      "prey.exhaustion.distance", 
                      "time.to.turn", 
                      "time.spent.circling",
                      "predator.limb.length", 
                      "predator.vision.distance", 
                      "predator.vision.angle", 
                      "predator.exhaustion.distance", 
                      "kill.distance", 
                      "obstacle.proportion", 
                      "obstacle.radius", 
                      "prey.obstacle.sensitivity", 
                      "predator.obstacle.sensitivity", 
                      "number.of.refuges", 
                      "number.of.target.patches",
                      "prob.escape",
                      "prob.avoid.detection",
                      "mean.sim.time", 
                      "median.sim.time", 
                      "mean.prey.curviness", 
                      "median.prey.curviness", 
                      "mean.predator.curviness", 
                      "median.predator.curviness",
                      "prob.escape.if.detected",
                      "mean.detect.time", 
                      "median.detect.time", 
                      "mean.pursuit.time", 
                      "median.pursuit.time",
                      "mean.prey.escape.length", 
                      "median.prey.escape.length", 
                      "mean.predator.pursuit.length", 
                      "median.predator.pursuit.length")
write.table(t(heading.names), ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = FALSE)

#---------------------------
# Parameter sets 1-18
#---------------------------
# Load our parameter sets
setwd(".../Data/Sensitivity analysis/001-018")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/001-018",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the chance of the prey escaping (overall)
  j = 1
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length (IF detected)
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length (IF detected)
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape.if.detected",
                       "mean.detect.time", 
                       "median.detect.time", 
                       "mean.pursuit.time", 
                       "median.pursuit.time", 
                       "mean.prey.escape.length", 
                       "median.prey.escape.length",
                       "mean.predator.pursuit.length", 
                       "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}

#---------------------------
# Parameter sets 19-35
#---------------------------
setwd(".../Data/Sensitivity analysis/019-035")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/019-035",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 2
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}


#---------------------------
# Parameter sets 36-51
#---------------------------
setwd(".../Data/Sensitivity analysis/036-051")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/036-051",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 3
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}


#---------------------------
# Parameter sets 52-66
#---------------------------
setwd(".../Data/Sensitivity analysis/052-066")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/052-066",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 4
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}

#---------------------------
# Parameter sets 67-80
#---------------------------
setwd(".../Data/Sensitivity analysis/067-080")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/067-080",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 5
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}



#---------------------------
# Parameter sets 81-93
#---------------------------
setwd(".../Data/Sensitivity analysis/081-093")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/081-093",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 6
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}

#---------------------------
# Parameter sets 94-105
#---------------------------
setwd(".../Data/Sensitivity analysis/094-105")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/094-105",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 7
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}


#---------------------------
# Parameter sets 106-116
#---------------------------
setwd(".../Data/Sensitivity analysis/106-116")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/106-116",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 8
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}

#---------------------------
# Parameter sets 117-126
#---------------------------
setwd(".../Data/Sensitivity analysis/117-126")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/117-126",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 9
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}

#---------------------------
# Parameter sets 127-135
#---------------------------
setwd(".../Data/Sensitivity analysis/127-135")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/127-135",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 10
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}


#---------------------------
# Parameter sets 136-143
#---------------------------
setwd(".../Data/Sensitivity analysis/136-143")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/136-143",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 11
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}


#---------------------------
# Parameter sets 144-150
#---------------------------
setwd(".../Data/Sensitivity analysis/144-150")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/144-150",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 12
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}



#---------------------------
# Parameter sets 151-156
#---------------------------
setwd(".../Data/Sensitivity analysis/151-156")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/151-156",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 13
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}


#---------------------------
# Parameter sets 157-161
#---------------------------
setwd(".../Data/Sensitivity analysis/157-161")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/157-161",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 14
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}


#---------------------------
# Parameter sets 162-165
#---------------------------
setwd(".../Data/Sensitivity analysis/162-165")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/162-165",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 15
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}


#---------------------------
# Parameter sets 166-168
#---------------------------
setwd(".../Data/Sensitivity analysis/166-168")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/166-168",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 16
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}

#---------------------------
# Parameter sets 169-170
#---------------------------
setwd(".../Data/Sensitivity analysis/169-170")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/169-170",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 17
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}

#---------------------------
# Parameter sets 171
#---------------------------
setwd(".../Data/Sensitivity analysis/171")
csv.import00 <- import.multiple.csv.files(".../Data/Sensitivity analysis/171",".csv$",sep=",", skip = 6, 
                                          colClasses = c('NULL', NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                         'NULL', 'NULL', 'NULL', NA, NA, NA, NA, NA, NA, NA,
                                                         NA, NA, 'NULL', 'NULL', NA))

# Calculate the mean and median of our various response variables, and append these to our output file
for (i in 1:length(csv.import00)){
  # Expand out our parameter set
  set <- do.call("rbind", csv.import00[i])
  
  # Calculate and append the mean prey.win and prey.detect
  j = 18
  means <- ave(set$prey.win, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  set$prob.escape       <- means
  
  # Calculate and append the chance of the prey avoiding detection
  means <- ave(set$prey.detected, list(set[[j]],
                                       set[[j + i]]), FUN = mean)
  set$prob.never.detected  <- 1 - means
  
  # Calculate and append the chance of the prey escaping IF detected
  set2 <- subset(set, prey.detected == 1)
  means <- ave(set2$prey.win, list(set2[[j]],
                                   set2[[j + i]]), FUN = mean)
  set2$prob.escape.if.detected  <- means
  
  # Calculate the mean and median detect.time (IF detected)
  means <- ave(set2$detect.time, list(set2[[j]],
                                      set2[[j + i]]), FUN = mean)
  medians <- ave(set2$detect.time, list(set2[[j]],
                                        set2[[j + i]]), FUN = median)
  set2$mean.detect.time       <- means
  set2$median.detect.time     <- medians
  
  # Calculate the mean and median pursuit.time (IF detected)
  means <- ave(set2$pursuit.time, list(set2[[j]],
                                       set2[[j + i]]), FUN = mean)
  medians <- ave(set2$pursuit.time, list(set2[[j]],
                                         set2[[j + i]]), FUN = median)
  set2$mean.pursuit.time       <- means
  set2$median.pursuit.time     <- medians
  
  # Calculate the mean and median sim.time
  means <- ave(set$sim.time, list(set[[j]],
                                  set[[j + i]]), FUN = mean)
  medians <- ave(set$sim.time, list(set[[j]],
                                    set[[j + i]]), FUN = median)
  set$mean.sim.time       <- means
  set$median.sim.time     <- medians
  
  # Calculate the mean and median prey.escape.length
  means <- ave(set2$prey.escape.length, list(set2[[j]],
                                             set2[[j + i]]), FUN = mean)
  medians <- ave(set2$prey.escape.length, list(set2[[j]],
                                               set2[[j + i]]), FUN = median)
  set2$mean.prey.escape.length       <- means
  set2$median.prey.escape.length     <- medians
  
  # Calculate the mean and median predator.pursuit.length
  means <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                  set2[[j + i]]), FUN = mean)
  medians <- ave(set2$predator.pursuit.length, list(set2[[j]],
                                                    set2[[j + i]]), FUN = median)
  set2$mean.predator.pursuit.length       <- means
  set2$median.predator.pursuit.length     <- medians
  
  # Calculate the mean and median prey.curviness
  means <- ave(set$prey.curviness, list(set[[j]],
                                        set[[j + i]]), FUN = mean)
  medians <- ave(set$prey.curviness, list(set[[j]],
                                          set[[j + i]]), FUN = median)
  set$mean.prey.curviness       <- means
  set$median.prey.curviness     <- medians
  
  # Calculate the mean and median predator.curviness
  means <- ave(set$predator.curviness, list(set[[j]],
                                            set[[j + i]]), FUN = mean)
  medians <- ave(set$predator.curviness, list(set[[j]],
                                              set[[j + i]]), FUN = median)
  set$mean.predator.curviness       <- means
  set$median.predator.curviness     <- medians
  
  # Create a new, reduced data set with our summary data
  set3 <- unique(set[c("prey.limb.length", 
                       "prey.vision.distance", 
                       "prey.vision.angle", 
                       "freeze.distance", 
                       "flight.initiation.distance", 
                       "prey.exhaustion.distance", 
                       "time.to.turn", 
                       "time.spent.circling",
                       "predator.limb.length",
                       "predator.vision.distance", 
                       "predator.vision.angle", 
                       "predator.exhaustion.distance", 
                       "kill.distance", 
                       "obstacle.proportion", 
                       "obstacle.radius", 
                       "prey.obstacle.sensitivity", 
                       "predator.obstacle.sensitivity", 
                       "number.of.refuges", 
                       "number.of.target.patches",
                       "prob.escape", 
                       "prob.never.detected",
                       "mean.sim.time", 
                       "median.sim.time", 
                       "mean.prey.curviness", 
                       "median.prey.curviness", 
                       "mean.predator.curviness", 
                       "median.predator.curviness")])
  
  # Create a new, reduced data set with our summary data
  set4 <- unique(set2[c("prey.limb.length", 
                        "prey.vision.distance", 
                        "prey.vision.angle", 
                        "freeze.distance", 
                        "flight.initiation.distance", 
                        "prey.exhaustion.distance", 
                        "time.to.turn", 
                        "time.spent.circling",
                        "predator.limb.length",
                        "predator.vision.distance", 
                        "predator.vision.angle", 
                        "predator.exhaustion.distance", 
                        "kill.distance", 
                        "obstacle.proportion", 
                        "obstacle.radius", 
                        "prey.obstacle.sensitivity", 
                        "predator.obstacle.sensitivity", 
                        "number.of.refuges", 
                        "number.of.target.patches",
                        "prob.escape.if.detected",
                        "mean.detect.time", 
                        "median.detect.time", 
                        "mean.pursuit.time", 
                        "median.pursuit.time", 
                        "mean.prey.escape.length", 
                        "median.prey.escape.length",
                        "mean.predator.pursuit.length", 
                        "median.predator.pursuit.length")])
  
  set3$prob.escape.if.detected <- set4$prob.escape.if.detected
  set3$mean.detect.time <- set4$mean.detect.time
  set3$median.detect.time <- set4$median.detect.time
  set3$mean.pursuit.time <- set4$mean.pursuit.time
  set3$median.pursuit.time <- set4$median.pursuit.time
  set3$mean.prey.escape.length <- set4$mean.prey.escape.length
  set3$median.prey.escape.length <- set4$median.prey.escape.length
  set3$mean.predator.pursuit.length <- set4$mean.predator.pursuit.length
  set3$median.predator.pursuit.length <- set4$median.predator.pursuit.length
  
  # Remove the headings so we don't keep writing them to file
  names(set3) <- NULL
  
  # Write this to file
  write.table(set3, ".../Data/Sensitivity analysis/predator-prey-model-revised-global_sensitivity_analysis-parameter_sets_and_results-summarised.csv", 
              sep = ",", append = TRUE)
}