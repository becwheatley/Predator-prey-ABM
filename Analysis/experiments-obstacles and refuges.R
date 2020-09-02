#----------------------------------------------------------------------------------------------------------------
# 'Habitat features and performance interact to determine the outcomes of terrestrial predator-prey pursuits'
# Wheatley R, Pavlic TP, Levy O, & Wilson RS
# Effect of relative performance, obstacles, and refuges on detection and escape
# Code by Rebecca Wheatley
# Last Modified 10 June 2020
#----------------------------------------------------------------------------------------------------------------

# Load required libraries
library(reshape2)
library(plot3D)

#--------------------------------------------------
# I. PREDATOR LIMB LENGTH VS OBSTACLE PROPORTION
#--------------------------------------------------

# Load data
data <- read.csv(".../Data/Obstacle and refuge experiments/predator-prey-model-revised-performance_vs_obstacles.csv", header=T, skip = 6)

#-----------------------------
# Chance of avoiding detection
#-----------------------------

# Double check that undetected prey always "win"
undetected <- subset(data, prey.detected == 0)
min(undetected$prey.win) ## yes, undetected prey always have prey.win == 1

# Calculate prey's probability of avoiding detection
means1 <- ave(data$prey.detected, list(data$predator.limb.length, 
                                       data$obstacle.proportion), FUN = mean)
data$prob.never.detected <- 1 - means1

# Write to new data file
data.2 <- unique(data[c("predator.limb.length", "obstacle.proportion", "prob.never.detected")])
write.csv(data.2, ".../Data/Obstacle and refuge experiments/predator-prey-model-revised-performance_vs_obstacles-NEVERDETECTED.csv")

# Plot probability of avoiding detection against predator limb length and proportion obstacles (Fig. 3A)
z <- acast(data.2, obstacle.proportion~predator.limb.length, value.var="prob.never.detected")
x <- unique(data.2[,"obstacle.proportion"])
y <- unique(data.2[,"predator.limb.length"])
x <- x[order(x)]
y <- y[order(y)]
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)

persp3D(x, y, z, theta=-50, phi=25, expand=0.75,
        resfac = 5,
        zlim = c(0, 0.4),
        ticktype="detailed", 
        ylab="Predator limb length (m)", 
        xlab="Proportion obstacles", 
        zlab="Probability of avoiding detection",
        axes=TRUE) 

#-------------------------------
# If detected, chance of escape
#-------------------------------

# Subset data to only examine runs where prey is detected
data2 <- subset(data, prey.detected == 1)

# Calculate prey's probability of escaping if detected
means1 <- ave(data2$prey.win, list(data2$predator.limb.length, 
                                   data2$obstacle.proportion), FUN = mean)
data2$prob.escape.if.detected <- means1

# Calculate median time to detection (if detected)
medians1 <- ave(data2$detect.time, list(data2$predator.limb.length, 
                                        data2$obstacle.proportion), FUN = median)
data2$median.detect.time <- medians1

# Write to new data file
data.2 <- unique(data2[c("predator.limb.length", "obstacle.proportion", "prob.escape.if.detected", "median.detect.time")])
write.csv(data.2, ".../Data/Obstacle and refuge experiments/predator-prey-model-revised-performance_vs_obstacles-IFDETECTED.csv")

# Plot probability of escaping if detected against predator limb length and proportion obstacles (Fig. 4A)
z <- acast(data.2, obstacle.proportion~predator.limb.length, value.var="prob.escape.if.detected")
x <- unique(data.2[,"obstacle.proportion"])
y <- unique(data.2[,"predator.limb.length"])
x <- x[order(x)]
y <- y[order(y)]
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)

persp3D(x, y, z, theta=-50, phi=25, expand=0.75,
        resfac = 5,
        zlim = c(0, 0.6),
        ticktype="detailed", 
        ylab="Predator limb length (m)", 
        xlab="Proportion obstacles", 
        zlab="Probability of escape if detected",
        axes=TRUE) 

# Plot median detection time against predator limb length and proportion obstacles (Fig. 3C)
z <- acast(data.2, obstacle.proportion~predator.limb.length, value.var="median.detect.time")
x <- unique(data.2[,"obstacle.proportion"])
y <- unique(data.2[,"predator.limb.length"])
x <- x[order(x)]
y <- y[order(y)]
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)

persp3D(x, y, z, theta=-50, phi=25, expand=0.75,
        resfac = 5,
        zlim = c(0, 400),
        ticktype="detailed", 
        ylab="Predator limb length (m)", 
        xlab="Proportion obstacles", 
        zlab="Median time to detection",
        axes=TRUE)



#-----------------------------
# Overall chance of survival
#-----------------------------

# Calculate the prey's overall probability of survival
means1 <- ave(data$prey.win, list(data$predator.limb.length, 
                                  data$obstacle.proportion), FUN = mean)
data$prob.escape <- means1

# Write to new data file
data.2 <- unique(data[c("predator.limb.length", "obstacle.proportion", "prob.escape")])
write.csv(data.2, ".../Data/Obstacle and refuge experiments/predator-prey-model-revised-performance_vs_obstacles-OVERALLSURVIVAL.csv")

# Plot overall probability escape against predator limb length and proportion obstacles (Fig. 4C)
z <- acast(data.2, obstacle.proportion~predator.limb.length, value.var="prob.escape")
x <- unique(data.2[,"obstacle.proportion"])
y <- unique(data.2[,"predator.limb.length"])
x <- x[order(x)]
y <- y[order(y)]
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)

persp3D(x, y, z, theta=-50, phi=25, expand=0.75,
        resfac = 5,
        zlim = c(0, 0.8),
        ticktype="detailed", 
        ylab="Predator limb length (m)", 
        xlab="Proportion obstacles", 
        zlab="Overall probability of escape",
        axes=TRUE)

#---------------------------------------------
# II. PREDATOR LIMB LENGTH VS NUMBER OF REFUGES
#---------------------------------------------

# Load data
data <- read.csv(".../Data/Obstacle and refuge experiments/predator-prey-model-revised-performance_vs_refuges.csv", header = T, skip = 6)

#-----------------------------
# Chance of avoiding detection
#-----------------------------

# Double check that undetected prey always "win"
undetected <- subset(data, prey.detected == 0)
min(undetected$prey.win) ## yes

# Calculate the prey's probability of avoiding detection
means1 <- ave(data$prey.detected, list(data$predator.limb.length, 
                                       data$number.of.refuges), FUN = mean)
data$prob.never.detected <- 1 - means1

# Write to new data file
data.2 <- unique(data[c("predator.limb.length", "number.of.refuges", "prob.never.detected")])
write.csv(data.2, ".../Data/Obstacle and refuge experiments/predator-prey-model-revised-performance_vs_refuges-NEVERDETECTED.csv")

# Plot probability of avoiding detection against predator limb length and number of refuges (Fig. 3B)
z <- acast(data.2, number.of.refuges~predator.limb.length, value.var="prob.never.detected")
x <- unique(data.2[,"number.of.refuges"])
y <- unique(data.2[,"predator.limb.length"])
x <- x[order(x)]
y <- y[order(y)]
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)

persp3D(x, y, z, theta=-50, phi=25, expand=0.75,
        resfac = 5,
        zlim = c(0, 0.05),
        ticktype="detailed", 
        ylab="Predator limb length (m)", 
        xlab="Number of refuges", 
        zlab="Probability of avoiding detection",
        axes=TRUE) 

#--------------------------------
# If detected, chance of escape
#--------------------------------

# Subset data to only examine runs where prey is detected
data2 <- subset(data, prey.detected == 1)

# Calculate prey's probability of escaping if detected
means1 <- ave(data2$prey.win, list(data2$predator.limb.length, 
                                   data2$number.of.refuges), FUN = mean)
data2$prob.escape.if.detected <- means1

# Calculate median time to detection (if detected)
medians1 <- ave(data2$detect.time, list(data2$predator.limb.length, 
                                        data2$number.of.refuges), FUN = median)
data2$median.detect.time <- medians1

# Write to new data file
data.2 <- unique(data2[c("predator.limb.length", "number.of.refuges", "prob.escape.if.detected", "median.detect.time")])
write.csv(data.2, ".../Data/Obstacle and refuge experiments/predator-prey-model-revised-performance_vs_refuges-IFDETECTED.csv")

# Plot probability of escaping if detected against predator limb length and number of refuges (Fig. 4B)
z <- acast(data.2, number.of.refuges~predator.limb.length, value.var="prob.escape.if.detected")
x <- unique(data.2[,"number.of.refuges"])
y <- unique(data.2[,"predator.limb.length"])
x <- x[order(x)]
y <- y[order(y)]
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)

persp3D(x, y, z, theta=-50, phi=25, expand=0.75,
        resfac = 5,
        zlim = c(0, 0.6),
        ticktype="detailed", 
        ylab="Predator limb length (m)", 
        xlab="Number of refuges", 
        zlab="Probability of escape if detected",
        axes=TRUE) 

# Plot median detection time against predator limb length and number of refuges (Fig. 3D)
z <- acast(data.2, number.of.refuges~predator.limb.length, value.var="median.detect.time")
x <- unique(data.2[,"number.of.refuges"])
y <- unique(data.2[,"predator.limb.length"])
x <- x[order(x)]
y <- y[order(y)]
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)

persp3D(x, y, z, theta=-50, phi=25, expand=0.75,
        resfac = 5,
        zlim = c(0, 150),
        ticktype="detailed", 
        ylab="Predator limb length (m)", 
        xlab="Number of refuges", 
        zlab="Median time to detection",
        axes=TRUE) 

#-----------------------------
# Overall chance of survival
#-----------------------------

# Calculate prey's overall probability of survival
means1 <- ave(data$prey.win, list(data$predator.limb.length, 
                                  data$number.of.refuges), FUN = mean)
data$prob.escape <- means1

# Write to new data file
data.2 <- unique(data[c("predator.limb.length", "number.of.refuges", "prob.escape")])
write.csv(data.2, ".../Data/Obstacle and refuge experiments/predator-prey-model-revised-performance_vs_refuges-OVERALLSURVIVAL.csv")

# Plot overall probability escape against predator limb length and number of refuges (Fig. 4D)
z <- acast(data.2, number.of.refuges~predator.limb.length, value.var="prob.escape")
x <- unique(data.2[,"number.of.refuges"])
y <- unique(data.2[,"predator.limb.length"])
x <- x[order(x)]
y <- y[order(y)]
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z, nbcol)

persp3D(x, y, z, theta=-50, phi=25, expand=0.75,
        resfac = 5,
        zlim = c(0, 0.6),
        ticktype="detailed", 
        ylab="Predator limb length (m)", 
        xlab="Number of refuges", 
        zlab="Overall probability of escape",
        axes=TRUE)