# This code fits curves to max velocity, max power, and max load from all the trials of an individual

# Load in the individual data file
filename <- file.choose()
data <- (read.table(filename, sep = "\t", skip=1))

#Write in the values for P0 and L0
P0 <- 10.57654233 # [N]
L0 <- 0.032 # [m]

output <-data.frame(NULL)

# Extract the individual name
Individual <- data[2,1]

# Extract the loads [N]
Load <- data$V7

# Calculate each load relative to P0
RelLoad <- Load/P0

# Extract the peak shortening velocity [m/s]; invert because shortening is negative velocity
Velocity <- data$V8*-1

# Create a data frame containing the variables of interest
plotdata <- data.frame(Load=Load,RelLoad=RelLoad,Velocity=Velocity)

#Bind the P0 to the data so that it is used in curve fitting
newdata <- data.frame(P0,1,0)
colnames(newdata) <- colnames(plotdata)
subdata <- rbind(newdata,plotdata)
colnames(subdata) <- list("load","relload","velocity")

# definte the variables for curve fitting
vel<-subdata$velocity
load <- subdata$load

# fit the polynomial equation to the data
polyfit = lm(vel ~ poly(load,2))

# create a series of loads to creat a predicted curve
  testload <- seq(from=0,to=max(load),by=max(load)/100)
  testdata <- data.frame(c(load = testload))
  
# create a predicted velocity curve
  polytestvel <- predict(polyfit, data.frame(load=testload))

# Calculate Vmax as the predicted velocity at load = 0
vmax <- predict(polyfit, data.frame(load=c(0))) #[m/s]
L0vmax <- vmax/L0 #[L0/s]

# these variables describe the shape of the force-velocity curve, but the actual peak power used is calculated below
polytestpower <- polytestvel*testload
velpower <- max(polytestpower)

# find the velocity at which power is max
relvelpower <- polytestvel[which(polytestpower==velpower)]/polyvmax #[% vmax]

# calculate the power ratio
powerratio <- velpower/(P0*vmax)

# Find the force at which power is max
forcepower <- testload[which(polytestpower==velpower)]/P0 #[% P0]
  
# redefine loads because fits for work and power do not include P0
workload <- data$V7

# extract work [J]
workwork <- data$V5

# Fit polynomial equation to work, create a series of loads, and predict a work curve
workmodel <- lm(workwork~poly(workload,3))
testworkload <- seq(from=0, to=max(workload), by=max(workload)/100)
predictwork <- predict(workmodel,data.frame(workload=testworkload))
peakwork <- max(predictwork) #[J]
musclemass <- data$V13[1] # Extract muscle mass [in kg]
peakmswork <- peakwork/musclemass #[J/kg]

# extract power [W] (invert because shortening power is negative)
powerpower <- -data$V11

# Fit polynomial equation to power and predict a power curve
powermodel <- lm(powerpower~poly(workload,3))
peakpower <- predict(powermodel,data.frame(workload=testworkload)) #[W]
peakmspower <- peakpower/musclemass #[W/kg]

#Write this output to the data table
output <- data.frame(Individual,musclemass,L0,P0,vmax,L0vmax,peakpower,peakmspower,relvelpower,forcepower,powerratio,peakwork,peakmswork)

