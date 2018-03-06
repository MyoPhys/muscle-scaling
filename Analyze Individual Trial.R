## This code will analyze all of the trials in a folder and combine the output into a single file
# Set the working directory to the file for the individual of interest
# Make sure the chance the name or the output file below

# Type in Muscle Mass (kg)
mass <- 0.000018

# Read in all the files in the folder
filenames <- list.files()

# Create empty variables that the loop can fill
allmass <- numeric()
allwork <- numeric(0)
allpeakvelocity <- numeric()
allload <- numeric()
allmswork <- numeric()
allworkvelocity <- integer()
allaveragevelocity <- numeric()
allaveragepower <- numeric()
allpeakpower <- numeric()
Individual <- character()
Temperature <- character()
Treatment <- character()
TrialNumber <- character()

# Start the loop
for (i in 1:length(filenames)){

# Read in the file
filename <- filenames[i]
filename <- basename(filename)
data <- (read.table(filename, skip = 22, sep = "\t"))
Individual[i] <- substring(filename, 1,5)
Temperature[i] <- substring(filename, 6,9)
Treatment[i] <- substring(filename, 10, 12)
TrialNumber[i] <- substring(filename, 13, 30)

# Name the variables from the data
time <- data[,1]
length <- data[,2]
force <- data[,4]
force <- 1.0192*force+0.026 # convert from volts to Newtons
# Calculates distance shortened from length
shortening <- length[1]-length 

# Calculates velocity then uses 1% of peak shortening velocity as the place where work is measured, if statement exclude P0 trials
if(Treatment[i] == "_P0" | Treatment[i] == "2_P" | Treatment[i] == "P0_" | Treatment[i] == "P02"| Treatment[i] == "_P0" ){
  velocity2 <- numeric(length = 1200)
}
else{
library(pspline)
velocity1 <- smooth.Pspline(time,length, norder=4,method=3)
velocity2=signif(predict(velocity1, time, nderiv=1), digits=3)

# Plots out the trial and allows you to select the point at which velocity is 0
plot(time,velocity2)
workvelocity <- identify(time,velocity2,n=1)

# If a point is clicked beyond 1200, the time of 0 velocity will be 1200 ms
if(workvelocity>1200){
  workvelocity <- 1200
}

# Find the peak and average shortening velocity during stiumation
peakvelocity <- min(velocity2[20:1200])
averagevelocity <- mean(velocity2[20:1200])

# Find peak and average power
power = velocity2*force
peakpower = min(power[20:workvelocity])
averagepower = mean(power[20:workvelocity])

#add the variables to the table
allpeakpower[i] <- peakpower
allaveragepower[i] <- averagepower
allaveragevelocity[i] <- averagevelocity
allpeakvelocity[i] <- peakvelocity
allworkvelocity[i] <- workvelocity
}

# Identify the start of contraction
start <- 20

# Determine 95% shortening
endshort <- which(shortening > (max(shortening)*0.95))[1]

# Trim the data accordingly
trimmed_data <- shortening

# Calculate distance (mm)

distance <- shortening[workvelocity]

# Define Load (N)
load <- max(force)
allload[i] <- load

# in N mm
work <- distance*load
# in N m (J)
work <- work/1000
allwork[i] <- work

# Mass-specific work (J/kg)
MSwork <- work/mass
allmswork[i] <- MSwork
allmass[i] <- mass

}

outputfile <- paste(Individual[2],"Output.xls") # The file to which it will append the data.
alldata <- data.frame(Individual, Temperature, Treatment, TrialNumber, allwork, allmswork, allload, allpeakvelocity, allworkvelocity, allaveragevelocity, allpeakpower, allaveragepower, allmass)
write.table(alldata,outputfile,append=FALSE,sep = "\t",row.names=FALSE,col.names=TRUE) # Uncomment to automatically write the output file each time a new curve is generated.
