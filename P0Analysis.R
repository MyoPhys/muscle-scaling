
################

getInfo = function(rowName){
  indiv=unlist(strsplit(as.character(rowName), split="-", fixed=TRUE))
  individ<-indiv[1]
  muscle<-indiv[2]
  temp<-indiv[3]
  trial<-indiv[7]
  return(c(individ,temp,trial,muscle))} # Function grabs all rows and splits the data into a list wherever there is a _ character. Then it returns the first item, indiv[1].
#######################

# Read in the file
filename <- file.choose()
filename <- basename(filename)
#filename<-"Ee08-20C-P0-trial13"
data <- (read.table(filename, skip = 22, sep = "\t")) # if the file was saved on the PC skip=22 if saved on a mac, skip=21


# Name the variables from the data

data<-data[,-c(3,5)]
colnames(data)<-c("time", "length", "force", "stim")


# Convert force from Volts to Newtons
data$force <- 1.0192*data$force+0.026


############# Determine P0 and time of P0 #####################
P0 <- max(data$force)

# Determine time of P0
P0time <- which(data$force == max(data$force))[1]

###############################################################


P99<-.99*P0

P90<-.90*P0 

P50<-.50*P0

P99time<-which(data$force>=P99)[1] 
P99force<-data$force[P99time]

P90time<-which(data$force>=P90)[1] # Determine time of P90
P90force<-data$force[P90time] # check the force at time of 90% of P0

P50time<-which(data$force>=P50)[1] 
P50force<-data$force[P50time]

###############################################################



################# Determine onset of stimulation ##############

plot(data$stim, xlim=c(0,500))
stimpeaks<-which(data$stim > 4)
stimstart<-stimpeaks[1]
###############################################################


########## Estimates the onset of force development based on consistent increases in force for 6ms ###########

newforce<-diff(data$force) # determines whether the force is increasing or decreasing at each time step (it is time value 2- value 1)

forcediffs<-rle(newforce>0) #  computes the lengths and values of sequences with numbers greater than or equal to 0 in a vector

positive.changes<-which(forcediffs$values==TRUE & forcediffs$lengths>5) # looks for runs of positive (or 0) numbers of 6 or more  

any(positive.changes) # checks if there are actually runs of 6 or more

positive.runs<-cumsum(forcediffs$lengths) #

ends<-positive.runs[positive.changes] # gets the ends of each positive run

startindex<-ifelse(positive.changes>1, positive.changes-1,0)

starts<-positive.runs[startindex]+1  # gets the starting point for each of the runs

if (0 %in% startindex) starts = c(1,starts)

###############################################################

print(starts)
print(data$force[starts[1]:ends[1]])
data$force[starts[1]]
force.onset.time<-starts[1] # pulls out the starting point of the first sequence of increasing force

par(mfrow=c(2,1))
plot(data$time, data$force, xlim=c(0,1000))
abline(v=starts[1], col="red", lwd=3)

plot(data$time, data$force, xlim=c(0,600))
abline(v=starts[1], col="red", lwd=3)
abline(v=P99time, col="darkblue", lwd=3)
abline(v=P90time, col="blue", lwd=3)
#abline(v=P50time, col="darkblue", lwd=3)
abline(v=stimstart, col="yellow", lwd=3)
filename

library(pspline)
rateofforce.spline <- smooth.Pspline(data$time,data$force,norder=5,method=3) # method=3 is for GCV
rateofforce <- predict(rateofforce.spline,data$time,nderiv=1)

notes<-"peaks at end of stim"

##########################################################



######## use the folowing code if finding the force onset using 6 increasing force measurements does not work well ##########

quartz()
plot(data$time, data$force, xlim=c(0,200))
force.onset.time<-identify(data$time, data$force, n=1)
#########################################################


###############Finds the 1st derivative of force to get peak rates#######

rateofforce99 <- rateofforce[force.onset.time:P99time]
peak.rate.99 <- mean(tail(sort(rateofforce99),3))/0.001
average.rate.99 <- mean(rateofforce99)/0.001

#########################################################################

##############Find the time of stimulation offset#########################

stimend <- stimpeaks[length(stimpeaks)]

##########################################################################
if(min(data$force[stimend:length(data$force)])<=P50){
Relax50time<-which(data$force[stimend:length(data$force)]<=P50)[1] 
Relax50time <- stimend+Relax50time
relax50 <- rateofforce[stimend:Relax50time]
peak.relax.50 <- mean(head(sort(relax50),3))/0.001
average.relax.50 <- mean(relax50)/0.001
} else{relax50 <- "NA"
	peak.relax.50 <- "NA"
	average.relax.50 <- "NA"
	}


###############Gathers the data for analyses in one place  ###############
stimend.force <- data$force[which(data$time == stimend)]
stimend <- stimend*0.001

if(is.character(relax50)){
	Relax50time <- "NA"
	timetorelax50 <- "NA"
	relax.rate.50 <- "NA"
} else{
Relax50time <- Relax50time*0.001
timetorelax50 <- Relax50time-stimend
relax.rate.50 <- (P50-stimend.force)/(timetorelax50)
}

###################Calculate rate ################################
P0
P90
stimstart
stimstart<-stimstart*.001
P90time<-P90time*.001
#can replace forceonset with force start if force onset looks bad
forceonset<-force.onset.time*.001
timetoP90<-P90time-forceonset
ElectroDelay<-forceonset-stimstart
forceRate90<-P90force/timetoP90

P50
stimstart
P50time<-P50time*.001
timetoP50<-P50time-forceonset
forceRate50<-P50force/timetoP50


c(timetoP50,timetoP90,average.rate.99,peak.rate.99,timetorelax50)

