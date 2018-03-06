# library(nlstools)
# library(nls2)
# library(scales)
# require(lmodel2)
# require(smatr)
# require(car)

# This code runs linear and non-linear regression models to compare scaling exponents with expected values.

#  #load in Muscel_Scaling_Data.csv
filename <- file.choose()
filename <- basename(filename)
physdata <- (read.table(filename, sep = ",", header=T,skip=1))

#Calculate muscle volume from mass and muscle density (CITE)
Volume <- (physdata$muscle.mass*1000)/1.06 #cm3

#Calculate the average CSA from volume and muscle length
CSA <- Volume/(physdata$L0*100) # in cm2

#Calculate pCSA by dividing CSA by the cos of the fascicle angle in radians
pCSA <- CSA/cos(physdata$Angle/(180/pi)) # in cm2

#Convert pCSA to mm2
pCSA <- pCSA*10 #in mm2

# Stress, or specific tension, in N/mm2
Stress <- physdata$P0/pCSA

# Define the variables for the regression models
y <- physdata$predictmspower
x <- physdata$SVL

# Define the expected scaling exponent
expected <- 3

# Create a dataframe containing the variables of interest
datax <- data.frame(x=x,y=y)
datax <- datax[order(datax$x),]

# Create a linear model of log10-transformed data
model_lr <- lm(log10(y)~log10(x), data=datax)

# Extract the slope, intercept, and standard error of the slope from the linear model
b_lr <- summary(model_lr)$coef[2,1]
a_lr <- summary(model_lr)$coef[1,1]
b_lrSE <- summary(model_lr)$coef[2,2]

#Calculate the AIC for the linear model, can't use the AIC function on the log10 transformed data
sigma<-sqrt(sum(residuals(model_lr)^2)/nrow(datax))
model.LL <- sum(log10(dnorm(log10(datax$y), predict(model_lr), sigma) * 1/(datax$y)))
LRAIC <- -2*model.LL + 2*(length(coef(model_lr))+1)

# Create a non-linear model on the untransformed data using the slope and intercept from the linear model as seed values
model_nlr = nls(y ~ a1 * x ^ a2, start = list(a1 = a_lr, a2 = b_lr),data=datax, control = nls.control(maxiter = 2000, warnOnly = TRUE))

# Extract the slope and intercept from the nonlinear model and SE for slope
a_nlr = coef(summary(model_nlr))[1, 1]
b_nlr = coef(summary(model_nlr))[2, 1]
b_nlrSE = coef(summary(model_nlr))[2,2]

# Calculate the AIC for the nonlinear model
NLRAIC <- AIC(model_nlr)

# Extract results for table, ttests performed in Excel
c(expected, b_nlr,b_nlrSE,NLRAIC,b_lr,b_lrSE,LRAIC)

