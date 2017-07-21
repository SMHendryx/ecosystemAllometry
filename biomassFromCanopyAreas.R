#This script describes what biomass density would be predicted from a model that only knows canopy area and, specifically, does not know species
#In development of a model to estimate biomass from Structure from Motion Point Clouds
#Specifically, I am trying to answer the question: do we need to know the species of individual plants, or can we account for diversity with species-mixture parameters?
#Load packages:
packages = c('ggplot2', 'data.table', 'dplyr', 'tools', 'plotly')
lapply(packages, library, character.only = TRUE)

#allometric models:
#Cercidium microphyllum (paloverde canopy area allometry-biomass not published?)
#natural log
#log(Y) = (a + b(log(X)))*CF
#Mesquite (Prosopis velutina) biomass - canopy area relationship:
mesqAllom <- function(X){
  #Function takes in Canopy Area (CA) in square meters or vector of CAs and returns Total biomass (kg) of mesquite
  #Equation from McClaran et al. 2013
  a = -.59
  b = 1.60
  CF = 1.06
  biomass <- exp(a + b*(log(X))*CF)
  return(biomass)

}

#hackberry (Celtis pallida)
hackAllom <- function(X){
  #Function takes in Canopy Area (CA) in square meters or vector of CAs and returns Total biomass (kg)
  #From HUANG et al. 2007
  #to return mass in kg, multiply by .001
  biomass <- .001*exp(1.02*(6.78 + 1.41 * log(X)))
  return(biomass)
}

#Burrowweed
#As scripted, this function returns funny values
burrAllom <- function(X){
   biomass<-.001*exp(-4.81 + 1.25 *log(X))
   return(biomass)
}


# Prickly pear (Opuntia engelmannii)
#r 1⁄4 ([center height/2] þ [longest diameter/2])/2, where center height and longest diameter are measured in METERS IN THIS IMPLEMENTATION
prickAllom <- function(r){  
  #Convert input METERS to centimeters:
  r <- r * 100
  biomass <-((4.189 * r^3)^0.965)/(10^5)
  return(biomass)
}  

#Plot lines biomass over canopy area:
x <- seq(from = 1, to = 25, length.out = 1000)
y <- mesqAllom(x)
yHack<- hackAllom(x)
yBurr <- burrAllom(x)

xPrick <- seq(from = 0 , to = 1., length.out = 1000)
yPrick <- prickAllom(xPrick)

plot(x,y, type = "l", col = "red")
lines(x,yHack, col = "darkgreen")
#lines(x,yBurr, col = "orange")
lines(xPrick,yPrick, col = "grey")
legend("topleft", c("Mesquite", "Hackberry", "Pricklypear"), col=c("red", "darkgreen", "grey"), title = NULL, lty = 1)


###############################################################################################################################################################################################################################################################################
# SAMPLING CANOPY AREAS

#  COULD BE A UNIFORM, NORMAL, ETC. DISTRIBUTION, OR WE COULD SAMPLE FROM THE CANOPY AREAS ACTUALLY SEEN IN THE POINT CLOUD?

# To sample Canopy Areas from a Gaussian distribution (the assumption should be verified with data):
# use a truncated Guassian, to represent that we don't find infinitely large plants or plants that have negative dimensions:
#library(msm)
# e.g.: rtnorm(n, 12.5, 3, lower = 0, upper = 25)

#But for now, let's sample from a uniform distribution, by extracting a sequence:
set.seed(1234)
n <- 1000000
mesqCASamp <- seq(0, 25, length = n)
hist(mesqCASamp, main = cat('Simulated Canopy Areas of', n, 'Individuals'), xlab = "Canopy Area (sq. meters)")

#Gaussian varation in mass from an idealized allometric state, for the time being
#Starting with Mesquite:
mesqMassSamp <- mesqAllom(mesqCASamp) + rnorm(n, 0, 4)
hist(mesqMassSamp, main = cat('Allometrically Estimated Mesquite Biomass of', n, 'Individuals'), xlab = "Biomass (kg)")

#And for hackberry:
CASampHack <- seq(0, 25, length = n)
massSampHack <- hackAllom(CASampHack) + rnorm(n, 0, 2)
hist(massSampHack, main = cat('Allometrically Estimated Hackberry Biomass of', n, 'Individuals'), xlab = "Biomass (kg)")

#And pricklypear:
#Note the low upper bound of the parameter r at .6 meters
rSampPrick <-seq(0, .6, length = n)
massSampPrick <- prickAllom(rSampPrick) + rnorm(n, 0, 2)
hist(massSampPrick, main = cat('Allometrically Estimated Prickly Pear Biomass of', n, 'Individuals'), xlab = "Biomass (kg)")

#Visualize the relationship of Biomass to Canopy Area by Species:
plot(mesqCASamp, mesqMassSamp, xlab = "Canopy Area (sq m)", ylab = "Biomass (kg)", main = "Biomass of individaul mesquite plants over canopy area")
dev.new()
plot(x,y, type = "l", col = "red", xlab = "Canopy Area (sq m)", ylab = "Biomass (kg)", main = "Biomass of individaul plant over canopy area")
lines(x,yHack, col = "darkgreen")
lines(xPrick,yPrick, col = "grey")
points(mesqCASamp, mesqMassSamp, col = "red")
points(CASampHack, massSampHack, col = "darkgreen")
points(rSampPrick, massSampPrick, col = "grey")
legend("topleft", c("Mesquite", "Hackberry", "Pricklypear"), col=c("red", "darkgreen", "grey"), title = NULL, lty = 1)

###############################################################################################################################################################################################################################################################################

# But now to the important part
# let's graph how much biomass DENSITY varies if we assume certain species mixtures:
# i.e. what is the estimated biomass density of a hectare of land if assume it as all mesquite vs all hackberry vs some species mixture
# The goal being to see IF the species classification matters when predicting biomass from a common feature such as canopy area or height)
#First we'll assume a number of plants in a given area in hectares
numPlants <- 1000
area <- 1

#Then, we assume some species mixing parameters:
#PROPORTIONS ARE BY NUMBERS OF SPECIES over 1 M
#Prosopis velutina:
pMesquite <- .6

#Celtis pallida:
pHackberry <- .3

#Isocoma tenuisecta: (don't know if this is present at our site)
#pBurroweed <- .025

#Opuntia engelmannii 
pPricklypear <- .1

#Cercidium microphyllum (don't know if this is present at our site nor do I have allometry-biomass relationship for it)
#pPaloverde <- .025

# Sample canopy areas from this distribution:
mesquites <- sample(mesqCASamp, numPlants * pMesquite)
mesquites <- as.data.table(mesquites)
mesquites[,mass := mesqAllom(mesquites)]
colnames(mesquites) <- c("CA", "mass")
mesquites[,species:="mesquite"]

hackberries <- sample(CASampHack, numPlants * pHackberry)
hackberries <- as.data.table(hackberries)
hackberries[,mass:= hackAllom(hackberries)]
colnames(hackberries) <- c("CA", "mass")
hackberries[,species:="hackberry"]

pricklypears <- sample(rSampPrick, numPlants * pPricklypear)
pricklypears <- as.data.table(pricklypears)
pricklypears[,mass:= prickAllom(pricklypears)]
colnames(pricklypears) <- c("CA", "mass")
pricklypears[,species:="pricklypear"]

DT = rbindlist(list(mesquites, hackberries, pricklypears))

# Here 10/14/16
#Print the number of each species:
count(DT, species)

plot(DT$CA, DT$mass)
title(main = "Deterministic Mass over Canopy Area")

# Generate different mass estimates assuming ONE allometric equation
assumeMesq<-mesqAllom(DT[,CA])
sum(assumeMesq)
hist(assumeMesq)
assumeHack<-hackAllom(DT[,CA])
sum(assumeHack)
hist(assumeHack)
assumePrick <- prickAllom(DT[,CA]) # %>% hist() #(won't sum if piped)
sum(assumePrick)

write.csv(DT, "/Users/seanhendryx/DATA/ecosystemAllometry/1000Deterministic_Mass_CA.csv")
# Sum biomasses

#Now do the same thing but add variance:
#Set standard deviation
sd <- 2

mesquites <- sample(mesqCASamp, numPlants * pMesquite)
mesquites <- as.data.table(mesquites)
mesquites[,mass := mesqAllom(mesquites) + rnorm(n, 0, sd)]
colnames(mesquites) <- c("CA", "mass")
mesquites[,species:="mesquite"]

hackberries <- sample(CASampHack, numPlants * pHackberry)
hackberries <- as.data.table(hackberries)
hackberries[,mass:= hackAllom(hackberries) + rnorm(n, 0, sd)]
colnames(hackberries) <- c("CA", "mass")
hackberries[,species:="hackberry"]

pricklypears <- sample(rSampPrick, numPlants * pPricklypear)
pricklypears <- as.data.table(pricklypears)
pricklypears[,mass:= prickAllom(pricklypears) + rnorm(n, 0, sd)]
colnames(pricklypears) <- c("CA", "mass")
pricklypears[,species:="pricklypear"]

DT = rbindlist(list(mesquites, hackberries, pricklypears))

# Here 10/14/16
#Print the number of each species:
count(DT, species)

plot(DT$CA, DT$mass)
title(main = "Stochastic Mass over Canopy Area")
write.csv(DT, "/Users/seanhendryx/DATA/ecosystemAllometry/1000Stochastic_Mass_CA.csv")

#HERE # Here
#Next: run cross validation to infer the polynomial order of a least squares model: f_eco() {the function of ecosystem allometry}






