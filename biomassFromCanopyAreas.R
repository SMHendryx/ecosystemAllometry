#This script describes what biomass density would be predicted from a model that only knows canopy area and, specifically, does not know species
#In development of a model to estimate biomass from Structure from Motion Point Clouds
#Specifically, I am trying to answer the question: do we need to know the species of individual plants, or can we account for diversity with species-mixture parameters?
#Load packages:
packages = c('ggplot2', 'data.table', 'dplyr', 'tools', 'plotly', 'feather')
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
# SAMPLING CANOPY AREAS FOR EACH SPECIES
# using stratified sampling/uniform distribution

# To sample Canopy Areas from a Gaussian distribution (the assumption should be verified with data):
# use a truncated Guassian, to represent that we don't find infinitely large plants or plants that have negative dimensions:
#library(msm)
# e.g.: rtnorm(n, 12.5, 3, lower = 0, upper = 25)

#But for now, let's sample from a uniform distribution, by extracting a sequence:
# in field sampling, such as in McClaren et al.'s 2013 paper, they sample a sequence to represent the possible growth forms, not ecosystem-state-
# -dependent size distributions
#Gaussian varation in mass from an idealized allometric state
# standard deviation taken from McClaren et al. 2013, standard error or the regression of biomass over canopy area:
se = 0.589227544
# Mitch McClaren's number of samples
n = 31
#se = sd/sqrt(n)
sd = se * sqrt(n)

set.seed(1234)
numPlantsEachSpecies <- 1000000
mesqCASamp <- seq(0, 60, length = numPlantsEachSpecies)
hist(mesqCASamp, main = cat('Simulated Canopy Areas of', numPlantsEachSpecies, 'Individuals'), xlab = "Canopy Area (sq. meters)")

#Starting with Mesquite:
mesqMassSamp <- mesqAllom(mesqCASamp) + rnorm(numPlantsEachSpecies, 0, sd)
hist(mesqMassSamp, main = cat('Allometrically Estimated Mesquite Biomass of', numPlantsEachSpecies, 'Individuals'), xlab = "Biomass (kg)")

#And for hackberry:
CASampHack <- seq(0, 30, length = numPlantsEachSpecies)
massSampHack <- hackAllom(CASampHack) + rnorm(numPlantsEachSpecies, 0, sd)
hist(massSampHack, main = cat('Allometrically Estimated Hackberry Biomass of', numPlantsEachSpecies, 'Individuals'), xlab = "Biomass (kg)")

#And pricklypear:
#Note the low upper bound of the parameter r at .6 meters
rSampPrick <-seq(0, .6, length = numPlantsEachSpecies)
massSampPrick <- prickAllom(rSampPrick) + rnorm(numPlantsEachSpecies, 0, sd)
hist(massSampPrick, main = cat('Allometrically Estimated Prickly Pear Biomass of', numPlantsEachSpecies, 'Individuals'), xlab = "Biomass (kg)")

#Visualize the relationship of Biomass to Canopy Area by Species:
#plot(mesqCASamp, mesqMassSamp, xlab = "Canopy Area (sq m)", ylab = "Biomass (kg)", main = "Biomass of individaul mesquite plants over canopy area")
#dev.new()
#plot(x,y, type = "l", col = "red", xlab = "Canopy Area (sq m)", ylab = "Biomass (kg)", main = "Biomass of individaul plant over canopy area")
#lines(x,yHack, col = "darkgreen")
#lines(xPrick,yPrick, col = "grey")
#points(mesqCASamp, mesqMassSamp, col = "red")
#points(CASampHack, massSampHack, col = "darkgreen")
#points(rSampPrick, massSampPrick, col = "grey")
#legend("topleft", c("Mesquite", "Hackberry", "Pricklypear"), col=c("red", "darkgreen", "grey"), title = NULL, lty = 1)

###############################################################################################################################################################################################################################################################################

# Synthesize observations:
numPlants = 10000
area = 1

#Then, we assume some species mixing parameters:
#PROPORTIONS ARE BY NUMBERS OF SPECIES over 1 M
#Prosopis velutina:
pMesquite <- 0.8271605
numMesq = numPlants * pMesquite

#Celtis pallida:
pHackberry <- 0.1728395
numHack = numPlants * pHackberry

#Isocoma tenuisecta: (don't know if this is present at our site)
#pBurroweed <- .025

#Opuntia engelmannii 
pPricklypear <- 0.0
numPrick = numPlants * pPricklypear

#Cercidium microphyllum (don't know if this is present at our site nor do I have allometry-biomass relationship for it)
#pPaloverde <- .025

# Sample canopy areas from this distribution:
mesquites <- sample(mesqCASamp, numMesq)
mesquites <- as.data.table(mesquites)
mesquites[,mass := mesqAllom(mesquites)]
colnames(mesquites) <- c("CA", "mass")
mesquites[,species:="mesquite"]

hackberries <- sample(CASampHack, numHack)
hackberries <- as.data.table(hackberries)
hackberries[,mass:= hackAllom(hackberries)]
colnames(hackberries) <- c("CA", "mass")
hackberries[,species:="hackberry"]

pricklypears <- sample(rSampPrick, numPrick)
pricklypears <- as.data.table(pricklypears)
pricklypears[,mass:= prickAllom(pricklypears)]
colnames(pricklypears) <- c("CA", "mass")
pricklypears[,species:="pricklypear"]

DT = rbindlist(list(mesquites, hackberries, pricklypears))

#Print the number of each species:
count(DT, species)

#plot(DT$CA, DT$mass)
#title(main = "Deterministic Mass over Canopy Area")

# Generate different mass estimates assuming ONE allometric equation
assumeMesq<-mesqAllom(DT[,CA])
sum(assumeMesq)
hist(assumeMesq)
assumeHack<-hackAllom(DT[,CA])
sum(assumeHack)
hist(assumeHack)
assumePrick <- prickAllom(DT[,CA]) # %>% hist() #(won't sum if piped)
sum(assumePrick)

#write.csv(DT, "/Users/seanhendryx/DATA/ecosystemAllometry/1000Deterministic_Mass_CA.csv")
# Sum biomasses

#Now do the same thing, generating a large sample of CA and Mass values from the species distribution but add variance (make non-deterministic) to the mass:
mesquites <- sample(mesqCASamp, numMesq)
mesquites <- as.data.table(mesquites)
mesquites[,mass := mesqAllom(mesquites) + rnorm(numMesq, 0, sd)]
colnames(mesquites) <- c("CA", "mass")
mesquites[,species:="mesquite"]

hackberries <- sample(CASampHack, numHack)
hackberries <- as.data.table(hackberries)
hackberries[,mass:= hackAllom(hackberries) + rnorm(numHack, 0, sd)]
colnames(hackberries) <- c("CA", "mass")
hackberries[,species:="hackberry"]

pricklypears <- sample(rSampPrick, numPrick)
pricklypears <- as.data.table(pricklypears)
pricklypears[,mass:= prickAllom(pricklypears) + rnorm(numPrick, 0, sd)]
colnames(pricklypears) <- c("CA", "mass")
pricklypears[,species:="pricklypear"]

DT = rbindlist(list(mesquites, hackberries, pricklypears))

#Print the number of each species:
count(DT, species)

#p = ggplot(data = DT, mapping = aes(x = CA, y = mass)) + geom_point(mapping = aes(color = species), alpha = 1) + labs(x = expression(paste("Canopy Area (", {m^2}, ")")), y = "AGB (kg)") + theme_bw()

write_feather(DT, "/Users/seanhendryx/DATA/ecosystemAllometry/measuredSpeciesDistribution/Generated_Mass_and_CAs.feather")

#HERE # Here
#Next: run cross validation to infer the polynomial order of a least squares model: f_eco() {the function of ecosystem allometry}






