#
# High School Mode Choice Study
#
# Dillon Fitch 
# Revised: Spring 2015
#
# Notes: Generates synthetic data for testing model structures,
# and builds latent variable and mode choice models.

# ======================
# LOAD LIBRARIES AND OTHER SOURCES
# ======================
# Below libraries are required to run this script

library(rethinking)

# =========
# FUNCTIONS
# =========
# Below functions are used in the script


# =========
# MAIN CODE
# =========

# Create synthetic data for model testing 
# Create Attitudes data (ID, Item, Factor, Male, Score)-----------
n <- 3000
# Person ID
ID <- rep(1:n, each=6)
# Item ID
Item <- rep(1:6, times=n)
#Factor ID
FID <- rep(1:2, times=n, each=3)
#break factor into two dummy variables
Factor1 <- rep(c(1,0), times=n, each=3)
Factor2 <- rep(c(0,1), times=n, each=3)
# Loadings
Load = rep(c(.5,2,4,4,2,.5),times=n)
# Potential covariate for later
Male <- rep(rbinom(n,1,.5), each=6)

# Create Scores
Score <- rep(NA,length(ID))
# generate phi and scores
for (i in 1:length(ID)){
  phi = Load[i]*Factor1[i] + Load[i]*Factor2[i]
  Score[i] = rordlogit(1,phi,1:4)
}
# Make sure scores by item look correct
table(Score,Item)

# Create Choice data -------------------------------------------
# Person ID
ID2 <- rep(1:n)
# Distance data for each mode. Lognormal is runing into problems
Dist_D <- rlnorm(n,meanlog=0,sdlog=1)
Dist_B <- Dist_D - .25
Dist_W <- Dist_D - .5
Dist_Bu <- Dist_D + .25
Dist_O <- Dist_D + .1
# Ordered bike environment variable
Bike_Env <- rordlogit(n,1.5,1:3)
# Parent education > BA degree
Parent_Ed <- rbinom(n,1,.5)
# School ID, for later use
School <- rordlogit(n,1.7,1:2)

# Each mode specific linear model
score_D <- 0.25*Dist_D + .5*Parent_Ed   
score_B <- 0.5*Dist_B + .5*Parent_Ed + .5*Bike_Env
score_W <- 1*Dist_W + .5*Parent_Ed 
score_Bu <- 0.25*Dist_Bu + .5*Parent_Ed 
score_O <- 0.25*Dist_O + .5*Parent_Ed 
# convert scores to probabilities 
p <- softmax(score_D, score_B, score_W, score_Bu, score_O)
# now simulate choice 
Mode <- rep(NA,n)
# sample mode for each individual 
for ( i in 1:n ){
  Mode[i] <- sample( 1:5 , size=1 , prob=p[i,] )
} 


# Attempt to define models using rethinking

# Latent Variable Model (two latent variables)--------------------

m.LV_comp<- map2stan(
          alist(
            Score ~ dordlogit(phi, cutpoints),
            phi <- Load[Item]*Factor[FID],
            Load[Item] ~ dnorm(0,10),
            Factor[FID] ~ dnorm(0,1),
            cutpoints ~ dnorm(0,10)
            ),
          data=list(Score=Score,Item=Item,FID=FID),
          start=list(cutpoints=c(-.5,-.25,0,.25)),
          iter=2, warmup=1
        )
m.LV <- resample(m.LV_comp,iter=1000,warmup=500)

#SCRAP
c(cutpoints,Load)[Item] ~ dmvnorm2(c(a,b),sigma_item,Rho),
c(a,b) ~ dnorm(0,10),

# Mode Choice Model (NO latent variables)--------------------

