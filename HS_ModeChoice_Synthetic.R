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
logit <- function(x) log(x/(1-x))

# =========
# MAIN CODE
# =========

# Create synthetic data for model testing 
# Create Attitudes data (ID, Item, Factor, Male, Score)-----------
n <- 100
# Person ID
ID <- rep(1:n, each=6)
ID2 <- ID
# Item ID
Item <- rep(1:6, times=n)
#Factor (factor 1 = 1, factor 2 = 2)
Factor <- rep(1:2, times=n, each=3)
# Factor dummy
Factor1Dum <- rep(c(1,0), times=n, each=3)
Factor2Dum <- rep(c(0,1), times=n, each=3)
# Loadings (item 1:6 loadings)
Load1 = rep(c(1,1,5,0,0,0),times=n)
Load2 = rep(c(0,0,0,2,1,1),times=n)
#Factor scores
FactorScores <-  rep(c(rbind(rnorm(n,1,.1),rnorm(n,2,.1))),each=3)
# Potential covariate for later
Male <- rep(rbinom(n,1,.5), each=6)

# Create Scores
Score <- rep(NA,length(ID))
# generate phi and scores
for (i in 1:length(ID)){
  phi = Load1[i]*FactorScores[i] + Load2[i]*FactorScores[i]
  Score[i] = rordlogit(1,phi,1:4)
}
# Make sure scores by item look correct
table(Score,Item, Factor)

# Plot Factor 1 Item loadings
barplot(table(Score,Item,Factor)[,,1])
# Plot Factor 2 Item loadings
barplot(table(Score,Item,Factor)[,,2])

# discrete proportion of each response value 
pr_k <- table( Score ) / length(Score)
# cumsum converts to cumulative proportions 
cum_pr_k <- cumsum( pr_k )
# plot 
log(cum_pr_k/(1-cum_pr_k))


# Test intercept ordered logit model-------------------
m.int_comp <- map2stan(
              alist(
                Score ~ dordlogit(phi, cutpoints),
                phi <- 0,
                cutpoints ~ dnorm(0,10)
              ),
              data=list(Score=Score,Item=Item,Factor=Factor,ID=ID,ID2=ID2,Factor1Dum=Factor1Dum,
                        Factor2Dum=Factor2Dum),
              start=list(cutpoints=c(-2,-1.7,-1.5,-1.4)),
              iter=2, warmup=1
            )
m.int <- resample(m.int_comp,iter=1000,warmup=300)
precis(m.int,depth=2)

# Intercept model returns correct cum_pr_k as mean cutpoints (looking good!)

# 2 Factor Model-------------------------------
m.LV2c <- map2stan(
              alist(
                Score ~ dordlogit(phi, cutpoints),
                phi <- Loading[Item]*Attitude1[ID]*Factor1Dum + 
                        Loading[Item]*Attitude2[ID]*Factor2Dum,
                Loading[Item] ~ dnorm(0,2),
                Attitude1[ID] ~ dnorm(0,sigma1),
                Attitude2[ID] ~ dnorm(0,sigma2),
                sigma1 ~ dcauchy(0,.5),
                sigma2 ~ dcauchy(0,.5),
                cutpoints ~ dnorm(0,5)
                ),
              data=list(Score=Score,Item=Item,ID=ID,Factor1Dum=Factor1Dum,
                        Factor2Dum=Factor2Dum),
              start=list(cutpoints=c(-1,-.2,.4,1)),
              iter=2, warmup=1
            )
m.LV2 <- resample(m.LV2c,iter=2000,warmup=500)
plot(precis(m.LV2,depth=2))

# Get samples and look at parameter posterior densities
post <- extract.samples(m.LV2)
dens(post$Loading[,1],xlim=c(-6,6))
dens(post$Loading[,2],add=TRUE)
dens(post$Loading[,3],add=TRUE)
dens(post$Loading[,4],add=TRUE,col="blue")
dens(post$Loading[,5],add=TRUE,col="blue")
dens(post$Loading[,6],add=TRUE,col="blue")

# Use model to predict outcome
p.cutpoints <- unname(m.LV2@coef[1:4])
p.load1 <- rep(c(unname(m.LV2@coef[5:7]),0,0,0),times=n)
p.load2 <- rep(c(0,0,0,unname(m.LV2@coef[8:10])),times=n)
p.Att <- rep(c(rbind(unname(m.LV2@coef[11:110]),unname(m.LV2@coef[111:210]))),each=3)

# Create Scores
p.score <- rep(NA,length(ID))
for (i in 1:length(ID)){
  phi = p.load1[i]*p.Att[i] + p.load2[i]*p.Att[i]
  p.score[i] = rordlogit(1,phi,p.cutpoints) 
}
# Make sure scores by item look correct (Factor 1 looks good, but not Factor 2)
table(p.score,Item, Factor)

# Plot Factor 1 Item loadings
par(mfrow=c(1,2))
barplot(table(Score,Item,Factor)[,,1],main="Factor 1 Synthetic Data")
barplot(table(p.score,Item,Factor)[,,1],main="Factor 1 Predicted Data")
# Plot Factor 2 Item loadings
barplot(table(Score,Item,Factor)[,,2],main="Factor 2 Synthetic Data")
barplot(table(p.score,Item,Factor)[,,2],main="Factor 2 Predicted Data")

# Not working!



# Create Choice data -------------------------------------------
# Person ID
ID2 <- rep(1:n)
# Distance data for each mode. Lognormal is runing into problems
Dist_D <- rlnorm(n,meanlog=1,sdlog=1)
Dist_B <- Dist_D - (.25*Dist_D)
Dist_W <- Dist_D - (.5*Dist_D)
Dist_Bu <- Dist_D + (.25*Dist_D)
Dist_O <- Dist_D + (.1*Dist_D)
# Ordered bike environment variable
Bike_Env <- rordlogit(n,1.5,1:3)
# Parent education > BA degree
Parent_Ed <- rbinom(n,1,.5)
# School ID, for later use
School <- rep(1:2,each=(n/2))

# Each mode specific linear model
score_D <- -1*Dist_D + 2*Parent_Ed   
score_B <- -2*Dist_B + .5*Parent_Ed + .5*Bike_Env
score_W <- -2.2*Dist_W + 1*Parent_Ed 
score_Bu <- -2*Dist_Bu + 2*Parent_Ed 
score_O <- -1*Dist_O + 1*Parent_Ed 
# convert scores to probabilities 
p <- softmax(score_D, score_B, score_W, score_Bu, score_O)
# now simulate choice 
Mode <- rep(NA,n)
# sample mode for each individual 
for ( i in 1:n ){
  Mode[i] <- sample( 1:5 , size=1 , prob=p[i,] )
} 
hist(Mode)

# Mode Choice Model (NO latent variables)--------------------
m.mnl_comp <- map2stan(
            alist(
              Mode ~ dcategorical(softmax(0,bike,walk,bus,other)),
              bike <- a_b + b_bdist*Dist_B + b_bPE*Parent_Ed + b_bBE*Bike_Env,
              walk <- a_w + b_wdist*Dist_W + b_wPE*Parent_Ed,
              bus <- a_bu + b_budist*Dist_Bu + b_buPE*Parent_Ed,
              other <- a_o + b_odist*Dist_O + b_oPE*Parent_Ed,
              c(a_b,a_w,a_bu,a_o) ~ dnorm(0,10),
              c(b_bdist,b_bPE,b_bBE,b_wdist,b_wPE,b_budist,b_buPE,b_odist,b_oPE) ~ dnorm(0,1)
              ),
            data = list(Mode=Mode,Dist_B=Dist_B,Dist_W=Dist_W,Dist_Bu=Dist_Bu,
                          Dist_O=Dist_O,Parent_Ed=Parent_Ed,Bike_Env=Bike_Env),
            iter=2, warmup=1
          )
m.mnl <- resample(m.mnl_comp,iter=2000,warmup=500)

pred <- sim(m.mnl)
# More complex mnl model ---------------------------------------------
# added varying intercepts by school
m.mnl2_comp <- map2stan(
                alist(
                  Mode ~ dcategorical(softmax(0,bike,walk,bus,other)),
                  bike <- a_b + a_bj[School]+ b_bdist*Dist_B + b_bPE*Parent_Ed + b_bBE*Bike_Env,
                  walk <- a_w + a_wj[School]+ b_wdist*Dist_W + b_wPE*Parent_Ed,
                  bus <- a_bu + a_buj[School]+ b_budist*Dist_Bu + b_buPE*Parent_Ed,
                  other <- a_o + a_oj[School]+ b_odist*Dist_O + b_oPE*Parent_Ed,
                  c(a_b,a_w,a_bu,a_o) ~ dnorm(0,10),
                  c(a_dj,a_bj,a_wj,a_buj,a_oj)[School] ~ dnorm(0,sigma_school),
                  c(b_bdist,b_bPE,b_bBE,b_wdist,b_wPE,b_budist,b_buPE,b_odist,b_oPE) ~ dnorm(0,1),
                  sigma_school ~ dcauchy(0,1)
                ),
                data = list(Mode=Mode,Dist_B=Dist_B,Dist_W=Dist_W,Dist_Bu=Dist_Bu,
                            Dist_O=Dist_O,Parent_Ed=Parent_Ed,Bike_Env=Bike_Env, School=School),
                iter=2, warmup=1
              )
m.mnl2 <- resample(m.mnl_comp,iter=1200,warmup=300)

# More complex mnl model ---------------------------------------------
# added varying slopes by individual
m.mnl3_comp <- map2stan(
  alist(
    Mode ~ dcategorical(softmax(0,bike,walk,bus,other)),
    bike <- a_b + b_bdist[ID2]*Dist_B + b_bPE*Parent_Ed + b_bBE*Bike_Env,
    walk <- a_w + b_wdist[ID2]*Dist_W + b_wPE*Parent_Ed,
    bus <- a_bu + b_budist[ID2]*Dist_Bu + b_buPE*Parent_Ed,
    other <- a_o + b_odist[ID2]*Dist_O + b_oPE*Parent_Ed,
    c(b_bdist,b_wdist,b_budist,b_odist)[ID2] ~ dmvnorm2(0,sigma_person,Rho_person),
    sigma_person ~ dcauchy(0,2),
    Rho_person ~ dlkjcorr(4),
    c(a_b,a_w,a_bu,a_o) ~ dnorm(0,10),
    c(b_bPE,b_bBE,b_wPE,b_buPE,b_oPE) ~ dnorm(0,1)
  ),
  data = list(Mode=Mode,Dist_B=Dist_B,Dist_W=Dist_W,Dist_Bu=Dist_Bu,
              Dist_O=Dist_O,Parent_Ed=Parent_Ed,Bike_Env=Bike_Env, ID2=ID2),
  iter=2, warmup=1
)
m.mnl3 <- resample(m.mnl3_comp,iter=1200,warmup=300)
