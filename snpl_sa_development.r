## **************************************************************##
## File: snpl_sa_development.r
## Author: Matthew Aiello-Lammens
## Data: 14 January 2013
##
## Purpose: This file includes R code for the the development
## of analysis techniques used to examine results from the 
## snowy plover analysis.

require(ggplot2)
require(reshape)
require(fields)
require(dismo)
require(grid)
require(gridExtra)
require(plyr)
require(reshape2)
require(rgl)

## Mac
sa.sens.dir <- '/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/'
sacode.snpl <- '/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/SACode_SNPL/'
brt.dir <- '/Volumes/Garage/Projects/SnowyPlover/BRT_Models/'
# Akcakaya Group
#sa.sens.dir <- '/Users/Matthew/Documents/My Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/'
#sacode.snpl <- '/Users/Matthew/Documents/My Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/SACode_SNPL/'
#brt.dir <- '/Projects/SnowyPlover/BRT_Models/'
setwd( sa.sens.dir )
# Source helpful SA functions
source( paste(sacode.snpl,'snpl_sa_functions.r',sep=''))

## Examining the results from SA2
# load results
snpl.10k.rep.SA2 <- read.csv('Results/snpl.10000.rep.SA2.csv')
snpl.100.rep.SA2 <- read.csv('Results/snpl.100.rep.SA2.csv')
snpl.250.rep.SA2 <- read.csv('Results/snpl.250.rep.SA2.csv')
snpl.500.rep.SA2 <- read.csv('Results/snpl.500.rep.SA2.csv')
snpl.1000.rep.SA2 <- read.csv('Results/snpl.1000.rep.SA2.csv')

# Partition the resutls data.frames
snpl.10k.SA2.part <- snpl.df.partition( snpl.10k.rep.SA2 )
snpl.100.SA2.part <- snpl.df.partition( snpl.100.rep.SA2 )
snpl.250.SA2.part <- snpl.df.partition( snpl.250.rep.SA2 )
snpl.500.SA2.part <- snpl.df.partition( snpl.500.rep.SA2 )
snpl.1000.SA2.part <- snpl.df.partition( snpl.1000.rep.SA2 )

## Examining the results from SA1
# load results
snpl.10k.rep.SA1 <- read.csv('Results/snpl.10000.rep.csv')
snpl.100.rep.SA1 <- read.csv('Results/snpl.100.rep.csv')
snpl.250.rep.SA1 <- read.csv('Results/snpl.250.rep.csv')
snpl.500.rep.SA1 <- read.csv('Results/snpl.500.rep.csv')
snpl.1000.rep.SA1 <- read.csv('Results/snpl.1000.rep.csv')
snpl.10.rep.SA1 <- read.csv('Results/snpl.10.rep.csv')

# Partition the resutls data.frames
snpl.10k.SA1.part <- snpl.df.partition( snpl.10k.rep.SA1 )
snpl.100.SA1.part <- snpl.df.partition( snpl.100.rep.SA1 )
snpl.250.SA1.part <- snpl.df.partition( snpl.250.rep.SA1 )
snpl.500.SA1.part <- snpl.df.partition( snpl.500.rep.SA1 )
snpl.1000.SA1.part <- snpl.df.partition( snpl.1000.rep.SA1 )
snpl.10.SA1.part <- snpl.df.partition( snpl.10.rep.SA1 )

## **************************************************************##
## Calculate some useful summary statistics and calculations
## **************************************************************##

## Mean and SD of Prob Decline to 50 - Tabled by SLR and RandType
## --------------------------------------------------------------------
p50.SA1.means <- tapply( snpl.10k.rep.SA1$prob.50, 
                         list( snpl.10k.rep.SA1$SLR, snpl.10k.rep.SA1$RandType ), 
                         mean)
p50.SA1.sd <- tapply( snpl.10k.rep.SA1$prob.50, 
                         list( snpl.10k.rep.SA1$SLR, snpl.10k.rep.SA1$RandType ), 
                         sd)

p50.SA2.means <- tapply( snpl.10k.rep.SA2$prob.50, 
                         list( snpl.10k.rep.SA2$SLR, snpl.10k.rep.SA2$RandType ), 
                         mean)
p50.SA2.sd <- tapply( snpl.10k.rep.SA2$prob.50, 
                         list( snpl.10k.rep.SA2$SLR, snpl.10k.rep.SA2$RandType ), 
                         sd)

## Calculate a t-stat value for differences between SA1-mean for Unif vs LHS
p50.nocc <- subset( snpl.10k.rep.SA1, subset=snpl.10k.rep.SA1$SLR=="NoCC" )
p50.nocc.tstat <- t.test(formula=p50.nocc$prob.50~p50.nocc$RandType)$statistic
p50.2m <- subset( snpl.10k.rep.SA1, subset=snpl.10k.rep.SA1$SLR=="2M" )
t.test(formula=p50.2m$prob.50~p50.2m$RandType)

# Create a distribution for t-stats using permutation - just for one case
t.vals <- c()
for (i in 1:10000){
  # Create new data set
  new.data <- data.frame( prob.50=p50.nocc$prob.50, 
                          RandType=sample(p50.nocc$RandType) )
  t.vals[i] <- t.test(formula=new.data$prob.50~new.data$RandType)$statistic
}
length( which( abs(p50.nocc.tstat) <= abs( t.vals ) ) )/10000
## The p-value here ends up being around 0.13, not very compelling

## **************************************************************##
## Choose/Set ten groups of the sample size 10 sets to work with
## **************************************************************##

## First work with the PartNum=10 case
# Pick 10 random RepNumbers to work with
#reps.toUse <- sample(1:1000,10)
reps.toUse <- 1:1000 # This reps.toUse value used to look at all reps
  # requires some changes to the code below too
# Here I'm setting reps.toUse to a previously generated set of 10 reps
# to be used **specifically** for Part.Num = 10
##reps.toUse <- c(2,10,31,39,54,72,84,88,92,96)
# Get indices for these reps. Indices will be 
# the same for each partition within PartNum sets
get.rep <- function( RN ){ which( snpl.10.SA1.part$lhs.nocc$RepNumber == RN ) }
rep.ind <- sapply( reps.toUse, get.rep )
rep.ind <- as.vector(rep.ind)

## **************************************************************##
## Below is code to examine differences in 'evenness' of 
## sampling in paramter space for the two sampling methods,
## Unif and LHS.  I'm using comparisons of 
## Nearest Neighbor values, between just two of the parameters
## sampled (ad.surv and fecund).  
## **************************************************************##

# Make bivariate plots of fecundity and survival to 
# examine visually the differences between unif
# and LHS sampling.  Note that these values will be the
# same for NoCC and 2M for each RandType
snpl.10.bivar.df <- rbind( snpl.10.SA1.part$lhs.nocc[ rep.ind, ], 
                            snpl.10.SA1.part$unif.nocc[ rep.ind, ] )
# Make bivariate plots
snpl.bv <- ggplot( snpl.10.bivar.df, aes( ad.surv, fecund )) + geom_point() + facet_grid(RandType~RepNumber)

## Here's the *Nearest Neighbor* analysis for 10 repetitions 
## of the PartNum=10 dataset.
# Make two empty data.frames
snpl.nn.unif <- vector()
snpl.nn.lhs <- vector()
# Set input variable column numbers to compare in dist matrix
input.dims <- c(2,10,11,63,64,62,48,61)

for ( Rep in 1:length(reps.toUse) ){
  # Get subsets of the LHS and Unif data
  snpl.unif.rep <- subset( snpl.10.SA1.part$unif.nocc, subset=RepNumber==reps.toUse[Rep] )
  snpl.lhs.rep <- subset( snpl.10.SA1.part$lhs.nocc, subset=RepNumber==reps.toUse[Rep] )

  ## Calculate distance matrix for all dimensions
  # first convert kch.type to a numeric value
  snpl.unif.rep$kch.type <- as.numeric(snpl.unif.rep$kch.type)
  snpl.lhs.rep$kch.type <- as.numeric(snpl.lhs.rep$kch.type)
  
  # Get only input dims
  snpl.unif.inputs <- cbind(snpl.unif.rep[input.dims])
  snpl.lhs.inputs <- cbind(snpl.lhs.rep[input.dims])
  
  # Scale all inputs to be between 0 and 1 by dividing each 
  # input by the max input value
  snpl.unif.inputs.max <- apply(snpl.unif.inputs,MARGIN=2,FUN=max)
  snpl.lhs.inputs.max <- apply(snpl.lhs.inputs,MARGIN=2,FUN=max)
  # To do the division, use this little trick so the vector 
  # cycling of the max vector lines up correctly with the inputs
  # matrix
  snpl.unif.inputs <- t(t(snpl.unif.inputs)/snpl.unif.inputs.max)
  snpl.lhs.inputs <- t(t(snpl.lhs.inputs)/snpl.lhs.inputs.max)
  
  # Calculate distance matrices
  #  dist.unif <- rdist( cbind(snpl.unif.rep$ad.surv, snpl.unif.rep$fecund) )
  #  dist.lhs <- rdist( cbind(snpl.lhs.rep$ad.surv, snpl.lhs.rep$fecund) )
  dist.unif <- as.matrix(dist(snpl.unif.inputs))
  dist.lhs <- as.matrix(dist(snpl.lhs.inputs))
   
  # Set diagonal elements to NAs
  diag(dist.unif) <- NA
  diag(dist.lhs) <- NA
  
  # Calc Nearest Neighbor
  near.neigh.unif <- apply( dist.unif, 2, min, na.rm=T )
  near.neigh.lhs <- apply( dist.lhs, 2, min, na.rm=T)
  
  # Return as part of data.frame
  snpl.nn.unif <- rbind( snpl.nn.unif, cbind(near.neigh.unif,rep(reps.toUse[Rep],10)) )
  snpl.nn.lhs <- rbind( snpl.nn.lhs, cbind(near.neigh.lhs,rep(reps.toUse[Rep],10)) )
}
# Combine nearest neighbor data.frames
snpl.nn <- rbind( snpl.nn.unif, snpl.nn.lhs )
snpl.nn <- as.data.frame(snpl.nn)
rand.type <- rep(c("Unif","LHS"),each=10000) # Change to each=10000 if using reps.toUse=1:1000
snpl.nn <- cbind( snpl.nn, rand.type )
names(snpl.nn) <- c("near.neighbor","RepNumber","RandType")
snpl.nn$RepNumber <- as.factor( snpl.nn$RepNumber )
# Copy snpl.nn to snpl.nn.10
snpl.nn.10
snpl.nn.10$SampleSize <- 10
# Plot these data
snpl.nn.hist <- ggplot() + geom_histogram(aes(x=near.neighbor),data=snpl.nn) + facet_grid(RandType~RepNumber)

# Make figure of bivariate plots with nearest neighbor
# histograms
pdf('figures/snpl.10.SA1.unifVlhs.pdf',width=14)
grid.arrange( snpl.bv, snpl.nn.hist )
dev.off()

## Calculate means and standard deviations of the nearest neighbors
snpl.nn.mean.10 <- tapply( snpl.nn.10$near.neighbor, 
                        list( snpl.nn.10$RepNumber, snpl.nn.10$RandType ), 
                        mean )
snpl.nn.var.10 <- tapply( snpl.nn.10$near.neighbor, 
                      list( snpl.nn.10$RepNumber, snpl.nn.10$RandType ), 
                      var )
snpl.nn.mean.10.df <- melt(as.data.frame(snpl.nn.mean.10))
snpl.nn.var.10.df <- melt(as.data.frame(snpl.nn.var.10))
names(snpl.nn.mean.10.df) <- c('samp.type','near.neighbor')
names(snpl.nn.var.10.df) <- c('samp.type','near.neighbor')
## Calculate the mean nearest neighbor across the 1000 sample sets
apply(snpl.nn.mean.10,2,mean)
apply(snpl.nn.var.10,2,mean)
apply(snpl.nn.mean.10,2,sd)
## Calculate the coefficient of variation for this sample size
apply(snpl.nn.mean.10,2,sd)/apply(snpl.nn.mean.10,2,mean)

## Carry out a  t-test on the means
snpl.nn.t.test.10 <- t.test(x=snpl.nn.mean.10[,1],y=snpl.nn.mean.10[,2])
print(snpl.nn.t.test.10)

## Carry out an ANOVA
snpl.nn.lm.10 <- lm( near.neighbor~RepNumber * RandType, data=snpl.nn.10)
anova(snpl.nn.lm.10)

## Make a plot
pdf('figures/lhs_unif_10_mean_nearneighbor_hist.pdf')
ggplot(data=snpl.nn.mean.10.df,aes(x=near.neighbor,fill=samp.type)) + 
  geom_histogram(position="identity",alpha=0.5)
dev.off()

## **************************************************************##
## Choose/Set ten groups of the sample size 100 sets to work with
## **************************************************************##

## First work with the PartNum=100 case
# Pick 10 random RepNumbers to work with
#reps.toUse <- sample(1:100,10)

# For figure makeing, choose only 5 plots to work with
set.seed(1981)
reps.toUse <- sample(1:100,5)

# Here I'm setting reps.toUse to a previously generated set of 10 reps
# to be used **specifically** for Part.Num = 100
#reps.toUse <- c(2,10,31,39,54,72,84,88,92,96)
#reps.toUse <- 1:100
# Get indices for these reps. Indices will be 
# the same for each partition within PartNum sets
get.rep <- function( RN ){ which( snpl.100.SA1.part$lhs.nocc$RepNumber == RN ) }
#rep.ind <- sapply( reps.toUse, get.rep )
#rep.ind <- as.vector(rep.ind)
rep.ind <- which( snpl.100.SA1.part$lhs.nocc$RepNumber %in% reps.toUse )

## **************************************************************##
## Below is code to examine differences in 'evenness' of 
## sampling in paramter space for the two sampling methods,
## Unif and LHS.  I'm using comparisons of 
## Nearest Neighbor values, between just two of the parameters
## sampled (ad.surv and fecund).  
## **************************************************************##

# Make bivariate plots of fecundity and survival to 
# examine visually the differences between unif
# and LHS sampling.  Note that these values will be the
# same for NoCC and 2M for each RandType
# snpl.100.bivar.df <- rbind( snpl.100.SA2.part$lhs.nocc[ rep.ind, ], 
#                             snpl.100.SA2.part$unif.nocc[ rep.ind, ] )
snpl.100.bivar.df <- rbind( snpl.100.SA1.part$lhs.nocc[ rep.ind, ], 
                           snpl.100.SA1.part$unif.nocc[ rep.ind, ] )
# Make bivariate plots
temp <- snpl.100.bivar.df
temp$RandType <- as.character( temp$RandType )
temp$RandType[ temp$RandType=="Unif" ] <- "URS"
snpl.bv <- ggplot( temp, aes( ad.surv, fecund )) + 
  geom_point() + 
  facet_grid(RandType~RepNumber) +
  xlab("Adult Survival") +
  ylab("Fecundity") +
  scale_x_continuous( breaks=seq( 0.65, 0.75, 0.05 ) ) +
  theme_bw() +
  theme( text=element_text( size=12, family="Times", face="bold") )

print(snpl.bv)
#ggsave( filename="figures/Diss_Fig_2_3.pdf", width=6.5, height=4.5, units="in" )
rm( temp )
## -------------------------------------------------------------------- ##
## Here's the *Nearest Neighbor* analysis for 10 repetitions 
## of the PartNum=100 dataset.
# Make two empty data.frames
snpl.nn.unif <- vector()
snpl.nn.lhs <- vector()

# Select input dimensions to use in nearest neighbor analysis
input.dims <- c(2,10,11,63,64,62,48,61)

for ( Rep in 1:length(reps.toUse) ){
  # Get subsets of the LHS and Unif data
#  snpl.unif.rep <- subset( snpl.100.SA2.part$unif.nocc, subset=RepNumber==reps.toUse[Rep] )
#  snpl.lhs.rep <- subset( snpl.100.SA2.part$lhs.nocc, subset=RepNumber==reps.toUse[Rep] )
    
  snpl.unif.rep <- subset( snpl.100.SA1.part$unif.nocc, subset=RepNumber==reps.toUse[Rep] )
  snpl.lhs.rep <- subset( snpl.100.SA1.part$lhs.nocc, subset=RepNumber==reps.toUse[Rep] )
  ## Calculate distance matrix for all dimensions
  # first convert kch.type to a numeric value
  snpl.unif.rep$kch.type <- as.numeric(snpl.unif.rep$kch.type)
  snpl.lhs.rep$kch.type <- as.numeric(snpl.lhs.rep$kch.type)
  
  # Get only input dims
  snpl.unif.inputs <- cbind(snpl.unif.rep[input.dims])
  snpl.lhs.inputs <- cbind(snpl.lhs.rep[input.dims])
  
  # Scale all inputs to be between 0 and 1 by dividing each 
  # input by the max input value
  snpl.unif.inputs.max <- apply(snpl.unif.inputs,MARGIN=2,FUN=max)
  snpl.lhs.inputs.max <- apply(snpl.lhs.inputs,MARGIN=2,FUN=max)
  # To do the division, use this little trick so the vector 
  # cycling of the max vector lines up correctly with the inputs
  # matrix
  snpl.unif.inputs <- t(t(snpl.unif.inputs)/snpl.unif.inputs.max)
  snpl.lhs.inputs <- t(t(snpl.lhs.inputs)/snpl.lhs.inputs.max)
  
  # Calculate distance matrices
#  dist.unif <- rdist( cbind(snpl.unif.rep$ad.surv, snpl.unif.rep$fecund) )
#  dist.lhs <- rdist( cbind(snpl.lhs.rep$ad.surv, snpl.lhs.rep$fecund) )
  dist.unif <- as.matrix(dist(snpl.unif.inputs))
  dist.lhs <- as.matrix(dist(snpl.lhs.inputs))
  
  # Set diagonal elements to NAs
  diag(dist.unif) <- NA
  diag(dist.lhs) <- NA
  
  # Calc Nearest Neighbor
  near.neigh.unif <- apply( dist.unif, 2, min, na.rm=T )
  near.neigh.lhs <- apply( dist.lhs, 2, min, na.rm=T)
  
  # Return as part of data.frame
  snpl.nn.unif <- rbind( snpl.nn.unif, cbind(near.neigh.unif,rep(reps.toUse[Rep],100)) )
  snpl.nn.lhs <- rbind( snpl.nn.lhs, cbind(near.neigh.lhs,rep(reps.toUse[Rep],100)) )
}
# Combine nearest neighbor data.frames
snpl.nn <- rbind( snpl.nn.unif, snpl.nn.lhs )
snpl.nn <- as.data.frame(snpl.nn)
rand.type <- rep(c("Unif","LHS"),each=500) # Change to 1000 if using 10 reps
#rand.type <- rep(c("Unif","LHS"),each=10000) # Change to 10000 if using all reps
snpl.nn <- cbind( snpl.nn, rand.type )
names(snpl.nn) <- c("near.neighbor","RepNumber","RandType")
snpl.nn$RepNumber <- as.factor( snpl.nn$RepNumber )
# Copy snpl.nn to snpl.nn.100
snpl.nn.100 <- snpl.nn
snpl.nn.100$SampleSize <- 100
# Plot these data
#snpl.nn.hist <- ggplot(aes(x=near.neighbor,fill=RandType),data=snpl.nn) + 
#  geom_histogram(position="identity",alpha=0.5) + 
#  facet_grid(.~RepNumber)
temp <- snpl.nn
temp$RandType <- as.character( temp$RandType )
temp$RandType[ temp$RandType=="Unif" ] <- "URS"
snpl.nn.hist <- ggplot(data=temp ,aes(x=near.neighbor)) + 
  geom_histogram() +
  facet_grid(RandType~RepNumber) +
  xlab( "Nearest Neighbor Distance" ) +
  theme_bw() +
  theme( text=element_text( size=12, family="Times", face="bold") )
  
print(snpl.nn.hist)
rm( temp )

# Make figure of bivariate plots with nearest neighbor
# histograms
#pdf('figures/snpl.100.SA1.unifVlhs.pdf',width=14)
pdf('figures/Diss_Fig_2_3.pdf',width=6.5,height=6.5)
grid.arrange( snpl.bv, snpl.nn.hist )
dev.off()

## Calculate means and standard deviations of the nearest neighbors
snpl.nn.mean.100 <- tapply( snpl.nn.100$near.neighbor, 
                        list( snpl.nn.100$RepNumber, snpl.nn.100$RandType ), 
                        mean )
snpl.nn.mean.100.df <- melt(as.data.frame(snpl.nn.mean.100))
names(snpl.nn.mean.100.df) <- c('samp.type','near.neighbor')
snpl.nn.var.100 <- tapply( snpl.nn.100$near.neighbor, 
                      list( snpl.nn.100$RepNumber, snpl.nn.100$RandType ), 
                      var )
snpl.nn.var.100.df <- melt(as.data.frame(snpl.nn.var.100))
names(snpl.nn.var.100.df) <- c('samp.type','near.neighbor')
## Calculate the coefficient of variation for this sample size
apply(snpl.nn.mean.100,2,mean)
apply(snpl.nn.var.100,2,mean)
apply(snpl.nn.mean.100,2,sd)/apply(snpl.nn.mean.100,2,mean)

## Carry out a  t-test on the means
snpl.nn.t.test <- t.test(x=snpl.nn.mean.100[,1],y=snpl.nn.mean.100[,2])
print(snpl.nn.t.test)

## Carry out an ANOVA
snpl.nn.lm.100 <- lm( near.neighbor~RepNumber * RandType, data=snpl.nn.100)
anova(snpl.nn.lm.100)

## Make a plot
pdf('figures/lhs_unif_100_mean_nearneighbor_hist.pdf')
ggplot(data=snpl.nn.mean.100.df,aes(x=near.neighbor,fill=samp.type)) + 
  geom_histogram(position="identity",alpha=0.5)
dev.off()

## END POINTS 
## And now we want to look at the differences between 
## LHS and Unif for p50 and EMA
# Absolute end points
###p50.hist <- ggplot() + geom_histogram(aes(x=prob.50),data=snpl.100.bivar.df) + facet_grid(RandType~RepNumber)
p50.hist <- ggplot() + geom_histogram(aes(x=prob.50),data=snpl.100.bivar.df) + facet_grid(RandType~RepNumber)
p50.hist <- p50.hist + theme(axis.text.x=element_text(size=18,angle=90,colour="black"))
p50.hist <- p50.hist + scale_x_continuous(breaks=seq(0,1,0.25))
p50.hist

ema.hist <- ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.100.bivar.df) + facet_grid(RandType~RepNumber)
# Relative end points
snpl.100.sampRel.df <- rbind( snpl.100.SA2.part$relEnds.unif[ rep.ind, ], 
                            snpl.100.SA2.part$relEnds.lhs[ rep.ind, ] )
##p50.delta.hist <- ggplot() + geom_histogram(aes(x=p50.delta),data=snpl.100.sampRel.df) + facet_grid(RandType~RepNumber)
p50.delta.hist <- ggplot() + geom_histogram(aes(x=p50.delta),data=snpl.100.sampRel.df) + facet_grid(RandType~RepNumber)
p50.delta.hist <- p50.delta.hist + theme(axis.text.x=element_text(size=18,angle=90,colour="black"))
p50.delta.hist <- p50.delta.hist + scale_x_continuous(breaks=seq(0,.2,0.05))
p50.delta.hist

ema.delta.hist <- ggplot() + geom_histogram(aes(x=ema.delta),data=snpl.100.sampRel.df) + facet_grid(RandType~RepNumber)

grid.arrange( p50.hist, p50.delta.hist )
grid.arrange( ema.hist, ema.delta.hist )

## Make a histogram plot of 2m SLR Prob decline to 50
snpl.100.SLR.df <- rbind( snpl.100.SA2.part$lhs.2m[ rep.ind, ], 
                            snpl.100.SA2.part$unif.2m[ rep.ind, ] )
p50.hist.2mSLR <- ggplot() + geom_histogram(aes(x=prob.50),data=snpl.100.SLR.df) + facet_grid(RandType~RepNumber)
p50.hist.2mSLR <- p50.hist.2mSLR + theme(axis.text.x=element_text(size=18,angle=90,colour="black"))
p50.hist.2mSLR <- p50.hist.2mSLR + scale_x_continuous(breaks=seq(0,1,0.25))
p50.hist.2mSLR

grid.arrange( p50.hist, p50.hist.2mSLR )

## -------------------------------------------------------------------- ##
## -------------------------------------------------------------------- ##
## Here's the *Nearest Neighbor* analysis for 10 repetitions 
## of the PartNum=250 dataset.

# Pick 10 random RepNumbers to work with
reps.toUse <- sample(1:40,10)
# Here I'm setting reps.toUse to a previously generated set of 10 reps
# to be used **specifically** for Part.Num = 100
#reps.toUse <- c(2,10,31,39,54,72,84,88,92,96)
reps.toUse <- 1:40
# Get indices for these reps. Indices will be 
# the same for each partition within PartNum sets
get.rep <- function( RN ){ which( snpl.250.SA1.part$lhs.nocc$RepNumber == RN ) }
rep.ind <- sapply( reps.toUse, get.rep )
rep.ind <- as.vector(rep.ind)

# Make two empty data.frames
snpl.nn.unif <- vector()
snpl.nn.lhs <- vector()

# Select input dimensions to use in nearest neighbor analysis
input.dims <- c(2,10,11,63,64,62,48,61)

for ( Rep in 1:length(reps.toUse) ){
  # Get subsets of the LHS and Unif data
  #  snpl.unif.rep <- subset( snpl.250.SA2.part$unif.nocc, subset=RepNumber==reps.toUse[Rep] )
  #  snpl.lhs.rep <- subset( snpl.250.SA2.part$lhs.nocc, subset=RepNumber==reps.toUse[Rep] )
  
  snpl.unif.rep <- subset( snpl.250.SA1.part$unif.nocc, subset=RepNumber==reps.toUse[Rep] )
  snpl.lhs.rep <- subset( snpl.250.SA1.part$lhs.nocc, subset=RepNumber==reps.toUse[Rep] )
  ## Calculate distance matrix for all dimensions
  # first convert kch.type to a numeric value
  snpl.unif.rep$kch.type <- as.numeric(snpl.unif.rep$kch.type)
  snpl.lhs.rep$kch.type <- as.numeric(snpl.lhs.rep$kch.type)
  
  # Get only input dims
  snpl.unif.inputs <- cbind(snpl.unif.rep[input.dims])
  snpl.lhs.inputs <- cbind(snpl.lhs.rep[input.dims])
  
  # Scale all inputs to be between 0 and 1 by dividing each 
  # input by the max input value
  snpl.unif.inputs.max <- apply(snpl.unif.inputs,MARGIN=2,FUN=max)
  snpl.lhs.inputs.max <- apply(snpl.lhs.inputs,MARGIN=2,FUN=max)
  # To do the division, use this little trick so the vector 
  # cycling of the max vector lines up correctly with the inputs
  # matrix
  snpl.unif.inputs <- t(t(snpl.unif.inputs)/snpl.unif.inputs.max)
  snpl.lhs.inputs <- t(t(snpl.lhs.inputs)/snpl.lhs.inputs.max)
  
  # Calculate distance matrices
  #  dist.unif <- rdist( cbind(snpl.unif.rep$ad.surv, snpl.unif.rep$fecund) )
  #  dist.lhs <- rdist( cbind(snpl.lhs.rep$ad.surv, snpl.lhs.rep$fecund) )
  dist.unif <- as.matrix(dist(snpl.unif.inputs))
  dist.lhs <- as.matrix(dist(snpl.lhs.inputs))
  
  # Set diagonal elements to NAs
  diag(dist.unif) <- NA
  diag(dist.lhs) <- NA
  
  # Calc Nearest Neighbor
  near.neigh.unif <- apply( dist.unif, 2, min, na.rm=T )
  near.neigh.lhs <- apply( dist.lhs, 2, min, na.rm=T)
  
  # Return as part of data.frame
  snpl.nn.unif <- rbind( snpl.nn.unif, cbind(near.neigh.unif,rep(reps.toUse[Rep],250)) )
  snpl.nn.lhs <- rbind( snpl.nn.lhs, cbind(near.neigh.lhs,rep(reps.toUse[Rep],250)) )
}
# Combine nearest neighbor data.frames
snpl.nn <- rbind( snpl.nn.unif, snpl.nn.lhs )
snpl.nn <- as.data.frame(snpl.nn)
#rand.type <- rep(c("Unif","LHS"),each=1000) # Change to 10000 if using all reps
rand.type <- rep(c("Unif","LHS"),each=10000) # Change to 10000 if using all reps
snpl.nn <- cbind( snpl.nn, rand.type )
names(snpl.nn) <- c("near.neighbor","RepNumber","RandType")
snpl.nn$RepNumber <- as.factor( snpl.nn$RepNumber )
# Copy snpl.nn to snpl.nn.250
snpl.nn.250 <- snpl.nn
snpl.nn.250$SampleSize <- 250

## Calculate means and standard deviations of the nearest neighbors
snpl.nn.mean.250 <- tapply( snpl.nn.250$near.neighbor, 
                           list( snpl.nn.250$RepNumber, snpl.nn.250$RandType ), 
                           mean )
snpl.nn.var.250 <- tapply( snpl.nn.250$near.neighbor, 
                          list( snpl.nn.250$RepNumber, snpl.nn.250$RandType ), 
                          var )
snpl.nn.mean.250.df <- melt(as.data.frame(snpl.nn.mean.250))
snpl.nn.var.250.df <- melt(as.data.frame(snpl.nn.var.250))
names(snpl.nn.mean.250.df) <- c('samp.type','near.neighbor')
names(snpl.nn.var.250.df) <- c('samp.type','near.neighbor')
## Calculate the mean nearest neighbor across the 25000 sample sets
apply(snpl.nn.mean.250,2,mean)
apply(snpl.nn.var.250,2,mean)
apply(snpl.nn.mean.250,2,sd)
## Calculate the coefficient of variation for this sample size
apply(snpl.nn.mean.250,2,sd)/apply(snpl.nn.mean.250,2,mean)

## Carry out an ANOVA
snpl.nn.lm.250 <- lm( near.neighbor~RepNumber * RandType, data=snpl.nn.250)
anova(snpl.nn.lm.250)

## -------------------------------------------------------------------- ##
## Here's the *Nearest Neighbor* analysis for 10 repetitions 
## of the PartNum=250 dataset.

# Pick 10 random RepNumbers to work with
reps.toUse <- sample(1:20,10)
# Here I'm setting reps.toUse to a previously generated set of 10 reps
# to be used **specifically** for Part.Num = 100
reps.toUse <- 1:20
# Get indices for these reps. Indices will be 
# the same for each partition within PartNum sets
get.rep <- function( RN ){ which( snpl.500.SA1.part$lhs.nocc$RepNumber == RN ) }
rep.ind <- sapply( reps.toUse, get.rep )
rep.ind <- as.vector(rep.ind)

# Make two empty data.frames
snpl.nn.unif <- vector()
snpl.nn.lhs <- vector()

# Select input dimensions to use in nearest neighbor analysis
input.dims <- c(2,10,11,63,64,62,48,61)

for ( Rep in 1:length(reps.toUse) ){
  # Get subsets of the LHS and Unif data
  #  snpl.unif.rep <- subset( snpl.500.SA2.part$unif.nocc, subset=RepNumber==reps.toUse[Rep] )
  #  snpl.lhs.rep <- subset( snpl.500.SA2.part$lhs.nocc, subset=RepNumber==reps.toUse[Rep] )
  
  snpl.unif.rep <- subset( snpl.500.SA1.part$unif.nocc, subset=RepNumber==reps.toUse[Rep] )
  snpl.lhs.rep <- subset( snpl.500.SA1.part$lhs.nocc, subset=RepNumber==reps.toUse[Rep] )
  ## Calculate distance matrix for all dimensions
  # first convert kch.type to a numeric value
  snpl.unif.rep$kch.type <- as.numeric(snpl.unif.rep$kch.type)
  snpl.lhs.rep$kch.type <- as.numeric(snpl.lhs.rep$kch.type)
  
  # Get only input dims
  snpl.unif.inputs <- cbind(snpl.unif.rep[input.dims])
  snpl.lhs.inputs <- cbind(snpl.lhs.rep[input.dims])
  
  # Scale all inputs to be between 0 and 1 by dividing each 
  # input by the max input value
  snpl.unif.inputs.max <- apply(snpl.unif.inputs,MARGIN=2,FUN=max)
  snpl.lhs.inputs.max <- apply(snpl.lhs.inputs,MARGIN=2,FUN=max)
  # To do the division, use this little trick so the vector 
  # cycling of the max vector lines up correctly with the inputs
  # matrix
  snpl.unif.inputs <- t(t(snpl.unif.inputs)/snpl.unif.inputs.max)
  snpl.lhs.inputs <- t(t(snpl.lhs.inputs)/snpl.lhs.inputs.max)
  
  # Calculate distance matrices
  #  dist.unif <- rdist( cbind(snpl.unif.rep$ad.surv, snpl.unif.rep$fecund) )
  #  dist.lhs <- rdist( cbind(snpl.lhs.rep$ad.surv, snpl.lhs.rep$fecund) )
  dist.unif <- as.matrix(dist(snpl.unif.inputs))
  dist.lhs <- as.matrix(dist(snpl.lhs.inputs))
  
  # Set diagonal elements to NAs
  diag(dist.unif) <- NA
  diag(dist.lhs) <- NA
  
  # Calc Nearest Neighbor
  near.neigh.unif <- apply( dist.unif, 2, min, na.rm=T )
  near.neigh.lhs <- apply( dist.lhs, 2, min, na.rm=T)
  
  # Return as part of data.frame
  snpl.nn.unif <- rbind( snpl.nn.unif, cbind(near.neigh.unif,rep(reps.toUse[Rep],500)) )
  snpl.nn.lhs <- rbind( snpl.nn.lhs, cbind(near.neigh.lhs,rep(reps.toUse[Rep],500)) )
}
# Combine nearest neighbor data.frames
snpl.nn <- rbind( snpl.nn.unif, snpl.nn.lhs )
snpl.nn <- as.data.frame(snpl.nn)
#rand.type <- rep(c("Unif","LHS"),each=1000) # Change to 10000 if using all reps
rand.type <- rep(c("Unif","LHS"),each=10000) # Change to 10000 if using all reps
snpl.nn <- cbind( snpl.nn, rand.type )
names(snpl.nn) <- c("near.neighbor","RepNumber","RandType")
snpl.nn$RepNumber <- as.factor( snpl.nn$RepNumber )
# Copy snpl.nn to snpl.nn.250
snpl.nn.500 <- snpl.nn
snpl.nn.500$SampleSize <- 500

## Calculate means and standard deviations of the nearest neighbors
snpl.nn.mean.500 <- tapply( snpl.nn.500$near.neighbor, 
                           list( snpl.nn.500$RepNumber, snpl.nn.500$RandType ), 
                           mean )
snpl.nn.var.500 <- tapply( snpl.nn.500$near.neighbor, 
                          list( snpl.nn.500$RepNumber, snpl.nn.500$RandType ), 
                          var )
snpl.nn.mean.500.df <- melt(as.data.frame(snpl.nn.mean.500))
snpl.nn.var.500.df <- melt(as.data.frame(snpl.nn.var.500))
names(snpl.nn.mean.500.df) <- c('samp.type','near.neighbor')
names(snpl.nn.var.500.df) <- c('samp.type','near.neighbor')
## Calculate the mean nearest neighbor across the 50000 sample sets
apply(snpl.nn.mean.500,2,mean)
apply(snpl.nn.var.500,2,mean)
apply(snpl.nn.mean.500,2,sd)
## Calculate the coefficient of variation for this sample size
apply(snpl.nn.mean.500,2,sd)/apply(snpl.nn.mean.500,2,mean)

## Carry out an ANOVA
snpl.nn.lm.500 <- lm( near.neighbor~RepNumber * RandType, data=snpl.nn.500)
anova(snpl.nn.lm.500)

## **************************************************************##

## Work with the PartNum=1000 case
# Use all ten Reps

## Below is code to examine differences in 'evenness' of 
## sampling in paramter space for the two sampling methods,
## Uinf and LHS.  I'm using comparisons of 
## Nearest Neighbor values, between just two of the parameters
## sampled (ad.surv and fecund).  

# Make bivariate plots of fecundity and survival to 
# examine visually the differences between unif
# and LHS sampling.  Note that these values will be the
# same for NoCC and 2M for each RandType
#snpl.1000.nocc.df <- rbind( snpl.1000.SA2.part$lhs.nocc, 
#                            snpl.1000.SA2.part$unif.nocc)
snpl.1000.nocc.df <- rbind( snpl.1000.SA1.part$lhs.nocc, 
                            snpl.1000.SA1.part$unif.nocc)

# Make bivariate plots
snpl.1000.bv <- ggplot( snpl.1000.nocc.df, aes( ad.surv, fecund )) + geom_point() + facet_grid(RandType~RepNumber)

## Here's the *Nearest Neighbor* analysis for all 10 repetitions 
## of the PartNum=1000 dataset.
# Over-writes work from above - Make two empty data.frames
snpl.nn.unif <- vector()
snpl.nn.lhs <- vector()
for ( Rep in 1:10 ){
  # Get subsets of the LHS and Unif data
#  snpl.unif.rep <- subset( snpl.1000.SA2.part$unif.nocc, subset=RepNumber==Rep )
#  snpl.lhs.rep <- subset( snpl.1000.SA2.part$lhs.nocc, subset=RepNumber==Rep )

  snpl.unif.rep <- subset( snpl.1000.SA1.part$unif.nocc, subset=RepNumber==Rep )
  snpl.lhs.rep <- subset( snpl.1000.SA1.part$lhs.nocc, subset=RepNumber==Rep )

  ## Calculate distance matrix for all dimensions
  # first convert kch.type to a numeric value
  snpl.unif.rep$kch.type <- as.numeric(snpl.unif.rep$kch.type)
  snpl.lhs.rep$kch.type <- as.numeric(snpl.lhs.rep$kch.type)
  
  # Get only input dims
  snpl.unif.inputs <- cbind(snpl.unif.rep[input.dims])
  snpl.lhs.inputs <- cbind(snpl.lhs.rep[input.dims])
  
  # Scale all inputs to be between 0 and 1 by dividing each 
  # input by the max input value
  snpl.unif.inputs.max <- apply(snpl.unif.inputs,MARGIN=2,FUN=max)
  snpl.lhs.inputs.max <- apply(snpl.lhs.inputs,MARGIN=2,FUN=max)
  # To do the division, use this little trick so the vector 
  # cycling of the max vector lines up correctly with the inputs
  # matrix
  snpl.unif.inputs <- t(t(snpl.unif.inputs)/snpl.unif.inputs.max)
  snpl.lhs.inputs <- t(t(snpl.lhs.inputs)/snpl.lhs.inputs.max)
  
  # Calculate distance matrices
  #  dist.unif <- rdist( cbind(snpl.unif.rep$ad.surv, snpl.unif.rep$fecund) )
  #  dist.lhs <- rdist( cbind(snpl.lhs.rep$ad.surv, snpl.lhs.rep$fecund) )
  dist.unif <- as.matrix(dist(snpl.unif.inputs))
  dist.lhs <- as.matrix(dist(snpl.lhs.inputs))

  # Set diagonal elements to NAs
  diag(dist.unif) <- NA
  diag(dist.lhs) <- NA
  
  # Calc Nearest Neighbor
  near.neigh.unif <- apply( dist.unif, 2, min, na.rm=T )
  near.neigh.lhs <- apply( dist.lhs, 2, min, na.rm=T)
  
  # Return as part of data.frame
  snpl.nn.unif <- rbind( snpl.nn.unif, cbind(near.neigh.unif,rep(Rep,1000)) )
  snpl.nn.lhs <- rbind( snpl.nn.lhs, cbind(near.neigh.lhs,rep(Rep,1000)) )
}
# Combine nearest neighbor data.frames
snpl.nn <- rbind( snpl.nn.unif, snpl.nn.lhs )
snpl.nn <- as.data.frame(snpl.nn)
rand.type <- rep(c("Unif","LHS"),each=10000)
snpl.nn <- cbind( snpl.nn, rand.type )
names(snpl.nn) <- c("near.neighbor","RepNumber","RandType")
snpl.nn$RepNumber <- as.factor( snpl.nn$RepNumber )
# Copy snpl.nn to snpl.nn.1000
snpl.nn.1000 <- snpl.nn
snpl.nn.1000$SampleSize <- 1000

# Plot these data
snpl.nn.hist <- ggplot() + geom_histogram(aes(x=near.neighbor),data=snpl.nn) + facet_grid(RandType~RepNumber)

# Make figure of bivariate plots with nearest neighbor
# histograms
pdf('figures/snpl.1000.SA1.unifVlhs.pdf',width=14)
#pdf('figures/snpl.1000.SA2.unifVlhs.pdf',width=14)
grid.arrange( snpl.1000.bv, snpl.nn.hist )
dev.off()

## Calculate means and standard deviations of the nearest neighbors
snpl.nn.mean.1000 <- tapply( snpl.nn.1000$near.neighbor, 
                        list( snpl.nn.1000$RepNumber, snpl.nn.1000$RandType ), 
                        mean )
snpl.nn.mean.1000.df <- melt(as.data.frame(snpl.nn.mean.1000))
names(snpl.nn.mean.1000.df) <- c('samp.type','near.neighbor')
snpl.nn.var.1000 <- tapply( snpl.nn.1000$near.neighbor, 
                      list( snpl.nn.1000$RepNumber, snpl.nn.1000$RandType ), 
                      var )
## Calculate the coefficient of variation for this sample size
apply(snpl.nn.mean.1000,2,sd)/apply(snpl.nn.mean.1000,2,mean)
apply(snpl.nn.var.1000,2,mean)
apply(snpl.nn.mean.1000,2,mean)

## Carry out a  t-test on the means
snpl.nn.t.test.1000 <- t.test(x=snpl.nn.mean.1000[,1],y=snpl.nn.mean.1000[,2])
print(snpl.nn.t.test.1000)
## Carry out an ANOVA
snpl.nn.lm.1000 <- lm( near.neighbor~RepNumber * RandType, data=snpl.nn.1000)
anova(snpl.nn.lm.1000)

## Make a plot
pdf('figures/lhs_unif_1000_mean_nearneighbor_hist.pdf')
ggplot(data=snpl.nn.mean.1000.df,aes(x=near.neighbor,fill=samp.type)) + 
  geom_histogram(position="identity",alpha=0.5)
dev.off()


## END POINTS 
## And now we want to look at the differences between 
## LHS and Unif for p50 and EMA
# Absolute end points
p50.hist <- ggplot() + geom_histogram(aes(x=prob.50),data=snpl.1000.nocc.df) + facet_grid(RandType~RepNumber)
ema.hist <- ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.1000.nocc.df) + facet_grid(RandType~RepNumber)
# Relative end points
snpl.1000.sampRel.df <- rbind( snpl.1000.SA2.part$relEnds.unif, 
                            snpl.1000.SA2.part$relEnds.lhs)
p50.delta.hist <- ggplot() + geom_histogram(aes(x=p50.delta),data=snpl.1000.sampRel.df) + facet_grid(RandType~RepNumber)
ema.delta.hist <- ggplot() + geom_histogram(aes(x=ema.delta),data=snpl.1000.sampRel.df) + facet_grid(RandType~RepNumber)

grid.arrange( p50.hist, p50.delta.hist )
grid.arrange( ema.hist, ema.delta.hist )

## ******************************************************************** ##
## Perform and ANOVA examing all sample sizes simultaneousl
## ******************************************************************** ##
#summary(snpl.nn.lm.10)
snpl.nn.anova.10 <- anova(snpl.nn.lm.10)
# Get Correct F-ratio and P val for RandType
F_nn_10 <- snpl.nn.anova.10$'Mean Sq'[2]/snpl.nn.anova.10$'Mean Sq'[3]
1-pf(F_nn_10,1,999)

snpl.nn.anova.100 <- anova(snpl.nn.lm.100)
# Get Correct F-ratio and P val for RandType
F_nn_100 <- snpl.nn.anova.100$'Mean Sq'[2]/snpl.nn.anova.100$'Mean Sq'[3]
1-pf(F_nn_100,1,99)

snpl.nn.anova.250 <- anova(snpl.nn.lm.250)
# Get Correct F-ratio and P val for RandType
F_nn_250 <- snpl.nn.anova.250$'Mean Sq'[2]/snpl.nn.anova.250$'Mean Sq'[3]
1-pf(F_nn_250,1,39)

snpl.nn.anova.500 <- anova(snpl.nn.lm.500)
snpl.nn.anova.500
# Get Correct F-ratio and P val for RandType
F_nn_500 <- snpl.nn.anova.500$'Mean Sq'[2]/snpl.nn.anova.500$'Mean Sq'[3]
1-pf(F_nn_500,1,19)

snpl.nn.anova.1000 <- anova(snpl.nn.lm.1000)
snpl.nn.anova.1000
F_nn_1000 <- snpl.nn.anova.1000$'Mean Sq'[2]/snpl.nn.anova.1000$'Mean Sq'[3]
F_nn_1000
1-pf(F_nn_1000,1,9)

snpl.nn <- rbind( snpl.nn.10, snpl.nn.100, snpl.nn.250,
                  snpl.nn.500, snpl.nn.1000 )

snpl.nn.lm <- lm( near.neighbor~RandType*factor(SampleSize), data=snpl.nn )
anova( snpl.nn.lm )
#TukeyHSD( aov( near.neighbor~RandType*factor(SampleSize), data=snpl.nn ) )

## **************************************************************##
## Examining variable interaction effects
## **************************************************************##
##
## WARNING: This section requires access the BRT_Models directory,
## which is currently on the Akcakaya-Lab Computer **or**
## AKCAKAYA2 external drive

## I'm interested in looking at interactions between variables
## and comparing across different PartNum scenarios

## Some notes about brt saved models
## - Naming Scheme snpl.[PartNum].[RandType.x.SLR.Ind].[RepNumber].RData
## - [RandType.x.SLR.Ind] values:
##   - 1 = lhs.nocc
##   - 2 = lhs.2m
##   - 3 = unif.nocc
##   - 4 = unif.2m
##   - 5 = relEnds.lhs
##   - 6 = relEnds.unif
## - Every model has the same name - snpl.sub.brt

## Examining interactions for 10k relEnds SLR scenario, endpoint = p50
load('C:/Projects/SnowyPlover/BRT_Models/snpl.10k.SA2.5.1.RData')
snpl.10k.SA2.relEnds.lhs.1 <- snpl.sub.brt
snpl.sub <- subset( snpl.10k.SA2.part$relEnds.lhs, subset=RepNumber==1 )
find.interactions <- gbm.interactions(snpl.10k.SA2.relEnds.lhs.1)
find.interactions$rank.list
fig.file <- paste(sa.sens.dir,'figures/BRT_Interactions/',
                  'snpl.10K.SA2.p50.lhs.pdf',sep="")
pdf(fig.file)
gbm.perspec(snpl.10k.SA2.relEnds.lhs.1,5,4,z.range=c(0.03,0.1), main='Part Num = 10k, Rand = LHS')
dev.off()

load('C:/Projects/SnowyPlover/BRT_Models/snpl.10k.SA2.6.1.RData')
snpl.10k.SA2.relEnds.unif.1 <- snpl.sub.brt
snpl.sub <- subset( snpl.10k.SA2.part$relEnds.unif, subset=RepNumber==1 )
find.interactions <- gbm.interactions(snpl.10k.SA2.relEnds.lhs.1)
find.interactions$rank.list
fig.file <- paste(sa.sens.dir,'figures/BRT_Interactions/',
                  'snpl.10K.SA2.p50.unif.pdf',sep="")
pdf(fig.file)
gbm.perspec(snpl.10k.SA2.relEnds.unif.1,5,4,z.range=c(0.03,0.1), main='Part Num = 10k, Rand = Unif')
dev.off()

## ----------------------------------------------------------------#
## Make a char vector of the file paths to the BRT models for the 
## ten models selected for the PartNum = 100 scenario, endpoint = p50

## relEnds SLR scenario, endpoit = p50
brt.base.lhs <- 'snpl.100.SA2.p50.5.'
brt.base.unif <- 'snpl.100.SA2.p50.6.'

brt.files.lhs <- paste( brt.dir, brt.base.lhs, reps.toUse, '.Rdata',sep="")
brt.files.unif <- paste( brt.dir, brt.base.unif, reps.toUse, '.Rdata',sep="")

## Loop through `brt.files.lhs` list
for ( brt.f in 1:length(brt.files.lhs) ){
  load( brt.files.lhs[brt.f])
  snpl.sub <- subset( snpl.100.SA2.part$relEnds.lhs, subset=RepNumber==reps.toUse[brt.f] )
  # Name file and open alternative output device
  fig.file <- paste(sa.sens.dir,'figures/BRT_Interactions/',
                    'snpl.100.SA2.p50.lhs.',reps.toUse[brt.f],'.pdf',sep="")
  pdf(fig.file)
  # Plot interactions between ad.survival and fecundity
  gbm.perspec( snpl.sub.brt, 5, 4, z.range=c(0.03,0.1), 
               main=paste('Part Num = 100, Rand = LHS, RepNum =', reps.toUse[brt.f] ) )
  # Turn off alternative device
  dev.off()
}

## Loop through `brt.files.unif` list
for ( brt.f in 1:length(brt.files.unif) ){
  load( brt.files.unif[brt.f])
  snpl.sub <- subset( snpl.100.SA2.part$relEnds.unif, subset=RepNumber==reps.toUse[brt.f] )
  # Name file and open alternative output device
  fig.file <- paste(sa.sens.dir,'figures/BRT_Interactions/',
                    'snpl.100.SA2.p50.unif.',reps.toUse[brt.f],'.pdf',sep="")
  pdf(fig.file)
  # Plot interactions between ad.survival and fecundity
  gbm.perspec( snpl.sub.brt, 5, 4, z.range=c(0.03,0.1),
               main=paste('Part Num = 100, Rand = Unif, RepNum =', reps.toUse[brt.f] ) )
  # Turn off alternative device
  dev.off()
}

## ----------------------------------------------------------------#
## Make a char vector of the file paths to the BRT models for all 
## ten models for the PartNum = 1000 scenario, endpoint = p50
## relEnds SLR scenario, endpoit = p50

reps.toUse <- 1:10

brt.base.lhs <- 'snpl.1000.SA2.p50.5.'
brt.base.unif <- 'snpl.1000.SA2.p50.6.'

brt.files.lhs <- paste( brt.dir, brt.base.lhs, reps.toUse, '.Rdata',sep="")
brt.files.unif <- paste( brt.dir, brt.base.unif, reps.toUse, '.Rdata',sep="")

## Loop through `brt.files.lhs` list
for ( brt.f in 1:length(brt.files.lhs) ){
  load( brt.files.lhs[brt.f])
  snpl.sub <- subset( snpl.1000.SA2.part$relEnds.lhs, subset=RepNumber==reps.toUse[brt.f] )
  # Name file and open alternative output device
  fig.file <- paste(sa.sens.dir,'figures/BRT_Interactions/',
                    'snpl.1000.SA2.p50.lhs.',reps.toUse[brt.f],'.pdf',sep="")
  pdf(fig.file)
  # Plot interactions between ad.survival and fecundity
  gbm.perspec( snpl.sub.brt, 5, 4, z.range=c(0.03,0.1), 
               main=paste('Part Num = 1000, Rand = LHS, RepNum =', reps.toUse[brt.f] ) )
  # Turn off alternative device
  dev.off()
}

## Loop through `brt.files.unif` list
for ( brt.f in 1:length(brt.files.unif) ){
  load( brt.files.unif[brt.f])
  snpl.sub <- subset( snpl.1000.SA2.part$relEnds.unif, subset=RepNumber==reps.toUse[brt.f] )
  # Name file and open alternative output device
  fig.file <- paste(sa.sens.dir,'figures/BRT_Interactions/',
                    'snpl.1000.SA2.p50.unif.',reps.toUse[brt.f],'.pdf',sep="")
  pdf(fig.file)
  # Plot interactions between ad.survival and fecundity
  gbm.perspec( snpl.sub.brt, 5, 4, z.range=c(0.03,0.1),
               main=paste('Part Num = 1000, Rand = Unif, RepNum =', reps.toUse[brt.f] ) )
  # Turn off alternative device
  dev.off()
}

## ----------------------------------------------------------------#
## Make a char vector of the file paths to the BRT models for all 
## ten models for the PartNum = 250 scenario, endpoint = p50
## relEnds SLR scenario, endpoit = p50

## First work with the PartNum=250 case
# Pick 10 random RepNumbers to work with
reps.toUse <- sample(1:40,10)
# Get indices for these reps. Indices will be 
# the same for each partition within PartNum sets
get.rep <- function( RN ){ which( snpl.250.SA2.part$lhs.nocc$RepNumber == RN ) }
rep.ind <- sapply( reps.toUse, get.rep )
rep.ind <- as.vector(rep.ind)

brt.base.lhs <- 'snpl.250.SA2.p50.5.'
brt.base.unif <- 'snpl.250.SA2.p50.6.'

brt.files.lhs <- paste( brt.dir, brt.base.lhs, reps.toUse, '.Rdata',sep="")
brt.files.unif <- paste( brt.dir, brt.base.unif, reps.toUse, '.Rdata',sep="")

## Loop through `brt.files.lhs` list
for ( brt.f in 1:length(brt.files.lhs) ){
  load( brt.files.lhs[brt.f])
  snpl.sub <- subset( snpl.250.SA2.part$relEnds.lhs, subset=RepNumber==reps.toUse[brt.f] )
  # Name file and open alternative output device
  fig.file <- paste(sa.sens.dir,'figures/BRT_Interactions/',
                    'snpl.250.SA2.p50.lhs.',reps.toUse[brt.f],'.pdf',sep="")
  pdf(fig.file)
  # Plot interactions between ad.survival and fecundity
  gbm.perspec( snpl.sub.brt, 5, 4, z.range=c(0.03,0.1), 
               main=paste('Part Num = 250, Rand = LHS, RepNum =', reps.toUse[brt.f] ) )
  # Turn off alternative device
  dev.off()
}

## Loop through `brt.files.unif` list
for ( brt.f in 1:length(brt.files.unif) ){
  load( brt.files.unif[brt.f])
  snpl.sub <- subset( snpl.250.SA2.part$relEnds.unif, subset=RepNumber==reps.toUse[brt.f] )
  # Name file and open alternative output device
  fig.file <- paste(sa.sens.dir,'figures/BRT_Interactions/',
                    'snpl.250.SA2.p50.unif.',reps.toUse[brt.f],'.pdf',sep="")
  pdf(fig.file)
  # Plot interactions between ad.survival and fecundity
  gbm.perspec( snpl.sub.brt, 5, 4, z.range=c(0.03,0.1),
               main=paste('Part Num = 250, Rand = Unif, RepNum =', reps.toUse[brt.f] ) )
  # Turn off alternative device
  dev.off()
}

## ----------------------------------------------------------------#
## Using the saved 'brt.interactions' information from the Results
## R data objects, look at the 'rank.list' of interactions

## Need to define a few functions for this

## Used within an lapply on one of the results objects, this essentially
## creates a new nested list. The new list's first level is scenario and 
## the second level is the group replicates
get.interaction <- function(scenario) {
  lapply( scenario$brt.interactions, function(brt.int){(brt.int$rank.list)} )
}

snpl.100.brt.int.ranklist <- lapply( snpl.100.brt.results, get.interaction )

# Still in development
# I'm thinking that I can compare the upper triangluar part
# of the `interactions` matrix. Here is some sample code with 
# how this might work.

# First get the matrices
snpl.10k.lhs.nocc.interactions <- snpl.10k.brt.results$lhs.nocc$brt.interactions[[1]]$interactions
snpl.100.lhs.nocc.interactions <- snpl.100.brt.results$lhs.nocc$brt.interactions[[1]]$interactions

# Next get the upper triangular part only
snpl.10k <- snpl.10k.lhs.nocc.interactions[ upper.tri(snpl.10k.lhs.nocc.interactions) ]
snpl.100 <- snpl.100.lhs.nocc.interactions[ upper.tri(snpl.100.lhs.nocc.interactions) ]

# Now get a correlation coefficient
cor(snpl.10k, snpl.100, method="spearman")

# I might also want to know what row,column values I'm getting 
# when I look at the upper triangle values - I can do this with 
# the following commands.
mat.indices <- which(upper.tri(snpl.10k.lhs.nocc.interactions),arr.ind=T)

# Other ideas to condiser, extracting the first value of the rank.list
# from all of the rank.list data.frames
# Or maybe to calculate the interaction values for some
# parameter combination, regardless of whether it is 
# a min/max value (I think I did this for the 
# fecund x ad survival already)

## **************************************************************##
## Examine Relative Influence values for 10k scenario
## **************************************************************##

## ----------------------------------------------------------------#
## Make a `data.frame` for 10k results, both for SA1 and SA2
## This data.frame can be used to plot the relative influence 
## values for all of the scenarios

## Make a function to create this data.frame for any of the 10k results
make.10k.brt.summary.df <- function( snpl.brt.results ){  
  scenario.names <- names( snpl.brt.results )
  # Change scenario names to make visualization and anlaysis easier
  # later
  scenario.names[5:6] <- c("lhs.relEnds","unif.relEnds")
  # Make a  vector classifing scenarios as abs or rel end
  abs.rel.vector <- ifelse( grepl( pattern="rel", scenario.names ), yes="rel", no="abs" )
  # Make a vector for the slr scenario ("nocc" or "2m")
  slr.vector <- ifelse( grepl( pattern="nocc", scenario.names ), yes="nocc", no="2m" )
  # Make a vector for randtype value (ie lhs vs unif)
  randtype.vec <- ifelse( grepl( pattern="lhs", scenario.names ), yes="lhs", no="unif")
  # Create a new list object to store all rel.inf data.frames
  brt.summ.df.complete <- vector( mode='list', length=0 )
  # Loop through list of lists of results
  for ( scenario.res in 1:length( snpl.brt.results ) ) {
    # Get the data.frame for brt.summary
    brt.summ.list <- as.data.frame( snpl.brt.results[[ scenario.res ]]$brt.summary )
    # Add scenario name as a column to this df
    scenario=rep(scenario.names[ scenario.res ],length(brt.summ.list[1]))
    # Add rand type
    randtype <- rep(randtype.vec[ scenario.res ], length( brt.summ.list[1] ))
    # Add SLR scenario
    slr <- rep(slr.vector[ scenario.res ], length( brt.summ.list[1] ))
    # Add abs.rel.vector value
    endPoint <- rep(abs.rel.vector[ scenario.res ], length( brt.summ.list[1] ))
    # Combine columns
    brt.summ.list <- cbind( scenario, randtype, slr, endPoint, brt.summ.list)
    brt.summ.df.complete <- rbind( brt.summ.df.complete,
                                   brt.summ.list )
  }
  return(brt.summ.df.complete)
}

## **************************************************************#

load('Results/snpl.10k.brt.results.RData') # SA1 -- p50
# Loads `snpl.10k.brt.results`
brt.10k.p50.summ.df.complete <- make.10k.brt.summary.df( snpl.10k.brt.results )

## Plot absolute endpoints and delta endpoints on separate plots
# Determine which rows in the data.frame are `relEnds`
relEnds <- which( grepl( pattern="relEnds", x=brt.10k.p50.summ.df.complete$scenario ) )
brt.10k.p50.summ.df.delta <- brt.10k.p50.summ.df.complete[ relEnds, ]
## Make a data.frame with absolute endpoints only (ie no relative)
brt.10k.p50.summ.df.absolute <- brt.10k.p50.summ.df.complete[ -relEnds, ]

# # Order the results based on increase of relative importance for 
# # absolute endpoints
# brt.10k.p50.summ.df.absolute <- 
#   brt.10k.p50.summ.df.absolute[ order(brt.10k.p50.summ.df.absolute$rel.inf,decreasing=F),]
# var.order <- unique( brt.10k.p50.summ.df.absolute$var )
# brt.10k.p50.summ.df.absolute$var <- factor( brt.10k.p50.summ.df.absolute$var, var.order )
# 
# # Order the delta values using the absolute order too
# brt.10k.p50.summ.df.delta$var <- factor( brt.10k.p50.summ.df.delta$var, var.order )

## ----------------------------------------------------------------#
## Save a grid arranged version of these plots to file
# pdf('figures/relative_influence_10k_p50.pdf',width=14)
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# pushViewport(viewport(layout = grid.layout(1, 2)))
## ----------------------------------------------------------------#

## Write a function that changes variable names
snpl_rename_vars <- function( vars ){
  vars <- ifelse( vars=="fecund", yes="Fecundity", no=vars )
  vars <- ifelse( vars=="ad.surv", yes="Adult Surv.", no=vars )
  vars <- ifelse( vars=="surv.stdev.avg", yes="Std. Dev. Surv.", no=vars )
  vars <- ifelse( vars=="fec.stdev.avg", yes="Std. Dev. Fec.", no=vars )
  vars <- ifelse( vars=="kch.type", yes="Carrying Capacity", no=vars )
  vars <- ifelse( vars=="metapop.initab", yes="Initial Abundance", no=vars )
  vars <- ifelse( vars=="avg.corr.dist.b", yes="Inter pop. Correlation", no=vars )
  vars <- ifelse( vars=="mean.t0.disp.rate", yes="Dispersal", no=vars )
}

## Add the full name columns
brt.10k.p50.summ.df.absolute$var_full <-
  sapply( as.character(brt.10k.p50.summ.df.absolute$var), snpl_rename_vars )

# Order the results based on increase of relative importance for 
# absolute endpoints
brt.10k.p50.summ.df.absolute <- 
  brt.10k.p50.summ.df.absolute[ order(brt.10k.p50.summ.df.absolute$rel.inf,decreasing=F),]
var.order <- unique( brt.10k.p50.summ.df.absolute$var_full )
brt.10k.p50.summ.df.absolute$var_full <- factor( brt.10k.p50.summ.df.absolute$var_full, var.order )

# Order the delta values using the absolute order too
brt.10k.p50.summ.df.delta$var_full <-
  sapply( as.character( brt.10k.p50.summ.df.delta$var ), snpl_rename_vars )
brt.10k.p50.summ.df.delta$var_full <- factor( brt.10k.p50.summ.df.delta$var_full, var.order )


## Make abs.p50 a bar plot for the manuscript
temp <- brt.10k.p50.summ.df.absolute
temp$randtype <- as.character( temp$randtype )
temp$randtype[ temp$randtype=="lhs" ] <- "LHS"
temp$randtype[ temp$randtype=="unif" ] <- "URS"

plot.abs.p50 <- 
  ggplot( temp, aes(var_full, weight=rel.inf, fill=slr)) +
  geom_bar( aes( order = var), position="dodge") + scale_fill_grey() + 
  xlab("Parameter") + 
  ylab("Relative Influence") + 
  ylim(0,80) +
  coord_flip() +
  facet_grid( randtype~. ) + 
  theme_bw() +
  theme( text=element_text( size=12, family="Times"),
         legend.position=c(.85,.65) )

rm(temp)  

# 
# # Use `theme_bw()` but modify the location of the legent
# theme_abs <- theme_set( theme_bw() )
# theme_abs <- theme_update( legend.position=c(.85,.65) )
# plot.abs.p50 <- plot.abs.p50 + coord_flip() #+ theme_bw()
# plot.abs.p50 <- plot.abs.p50 + facet_grid(randtype~.)
# print(plot.abs.p50, vp=vplayout(1,1))
#plot.abs.p50

## ----------------------------------------------------------------#
## Make delta.p50 a bar plot for the manuscript
temp <- brt.10k.p50.summ.df.delta
temp$randtype <- as.character( temp$randtype )
temp$randtype[ temp$randtype=="lhs" ] <- "LHS"
temp$randtype[ temp$randtype=="unif" ] <- "URS"

plot.delta.p50 <- 
  ggplot( temp, aes(var_full, weight=rel.inf)) +
  geom_bar(position="dodge") + 
  scale_fill_grey() +
  ylab("Relative Influence") + 
  ylim(0,80) + 
  coord_flip() +
  facet_grid( randtype~. ) +
  #annotate( "text", y=70, x=0, label="B") +
  theme_bw() +
  theme( text=element_text( size=12, family="Times"),
         axis.title.y=element_blank(),
         axis.text.y=element_blank() )
rm(temp)

pdf( file="figures/Diss_Fig_2_4.pdf", width=9, height=5.5 )
grid.arrange( plot.abs.p50, plot.delta.p50, ncol=2, nrow=1, widths=c(1.3, 1) )
dev.off()

# print(plot.delta.p50, vp=vplayout(1,2))
#plot.delta.p50
## ----------------------------------------------------------------#



#dev.off()

## ----------------------------------------------------------------#
## Make bar plot for a Power Point Presentation
plot.abs.p50 <- ggplot( brt.10k.p50.summ.df.absolute, aes(var,weight=rel.inf,fill=scenario)) 
plot.abs.p50 <- plot.abs.p50 + geom_bar(position="dodge") + scale_fill_brewer(palette=1) 
plot.abs.p50 <- plot.abs.p50 + xlab("Parameter") + ylab("Relative Influence") 
plot.abs.p50 <- plot.abs.p50+theme(axis.text.x=element_text(angle=90,vjust=0.5,size=28,colour="black",face="bold")) 
plot.abs.p50 <- plot.abs.p50+theme(axis.title.x=element_text(size=32,face="bold"))
plot.abs.p50 <- plot.abs.p50+theme(axis.text.y=element_text(size=24,colour="black",face="bold"),
                                   axis.title.y=element_text(size=32,face="bold"))
par.names <- unique( brt.10k.p50.summ.df.complete$var )
par.names.new <- c("Fecundity", "Adult Survival", "Var. Ad. Surv.",
                   "Var. Fec.", "Carrying Capacity", "Spatial Corr.",
                   "Dispersal", "Init. Abund.")
plot.abs.p50 <- plot.abs.p50 + scale_x_discrete(breaks=par.names, labels=par.names.new)
plot.abs.p50 <- plot.abs.p50 + theme(legend.title=element_text(size=28),
                                     legend.text=element_text(size=24))
plot.abs.p50  
## ----------------------------------------------------------------#

## ******************************************************************** ##
## Work with EMA results
## ******************************************************************** ##

## **************************************************************#
load('Results/snpl.10k.ema.brt.results.RData') # SA1 -- EMA
## WARNING ## This overwrites the previous `snpl.10k.brt.results`
# Loads `snpl.10k.brt.results`
brt.10k.ema.summ.df.complete <- make.10k.brt.summary.df( snpl.10k.brt.results )

## Plot absolute endpoints and delta endpoints on separate plots
# Determine which rows in the data.frame are `relEnds`
relEnds <- which( grepl( pattern="relEnds", x=brt.10k.ema.summ.df.complete$scenario ) )
brt.10k.ema.summ.df.delta <- brt.10k.ema.summ.df.complete[ relEnds, ]
## Make a data.frame with absolute endpoints only (ie no relative)
brt.10k.ema.summ.df.absolute <- brt.10k.ema.summ.df.complete[ -relEnds, ]

# # Order the results based on increase of relative importance for 
# # absolute endpoints
# brt.10k.ema.summ.df.absolute <- 
#   brt.10k.ema.summ.df.absolute[ order(brt.10k.ema.summ.df.absolute$rel.inf,decreasing=F),]
# var.order <- unique( brt.10k.ema.summ.df.absolute$var )
# brt.10k.ema.summ.df.absolute$var <- factor( brt.10k.ema.summ.df.absolute$var, var.order )
# 
# # Order the delta values using the absolute order too
# brt.10k.ema.summ.df.delta$var <- factor( brt.10k.ema.summ.df.delta$var, var.order )

## ----------------------------------------------------------------#
## Save a grid arranged version of these plots to file
# pdf('figures/relative_influence_10k_ema.pdf',width=14)
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# pushViewport(viewport(layout = grid.layout(1, 2)))

## ----------------------------------------------------------------#
## Make abs.ema a bar plot for the manuscript

## Add the full name columns
brt.10k.ema.summ.df.absolute$var_full <-
  sapply( as.character(brt.10k.ema.summ.df.absolute$var), snpl_rename_vars )

# Order the results based on increase of relative importance for 
# absolute endpoints
brt.10k.ema.summ.df.absolute <- 
  brt.10k.ema.summ.df.absolute[ order(brt.10k.ema.summ.df.absolute$rel.inf,decreasing=F),]
var.order <- unique( brt.10k.ema.summ.df.absolute$var_full )
brt.10k.ema.summ.df.absolute$var_full <- factor( brt.10k.ema.summ.df.absolute$var_full, var.order )

# Order the delta values using the absolute order too
brt.10k.ema.summ.df.delta$var_full <-
  sapply( as.character( brt.10k.ema.summ.df.delta$var ), snpl_rename_vars )
brt.10k.ema.summ.df.delta$var_full <- factor( brt.10k.ema.summ.df.delta$var_full, var.order )


## Make abs.ema a bar plot for the manuscript
temp <- brt.10k.ema.summ.df.absolute
temp$randtype <- as.character( temp$randtype )
temp$randtype[ temp$randtype=="lhs" ] <- "LHS"
temp$randtype[ temp$randtype=="unif" ] <- "URS"

plot.abs.ema <- 
  ggplot( temp, aes(var_full, weight=rel.inf, fill=slr)) +
  geom_bar( aes( order = var), position="dodge") + scale_fill_grey() + 
  xlab("Parameter") + 
  ylab("Relative Influence") + 
  ylim(0,70) +
  coord_flip() +
  facet_grid( randtype~. ) + 
  theme_bw() +
  theme( text=element_text( size=12, family="Times"),
         legend.position=c(.85,.65) )

rm(temp)  


## ----------------------------------------------------------------#
## Make delta.ema a bar plot for the manuscript
temp <- brt.10k.ema.summ.df.delta
temp$randtype <- as.character( temp$randtype )
temp$randtype[ temp$randtype=="lhs" ] <- "LHS"
temp$randtype[ temp$randtype=="unif" ] <- "URS"

plot.delta.ema <- 
  ggplot( temp, aes(var_full, weight=rel.inf)) +
  geom_bar(position="dodge") + 
  scale_fill_grey() +
  ylab("Relative Influence") + 
  ylim(0,70) + 
  coord_flip() +
  facet_grid( randtype~. ) +
  #annotate( "text", y=70, x=0, label="B") +
  theme_bw() +
  theme( text=element_text( size=12, family="Times"),
         axis.title.y=element_blank(),
         axis.text.y=element_blank() )
rm(temp)

pdf( file="figures/Diss_Fig_2_5.pdf", width=9, height=5.5 )
grid.arrange( plot.abs.ema, plot.delta.ema, ncol=2, nrow=1, widths=c(1.3, 1) )
dev.off()


# plot.abs.ema <- ggplot( brt.10k.ema.summ.df.absolute, aes(var,weight=rel.inf,fill=slr)) 
# plot.abs.ema <- plot.abs.ema + geom_bar(position="dodge") + scale_fill_grey() 
# plot.abs.ema <- plot.abs.ema + xlab("Parameter") + ylab("Relative Influence") + ylim(0,70)
# # Use `theme_bw()` but modify the location of the legent
# theme_abs <- theme_set( theme_bw() )
# theme_abs <- theme_update( legend.position=c(.85,.65) )
# plot.abs.ema <- plot.abs.ema + coord_flip() #+ theme_bw()
# plot.abs.ema <- plot.abs.ema + facet_grid(randtype~.)
# print(plot.abs.ema, vp=vplayout(1,1))
# #plot.abs.ema
# ## ----------------------------------------------------------------#
# ## Make delta.ema a bar plot for the manuscript
# plot.delta.ema <- ggplot( brt.10k.ema.summ.df.delta, aes(var,weight=rel.inf)) 
# plot.delta.ema <- plot.delta.ema + geom_bar(position="dodge") + scale_fill_grey() 
# plot.delta.ema <- plot.delta.ema + ylab("Relative Influence") + ylim(0,70)
# # Use `theme_bw()` but modify to remove axis labels
# theme_new <- theme_set(theme_bw())
# theme_new <- theme_update( axis.title.y=element_blank(),
#                            axis.text.y=element_blank() )
# plot.delta.ema <- plot.delta.ema + coord_flip() 
# plot.delta.ema <- plot.delta.ema + facet_grid(randtype~.)
# print(plot.delta.ema, vp=vplayout(1,2))
# #plot.delta.ema
# ## ----------------------------------------------------------------#
# dev.off()

## ******************************************************************** ##
## ******************************************************************** ##
## Begin working with SA2 results
## ******************************************************************** ##
## ******************************************************************** ##

## **************************************************************#
load('Results/snpl.10k.SA2.p50.brt.results.RData') # SA2 -- p50
# Loads `snpl.10k.SA2.p50.brt.results`
brt.10k.SA2.p50.summ.df.complete <- make.10k.brt.summary.df( snpl.10k.SA2.p50.brt.results )

## Plot absolute endpoints and delta endpoints on separate plots
# Determine which rows in the data.frame are `relEnds`
relEnds <- which( grepl( pattern="relEnds", x=brt.10k.SA2.p50.summ.df.complete$scenario ) )
brt.10k.SA2.p50.summ.df.delta <- brt.10k.SA2.p50.summ.df.complete[ relEnds, ]
## Make a data.frame with absolute endpoints only (ie no relative)
brt.10k.SA2.p50.summ.df.absolute <- brt.10k.SA2.p50.summ.df.complete[ -relEnds, ]
# Order the results based on increase of relative importance for 
# absolute endpoints
brt.10k.SA2.p50.summ.df.absolute <- 
  brt.10k.SA2.p50.summ.df.absolute[ order(brt.10k.SA2.p50.summ.df.absolute$rel.inf,decreasing=F),]
var.order <- unique( brt.10k.SA2.p50.summ.df.absolute$var )
brt.10k.SA2.p50.summ.df.absolute$var <- factor( brt.10k.SA2.p50.summ.df.absolute$var, var.order )
# Order the delta values using the absolute order too
brt.10k.SA2.p50.summ.df.delta$var <- factor( brt.10k.SA2.p50.summ.df.delta$var, var.order )

## -------------------------------------------------------------------- ##


## Add the full name columns
brt.10k.SA2.p50.summ.df.absolute$var_full <-
  sapply( as.character(brt.10k.SA2.p50.summ.df.absolute$var), snpl_rename_vars )

# Order the results based on increase of relative importance for 
# absolute endpoints
brt.10k.SA2.p50.summ.df.absolute <- 
  brt.10k.SA2.p50.summ.df.absolute[ order(brt.10k.SA2.p50.summ.df.absolute$rel.inf,decreasing=F),]
var.order <- unique( brt.10k.SA2.p50.summ.df.absolute$var_full )
brt.10k.SA2.p50.summ.df.absolute$var_full <- factor( brt.10k.SA2.p50.summ.df.absolute$var_full, var.order )

# Order the delta values using the absolute order too
brt.10k.SA2.p50.summ.df.delta$var_full <-
  sapply( as.character( brt.10k.SA2.p50.summ.df.delta$var ), snpl_rename_vars )
brt.10k.SA2.p50.summ.df.delta$var_full <- factor( brt.10k.SA2.p50.summ.df.delta$var_full, var.order )

## Make abs.p50 a bar plot for the manuscript
temp <- brt.10k.SA2.p50.summ.df.absolute
temp$randtype <- as.character( temp$randtype )
temp$randtype[ temp$randtype=="lhs" ] <- "LHS"
temp$randtype[ temp$randtype=="unif" ] <- "URS"

plot.abs.SA2.p50 <- 
  ggplot( temp, aes(var_full, weight=rel.inf, fill=slr)) +
  geom_bar( aes( order = var), position="dodge") + scale_fill_grey() + 
  xlab("Parameter") + 
  ylab("Relative Influence") + 
  ylim(0,40) +
  coord_flip() +
  facet_grid( randtype~. ) + 
  theme_bw() +
  theme( text=element_text( size=12, family="Times"),
         legend.position=c(.85,.65) )

rm(temp)  

# 
# # Use `theme_bw()` but modify the location of the legent
# theme_abs <- theme_set( theme_bw() )
# theme_abs <- theme_update( legend.position=c(.85,.65) )
# plot.abs.p50 <- plot.abs.p50 + coord_flip() #+ theme_bw()
# plot.abs.p50 <- plot.abs.p50 + facet_grid(randtype~.)
# print(plot.abs.p50, vp=vplayout(1,1))
#plot.abs.p50

## ----------------------------------------------------------------#
## Make delta.p50 a bar plot for the manuscript
temp <- brt.10k.SA2.p50.summ.df.delta
temp$randtype <- as.character( temp$randtype )
temp$randtype[ temp$randtype=="lhs" ] <- "LHS"
temp$randtype[ temp$randtype=="unif" ] <- "URS"

plot.delta.SA2.p50 <- 
  ggplot( temp, aes(var_full, weight=rel.inf)) +
  geom_bar(position="dodge") + 
  scale_fill_grey() +
  ylab("Relative Influence") + 
  ylim(0,40) + 
  coord_flip() +
  facet_grid( randtype~. ) +
  #annotate( "text", y=70, x=0, label="B") +
  theme_bw() +
  theme( text=element_text( size=12, family="Times"),
         axis.title.y=element_blank(),
         axis.text.y=element_blank() )
rm(temp)

pdf( file="figures/Diss_Fig_2_8.pdf", width=9, height=5.5 )
grid.arrange( plot.abs.SA2.p50, plot.delta.SA2.p50, ncol=2, nrow=1, widths=c(1.3, 1) )
dev.off()

# print(plot.delta.p50, vp=vplayout(1,2))
#plot.delta.p50
## ----------------------------------------------------------------#









# ## ----------------------------------------------------------------#
# ## Save a grid arranged version of these plots to file
# pdf('figures/relative_influence_10k_SA2_p50.pdf',width=14)
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# pushViewport(viewport(layout = grid.layout(1, 2)))
## ----------------------------------------------------------------#
# ## Make abs.SA2.p50 a bar plot for the manuscript
# plot.abs.SA2.p50 <- ggplot( brt.10k.SA2.p50.summ.df.absolute, aes(var,weight=rel.inf,fill=slr)) 
# plot.abs.SA2.p50 <- plot.abs.SA2.p50 + geom_bar(position="dodge") + scale_fill_grey() 
# plot.abs.SA2.p50 <- plot.abs.SA2.p50 + xlab("Parameter") + ylab("Relative Influence") + ylim(0,40)
# # Use `theme_bw()` but modify the location of the legent
# theme_abs <- theme_set( theme_bw() )
# theme_abs <- theme_update( legend.position=c(.85,.65) )
# plot.abs.SA2.p50 <- plot.abs.SA2.p50 + coord_flip() #+ theme_bw()
# plot.abs.SA2.p50 <- plot.abs.SA2.p50 + facet_grid(randtype~.)
# print(plot.abs.SA2.p50, vp=vplayout(1,1))
# #plot.abs.SA2.p50
# ## ----------------------------------------------------------------#
# ## Make delta.SA2.p50 a bar plot for the manuscript
# plot.delta.SA2.p50 <- ggplot( brt.10k.SA2.p50.summ.df.delta, aes(var,weight=rel.inf)) 
# plot.delta.SA2.p50 <- plot.delta.SA2.p50 + geom_bar(position="dodge") + scale_fill_grey() 
# plot.delta.SA2.p50 <- plot.delta.SA2.p50 + ylab("Relative Influence") + ylim(0,40)
# # Use `theme_bw()` but modify to remove axis labels
# theme_new <- theme_set(theme_bw())
# theme_new <- theme_update( axis.title.y=element_blank(),
#                            axis.text.y=element_blank() )
# plot.delta.SA2.p50 <- plot.delta.SA2.p50 + coord_flip() 
# plot.delta.SA2.p50 <- plot.delta.SA2.p50 + facet_grid(randtype~.)
# print(plot.delta.SA2.p50, vp=vplayout(1,2))
# #plot.delta.SA2.p50
# ## ----------------------------------------------------------------#
# dev.off()

## ----------------------------------------------------------------#
# Make a plot with absolute endpoints only (ie no relative) 
# for a Power Point Presentation
relEnds <- which( grepl( pattern="relEnds", x=brt.10k.SA2.p50.summ.df.complete$scenario ) )
brt.10k.SA2.p50.summ.df.absolute <- brt.10k.SA2.p50.summ.df.complete[ -relEnds, ]
brt.10k.SA2.p50.summ.df.absolute <- brt.10k.SA2.p50.summ.df.absolute[ order(brt.10k.SA2.p50.summ.df.absolute$rel.inf,decreasing=T),]
var.order <- unique( brt.10k.SA2.p50.summ.df.absolute$var )
brt.10k.SA2.p50.summ.df.absolute$var <- factor( brt.10k.SA2.p50.summ.df.absolute$var, var.order )
plot.SA2.p50.abs <- ggplot( brt.10k.SA2.p50.summ.df.absolute, aes(var,weight=rel.inf,fill=scenario)) 
plot.SA2.p50.abs <- plot.SA2.p50.abs + geom_bar(position="dodge") + scale_fill_brewer(palette=1) + xlab("Parameter") + ylab("Relative Influence") 
plot.SA2.p50.abs <- plot.SA2.p50.abs+theme(axis.text.x=element_text(angle=90,vjust=0.5,size=28,colour="black",face="bold")) 
plot.SA2.p50.abs <- plot.SA2.p50.abs+theme(axis.title.x=element_text(size=32,face="bold"))
plot.SA2.p50.abs <- plot.SA2.p50.abs+theme(axis.text.y=element_text(size=24,colour="black",face="bold"),
                   axis.title.y=element_text(size=32,face="bold"))
par.names <- unique( brt.10k.SA2.p50.summ.df.complete$var )
par.names.new <- c("Fecundity", "Adult Survival", "Var. Ad. Surv.",
                   "Var. Fec.", "Carrying Capacity", "Spatial Corr.",
                   "Dispersal", "Init. Abund.")
plot.SA2.p50.abs <- plot.SA2.p50.abs + scale_x_discrete(breaks=par.names, labels=par.names.new)
plot.SA2.p50.abs <- plot.SA2.p50.abs + theme(legend.title=element_text(size=28),
                     legend.text=element_text(size=24))
plot.SA2.p50.abs  
## ----------------------------------------------------------------#

## **************************************************************#

load('Results/snpl.10k.SA2.ema.brt.results.RData') # SA2 -- EMA
# Loads `snpl.10k.SA2.brt.results.ema`
brt.10k.SA2.ema.summ.df.complete <- make.10k.brt.summary.df( snpl.10k.SA2.brt.results.ema )

## Plot absolute endpoints and delta endpoints on separate plots
# Determine which rows in the data.frame are `relEnds`
relEnds <- which( grepl( pattern="relEnds", x=brt.10k.SA2.ema.summ.df.complete$scenario ) )
brt.10k.SA2.ema.summ.df.delta <- brt.10k.SA2.ema.summ.df.complete[ relEnds, ]
## Make a data.frame with absolute endpoints only (ie no relative)
brt.10k.SA2.ema.summ.df.absolute <- brt.10k.SA2.ema.summ.df.complete[ -relEnds, ]
# Order the results based on increase of relative importance for 
# absolute endpoints
brt.10k.SA2.ema.summ.df.absolute <- 
  brt.10k.SA2.ema.summ.df.absolute[ order(brt.10k.SA2.ema.summ.df.absolute$rel.inf,decreasing=F),]
var.order <- unique( brt.10k.SA2.ema.summ.df.absolute$var )
brt.10k.SA2.ema.summ.df.absolute$var <- factor( brt.10k.SA2.ema.summ.df.absolute$var, var.order )
# Order the delta values using the absolute order too
brt.10k.SA2.ema.summ.df.delta$var <- factor( brt.10k.SA2.ema.summ.df.delta$var, var.order )



## Add the full name columns
brt.10k.SA2.ema.summ.df.absolute$var_full <-
  sapply( as.character(brt.10k.SA2.ema.summ.df.absolute$var), snpl_rename_vars )

# Order the results based on increase of relative importance for 
# absolute endpoints
brt.10k.SA2.ema.summ.df.absolute <- 
  brt.10k.SA2.ema.summ.df.absolute[ order(brt.10k.SA2.ema.summ.df.absolute$rel.inf,decreasing=F),]
var.order <- unique( brt.10k.SA2.ema.summ.df.absolute$var_full )
brt.10k.SA2.ema.summ.df.absolute$var_full <- factor( brt.10k.SA2.ema.summ.df.absolute$var_full, var.order )

# Order the delta values using the absolute order too
brt.10k.SA2.ema.summ.df.delta$var_full <-
  sapply( as.character( brt.10k.SA2.ema.summ.df.delta$var ), snpl_rename_vars )
brt.10k.SA2.ema.summ.df.delta$var_full <- factor( brt.10k.SA2.ema.summ.df.delta$var_full, var.order )

## Make abs.ema a bar plot for the manuscript
temp <- brt.10k.SA2.ema.summ.df.absolute
temp$randtype <- as.character( temp$randtype )
temp$randtype[ temp$randtype=="lhs" ] <- "LHS"
temp$randtype[ temp$randtype=="unif" ] <- "URS"

plot.abs.SA2.ema <- 
  ggplot( temp, aes(var_full, weight=rel.inf, fill=slr)) +
  geom_bar( aes( order = var), position="dodge") + scale_fill_grey() + 
  xlab("Parameter") + 
  ylab("Relative Influence") + 
  ylim(0,35) +
  coord_flip() +
  facet_grid( randtype~. ) + 
  theme_bw() +
  theme( text=element_text( size=12, family="Times"),
         legend.position=c(.85,.65) )

rm(temp)  

## ----------------------------------------------------------------#
## Make delta.ema a bar plot for the manuscript
temp <- brt.10k.SA2.ema.summ.df.delta
temp$randtype <- as.character( temp$randtype )
temp$randtype[ temp$randtype=="lhs" ] <- "LHS"
temp$randtype[ temp$randtype=="unif" ] <- "URS"

plot.delta.SA2.ema <- 
  ggplot( temp, aes(var_full, weight=rel.inf)) +
  geom_bar(position="dodge") + 
  scale_fill_grey() +
  ylab("Relative Influence") + 
  ylim(0,35) + 
  coord_flip() +
  facet_grid( randtype~. ) +
  #annotate( "text", y=70, x=0, label="B") +
  theme_bw() +
  theme( text=element_text( size=12, family="Times"),
         axis.title.y=element_blank(),
         axis.text.y=element_blank() )
rm(temp)

pdf( file="figures/Diss_Fig_2_9.pdf", width=9, height=5.5 )
grid.arrange( plot.abs.SA2.ema, plot.delta.SA2.ema, ncol=2, nrow=1, widths=c(1.3, 1) )
dev.off()



# ## ----------------------------------------------------------------#
# ## Save a grid arranged version of these plots to file
# pdf('figures/relative_influence_10k_SA2_ema.pdf',width=14)
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# pushViewport(viewport(layout = grid.layout(1, 2)))
# ## ----------------------------------------------------------------#
# ## Make abs.SA2.ema a bar plot for the manuscript
# plot.abs.SA2.ema <- ggplot( brt.10k.SA2.ema.summ.df.absolute, aes(var,weight=rel.inf,fill=slr)) 
# plot.abs.SA2.ema <- plot.abs.SA2.ema + geom_bar(position="dodge") + scale_fill_grey() 
# plot.abs.SA2.ema <- plot.abs.SA2.ema + xlab("Parameter") + ylab("Relative Influence") + ylim(0,35)
# # Use `theme_bw()` but modify the location of the legent
# theme_abs <- theme_set( theme_bw() )
# theme_abs <- theme_update( legend.position=c(.85,.65) )
# plot.abs.SA2.ema <- plot.abs.SA2.ema + coord_flip() #+ theme_bw()
# plot.abs.SA2.ema <- plot.abs.SA2.ema + facet_grid(randtype~.)
# print(plot.abs.SA2.ema, vp=vplayout(1,1))
# #plot.abs.SA2.ema
# ## ----------------------------------------------------------------#
# ## Make delta.SA2.ema a bar plot for the manuscript
# plot.delta.SA2.ema <- ggplot( brt.10k.SA2.ema.summ.df.delta, aes(var,weight=rel.inf)) 
# plot.delta.SA2.ema <- plot.delta.SA2.ema + geom_bar(position="dodge") + scale_fill_grey() 
# plot.delta.SA2.ema <- plot.delta.SA2.ema + ylab("Relative Influence") + ylim(0,35)
# # Use `theme_bw()` but modify to remove axis labels
# theme_new <- theme_set(theme_bw())
# theme_new <- theme_update( axis.title.y=element_blank(),
#                            axis.text.y=element_blank() )
# plot.delta.SA2.ema <- plot.delta.SA2.ema + coord_flip() 
# plot.delta.SA2.ema <- plot.delta.SA2.ema + facet_grid(randtype~.)
# print(plot.delta.SA2.ema, vp=vplayout(1,2))
# #plot.delta.SA2.ema
# ## ----------------------------------------------------------------#
# dev.off()




## **************************************************************##
## Create the distribution of delta P50 in the *unmatched* scenario
## **************************************************************##

## Assuming that SA2 10k data is already read in

## Endpoint = Probability of decline to 50 "prob.50"
lhs.nocc.prob.50 <- snpl.10k.SA2.part$lhs.nocc$prob.50
lhs.2m.prob.50 <- snpl.10k.SA2.part$lhs.2m$prob.50
mean.prob.50.diff <- mean(snpl.10k.SA2.part$lhs.2m$prob.50) - 
  mean(snpl.10k.SA2.part$lhs.nocc$prob.50)

# ##Assuming that SA2 10k data is already read in
# ## Endpoint = Probability of decline to 50 "prob.50"
# lhs.nocc.prob.50 <- snpl.10k.SA1.part$lhs.nocc$prob.50
# lhs.2m.prob.50 <- snpl.10k.SA1.part$lhs.2m$prob.50
# mean.prob.50.diff <- mean(snpl.10k.SA1.part$lhs.2m$prob.50) - 
#   mean(snpl.10k.SA1.part$lhs.nocc$prob.50)

# Make a data frame with these values
slr.scenario.vect <- rep( c('nocc','2m'), each=length(lhs.nocc.prob.50) )
lhs.prob.50.df <- data.frame( slr=slr.scenario.vect, prob.50=c(lhs.nocc.prob.50,lhs.2m.prob.50) )

# Make figures
#pdf('figures/p50.nocc.2m.dens.means.pdf')
prob.50.hist <- 
  ggplot(data=lhs.prob.50.df, aes(x=prob.50,fill=slr)) +
  aes(y=..density..) + theme_bw() +
  geom_histogram(position="identity",alpha=0.8,binwidth=0.01) +
  geom_vline(aes(xintercept = mean(lhs.nocc.prob.50))) +
  geom_vline(aes(xintercept = mean(lhs.2m.prob.50))) +
  xlab("Probability of decline to 50 individuals") +
  ylab("Density") + 
  scale_fill_manual( values=c("grey20","grey70"),
                     name="SLR", 
                     breaks=c("2m","nocc"),
                     labels=c("2m SLR", "No SLR") ) +
  theme_bw() +
  theme( text=element_text( size=12, family="Times") )
print(prob.50.hist)
ggsave( filename="figures/Diss_Fig_2_12.pdf", width=6.5, height=6.5, units="in" )

#dev.off()

# Test to see if these results are statistically significantly
# different
prob.50.t.test <- 
  t.test(lhs.nocc.prob.50,lhs.2m.prob.50)
## Somewhat surprisingly to me, these differences are significant,
## though I wonder how much of that is driven by the sample size

# hist(lhs.nocc.prob.50, freq=F, col="skyblue", breaks=50 , 
#      xlab='', ylab='', main = '')
# abline(v=mean(lhs.nocc.prob.50),lwd=5,col="salmon")
# hist(lhs.2m.prob.50, freq=F, col="royalblue", breaks=50, add=T, 
#      density=65,border='black')
# abline(v=mean(lhs.2m.prob.50),lwd=5,col='red')

## Bootstrap:
## Make two vectors of random draws from the "prob.50" values for the nocc 
## and 2m cases
lhs.nocc.prob.50.Rand <- sample(snpl.10k.SA2.part$lhs.nocc$prob.50, 
                                size=100000, replace=TRUE )
lhs.2m.prob.50.Rand <- sample(snpl.10k.SA2.part$lhs.2m$prob.50,
                              size=100000, replace=TRUE)

delta.p50.bootstrap <- lhs.2m.prob.50.Rand - lhs.nocc.prob.50.Rand
# Calcualte the mean delta value
mean.diff.boot <- mean(delta.p50.bootstrap)

# Make a data.frame for plots that has the bootstrapped and 
# paired delta.p50.values
dp50.boot.col <- rep('Unpaired',length(delta.p50.bootstrap))
delta.p50.bootstrap.df <- data.frame( del.p50.method=dp50.boot.col, 
                                      delta.p50=delta.p50.bootstrap)
dp50.pair.col <- rep('Paired',length(snpl.10k.SA2.part$relEnds.lhs$p50.delta))
delta.p50.paired.df <- data.frame( del.p50.method=dp50.pair.col,
                                   delta.p50=snpl.10k.SA2.part$relEnds.lhs$p50.delta)
delta.p50.comp.df <- rbind(delta.p50.bootstrap.df,delta.p50.paired.df)
delta.p50.comp.df$del.p50.method <- as.factor( delta.p50.comp.df$del.p50.method )

# Get the mean of paired delta p50 values
mean(snpl.10k.SA2.part$relEnds.lhs$p50.delta)
# Get the range for these values
range(snpl.10k.SA2.part$relEnds.lhs$p50.delta)
# How many paired delta p50 values are less than 0?
length(which(snpl.10k.SA2.part$relEnds.lhs$p50.delta<=0))
# How many *unparied* delta p50 values are less than 0?
length(which(delta.p50.bootstrap<=0))

## Make figure with ggplot2
#pdf('figures/Delta.p50.Distributions.pdf')
delta.p50.comp.plot <- 
  ggplot( delta.p50.comp.df, aes(x=delta.p50, fill=del.p50.method) ) + 
  aes(y=..density..) + theme_bw() +
  geom_histogram(position="identity",alpha=0.8,binwidth=0.01) +
  geom_vline(aes(xintercept = (mean.diff.boot))) +
  xlab( expression( paste( Delta, "Probability of decline to 50 individuals") ) ) +
  ylab("Density") + 
  scale_fill_manual( values=c("grey20","grey70"),
                     name=expression( paste( Delta, "P50 Calc. Method" ) ) ) + 
  theme_bw() +
  theme( text=element_text( size=12, family="Times") )

print(delta.p50.comp.plot)
#dev.off()

ggsave( filename="figures/Diss_Fig_2_13.pdf", width=6.5, height=6.5, units="in" )



png('figures/Delta.p50.Distributions.png')
hist(delta.p50.bootstrap, freq=FALSE, ## breaks=40, col="blue", xlab='', ylab='', main = '') ## Used to make figure for a presentation
     #ylim=c(0,14),
     breaks=40,col="blue" , 
     xlab='', ylab='', main = '',cex.axis=2.5)
##abline( v=mean(delta.p50.bootstrap), lwd=5 )
hist( snpl.10k.SA2.part$relEnds.lhs$p50.delta, freq=FALSE, ##col="red") 
                             add=TRUE, col="red" )
abline(v=(mean(snpl.10k.SA2.part$lhs.2m$prob.50) - mean(snpl.10k.SA2.part$lhs.nocc$prob.50)),
       lwd=3)
dev.off()

## Endpoint = EMA

## Make two vectors of random draws from the "prob.50" values for the nocc 
## and 2m cases
lhs.nocc.ema.Rand <- sample(snpl.10k.SA2.part$lhs.nocc$exp.min.n, 
                                size=100000, replace=TRUE )
lhs.2m.ema.Rand <- sample(snpl.10k.SA2.part$lhs.2m$exp.min.n,
                              size=100000, replace=TRUE)

delta.ema.bootstrap <- lhs.2m.ema.Rand - lhs.nocc.ema.Rand

png('figures/Delta.EMA.Distributions.png')
hist(delta.ema.bootstrap, freq=FALSE,ylim=c(0,0.04),breaks=40,col="blue")
hist( snpl.10k.SA2.part$relEnds.lhs$ema.delta, freq=FALSE, ##col="red") 
                             add=TRUE, col="red" )
abline(v=(mean(snpl.10k.SA2.part$lhs.2m$exp.min.n) - mean(snpl.10k.SA2.part$lhs.nocc$exp.min.n)),
       lwd=3)
dev.off()


## Make figures for SA1 10k
## ************************

## Read in SA1 10k data
snpl.10k.rep.SA1 <- read.csv('Results/snpl.10000.rep.csv')
snpl.10k.SA1.part <- snpl.df.partition( snpl.10k.rep.SA1 )

## Endpoint = Probability of decline to 50 "prob.50"
lhs.nocc.prob.50 <- snpl.10k.SA1.part$lhs.nocc$prob.50
lhs.2m.prob.50 <- snpl.10k.SA1.part$lhs.2m$prob.50

# Make figures
hist(lhs.nocc.prob.50, freq=F, col="skyblue", breaks=50 , 
     xlab='', ylab='', main = '')
abline(v=mean(lhs.nocc.prob.50),lwd=5,col="salmon")
hist(lhs.2m.prob.50, freq=F, col="royalblue", breaks=50, add=T, 
     density=65,border='black')
abline(v=mean(lhs.2m.prob.50),lwd=5,col='red')

## Bootstrap:
## Make two vectors of random draws from the "prob.50" values for the nocc 
## and 2m cases
lhs.nocc.prob.50.Rand <- sample(snpl.10k.SA1.part$lhs.nocc$prob.50, 
                                size=100000, replace=TRUE )
lhs.2m.prob.50.Rand <- sample(snpl.10k.SA1.part$lhs.2m$prob.50,
                              size=100000, replace=TRUE)

delta.p50.bootstrap <- lhs.2m.prob.50.Rand - lhs.nocc.prob.50.Rand

png('figures/Delta.p50.Distributions.png')
hist(delta.p50.bootstrap, freq=FALSE,  breaks=40, col="blue") #, xlab='', ylab='', main = '') ## Used to make figure for a presentation
                               ylim=c(0,18),breaks=40,col="blue" , xlab='', ylab='', main = '')
abline( v=mean(delta.p50.bootstrap), lwd=5 )
hist( snpl.10k.SA1.part$relEnds.lhs$p50.delta, freq=FALSE, ##col="red") 
                             add=TRUE, col="red" )
abline(v=(mean(snpl.10k.SA1.part$lhs.2m$prob.50) - mean(snpl.10k.SA1.part$lhs.nocc$prob.50)),
       lwd=5)
dev.off()

## Endpoint = EMA

## Make two vectors of random draws from the "prob.50" values for the nocc 
## and 2m cases
lhs.nocc.ema.Rand <- sample(snpl.10k.SA2.part$lhs.nocc$exp.min.n, 
                                size=100000, replace=TRUE )
lhs.2m.ema.Rand <- sample(snpl.10k.SA2.part$lhs.2m$exp.min.n,
                              size=100000, replace=TRUE)

delta.ema.bootstrap <- lhs.2m.ema.Rand - lhs.nocc.ema.Rand

png('figures/Delta.EMA.Distributions.png')
hist(delta.ema.bootstrap, freq=FALSE,ylim=c(0,0.04),breaks=40,col="blue")
hist( snpl.10k.SA2.part$relEnds.lhs$ema.delta, freq=FALSE, ##col="red") 
                             add=TRUE, col="red" )
abline(v=(mean(snpl.10k.SA2.part$lhs.2m$exp.min.n) - mean(snpl.10k.SA2.part$lhs.nocc$exp.min.n)),
       lwd=3)
dev.off()



## **************************************************************##
## Deveopment work
## **************************************************************##

## Examine the gbm.interactions results, as calculated
## during run time of the gbm analysis

# Load reference scenario results (ie 10k)
load( 'Results/snpl.10k.SA2.p50.brt.results.RData')
# Load GBM results
load('Results/snpl.SA2.p50.brt.results.RData')

# Get the element names for the xxx.results lists - should be the
# same for all Part.Num. cases
scenario.names <- names( snpl.10k.SA2.p50.brt.results )
# Change scenario names to make visualization and anlaysis easier
# later
scenario.names[5:6] <- c("lhs.relEnds","unif.relEnds")
# Make a  vector classifing scenarios as abs or rel end
abs.rel.vector <- ifelse( grepl( pattern="rel", scenario.names ), yes="rel", no="abs" )
# Make a vector for the slr scenario ("nocc" or "2m")
slr.vector <- ifelse( grepl( pattern="nocc", scenario.names ), yes="nocc", no="2m" )

## Functions used to extract the brt interaction values for
## interaction between fecundity and adult survival
# Get element [4,5] of the interaction matrix
extract.int <- function( Int ) { return( Int$interactions[4,5])}
# Apply extract.int across all rand.type x slr scenarios for a given PartNum scenario
use.extract.int <- function( Scenario ) { unlist( lapply( Scenario$brt.interactions, extract.int ) )}
# Apply use.extract.int across all PartNum scenarios
part.num.extract.int <- function( PartNum.Scen ) { lapply( PartNum.Scen, use.extract.int ) }

# Aplly the part.num.extract.in to the whole snpl.brt.results.p50
# This results in list of brt.interactions from each scenario
snpl.brt.interactions.p50 <- lapply( snpl.brt.results.p50, part.num.extract.int )
# Change names of this list elements
names(snpl.brt.interactions.p50$snpl.1000.SA2) <- scenario.names
names(snpl.brt.interactions.p50$snpl.500.SA2) <- scenario.names
names(snpl.brt.interactions.p50$snpl.250.SA2) <- scenario.names
names(snpl.brt.interactions.p50$snpl.100.SA2) <- scenario.names

# Melt this list into a data.frame
snpl.brt.interactions.p50.df <- melt(snpl.brt.interactions.p50)
int.df_split <- strsplit( snpl.brt.interactions.p50.df$L2, split="\\." )
snpl.brt.interactions.p50.df <- cbind( snpl.brt.interactions.p50.df, do.call( rbind, int.df_split ) )
# Assign some column names
names( snpl.brt.interactions.p50.df ) <- c( "ad.surv.x.fec.interaction", "samp.slr", "part.num",
                                  "samp","slr" )
# Set the order of the part.num scenarios
snpl.brt.interactions.p50.df$part.num <- factor( snpl.brt.interactions.p50.df$part.num, 
                                       c('snpl.100.SA2','snpl.250.SA2','snpl.500.SA2','snpl.1000.SA2') )

# Lets plot this
ggplot( snpl.brt.interactions.p50.df, aes(samp,ad.surv.x.fec.interaction)) + geom_boxplot() + facet_grid( slr~part.num, scales='free_y' )


## **************************************************************##
## Looking at variances of relative influence values??

load( 'Results/snpl.10k.SA2.p50.brt.results.RData')
## Define the importance values that we are considering the
## references (ie TRUE) values.  In nearly all scenarios, this
## should be one of the Part.Num. = 10000 scenarios. Currently
## they are defined as the 'lhs' results
# Relative end-point metrics
brt.summary.relEnds.ref <- brt.summary.ord( snpl.10k.SA2.p50.brt.results$relEnds.lhs$brt.summary[[1]] )
# Absolute end-point metrics - 2M SLR
brt.summary.absEnds.slr.ref <- brt.summary.ord( snpl.10k.SA2.p50.brt.results$lhs.2m$brt.summary[[1]] )
# Absolute end-point metrics - NoCC
brt.summary.absEnds.nocc.ref <- brt.summary.ord( snpl.10k.SA2.p50.brt.results$lhs.nocc$brt.summary[[1]] )
brt.summary.relEnds.ref
brt.summary.absEnds.slr.ref
var(brt.summary.absEnds.slr.ref$rel.inf)
var(brt.summary.relEnds.ref$rel.inf)


## **************************************************************##
## Making a 3d plot for visualization

require(scatterplot3d)
snpl.100.rep.SA1 <- read.csv('Results/snpl.100.rep.csv')
snpl.100.SA1.part <- snpl.df.partition( snpl.100.rep.SA1 )
snpl <- subset(snpl.100.SA1.part$lhs.nocc, snpl.100.SA1.part$lhs.nocc$RepNumber==1)
#pdf( 'figures/InputParameterSpace.pdf',width=7.5,height=7.5, family="Times", pointsize=12 )
pdf( 'figures/Diss_Fig_2_1.pdf',width=7.5,height=7.5, family="Times", pointsize=12 )
scatterplot3d(snpl$fecund,snpl$ad.surv,snpl$avg.corr.dist.b,
              main="Input Parameter Space",
              xlab="Fecundity",
              ylab="Adult Survival",
              zlab="Inter-Population Corr",
              pch=20)
dev.off()