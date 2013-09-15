# Work log for SNPL Sensitivity Project
# Author: Matthew Aiello-Lammens
# Begin date: 10 March 2011

############## PREPARING AND RUNNING SIMULATIONS ###################
## This portion of the code goes through all of the steps taken
## to actually create the *.mp files for the various sample size
## categories for the Snowy Plover Sensitivity Analysis. By 'sample
## size' we are referring to the number of random metapop input
## parameter sets drawn at one time.  For uniform random sampling of 
## parameter sets, 10 replications of sample size = 10 par.sets. is 
## this same as 1 replication of sample size = 100 par.sets.  However,
## this is not true if Latin Hypercube Sampling is employed.

# Run on my mac, after sourcing sensitivity.setup, run as:
source('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/SACode_SNPL/sensitivity.setup.r')
sensitivity.setup('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/SACode_SNPL/')

# Run on Franklin (Linux Box)
source('/home/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/SACode_SNPL/sensitivity.setup.r')
sensitivity.setup('/home/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/SACode_SNPL/')

# First ran sensitivity analysis for FLSNPL_NoCC_Ceiling
sensitivity(sens.config.file='sens.cfg.snpl_nocc_ceiling.txt')
# Next, have to take a few steps to convert from *nix to Windows file format
snpl.l <- list.files(pattern="*.mp")
snpl.l <- snpl.l[-(1:3)] # Remove two templates
# Get current path
my.path <- getwd()
# Write text file of list of mp files
for ( sf in snpl.l ) { write( paste(my.path,sf,sep="/"), 'snpl_list.txt', append=TRUE) }
# From here I ran a bash script that converts from *nix to Windows
# Then, run batch shell script
# Once all *.mp files were done, I collected the data to have a look
mp.mult.results(mp.file.list='snpl_list.txt',out.csv='nocc_ceil_res.csv',spatial=TRUE,mac=TRUE)

# Now working on FLSNPL_NoCC_Contest
setwd('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/SampleSize_100/FLSNPL_2M_Ceiling_100/')
sensitivity(sens.config.file='sens.cfg.snpl_nocc_contest.txt')

# In shell, move all of the files just created to the current working directory
# $ mv snpl_out/* .
# Same as previous scenario to make mp list
snpl.l <- list.files(pattern="*.mp")
snpl.l <- snpl.l[-(1:2)] # Remove two templates
my.path <- getwd()
for ( sf in snpl.l ) { write( paste(my.path,sf,sep="/"), 'snpl_list.txt', append=TRUE) }
# In shell, convert from *nix to Windows format
# $ ../SACode_SNPL/mp_nix2win.sh snpl_list.txt
# Then execute shell script

# Now working on FLSNPL_1M_Ceiling
setwd('../FLSNPL_1M_Ceiling/')
sensitivity(sens.config.file='sens.cfg.snpl_1m_ceiling.txt')
# In shell, move all of the files just created to the current working directory
# $ mv snpl_out/* .
snpl.l <- list.files(pattern="*.mp")
snpl.l <- snpl.l[-(1:3)] # Remove three templates
my.path <- getwd()
for ( sf in snpl.l ) { write( paste(my.path,sf,sep="/"), 'snpl_list.txt', append=TRUE) }
# In shell, convert from *nix to Windows format
# $ ../SACode_SNPL/mp_nix2win.sh snpl_list.txt
# Execute shell script

# Now working on FLSNPL_1M_Contest
setwd('../FLSNPL_1M_Contest/')
sensitivity(sens.config.file='sens.cfg.snpl_1m_Contest.txt')
# In shell, move all of the files just created to the current working directory
# $ mv snpl_out/* .
snpl.l <- list.files(pattern="*.mp")
snpl.l <- snpl.l[-(1:3)] # Remove three templates
my.path <- getwd()
for ( sf in snpl.l ) { write( paste(my.path,sf,sep="/"), 'snpl_list.txt', append=TRUE) }
# In shell, convert from *nix to Windows format
# $ ../SACode_SNPL/mp_nix2win.sh snpl_list.txt
# Execute shell script

# Now working on FLSNPL_2M_Ceiling
setwd('../FLSNPL_2M_Ceiling/')
sensitivity(sens.config.file='sens.cfg.snpl_2M_ceiling.txt')
# In shell, move all of the files just created to the current working directory
# $ mv snpl_out/* .
snpl.l <- list.files(pattern="*.mp")
snpl.l <- snpl.l[-(1:3)] # Remove three templates
my.path <- getwd()
for ( sf in snpl.l ) { write( paste(my.path,sf,sep="/"), 'snpl_list.txt', append=TRUE) }
# In shell, convert from *nix to Windows format
# $ ../SACode_SNPL/mp_nix2win.sh snpl_list.txt
# Execute shell script

# Now working on FLSNPL_2M_Contest
setwd('../FLSNPL_2M_Contest/')
sensitivity(sens.config.file='sens.cfg.snpl_2M_contest.txt')
# In shell, move all of the files just created to the current working directory
# $ mv snpl_out/* .
snpl.l <- list.files(pattern="*.mp")
snpl.l <- snpl.l[-(1:3)] # Remove three templates
my.path <- getwd()
for ( sf in snpl.l ) { write( paste(my.path,sf,sep="/"), 'snpl_list.txt', append=TRUE) }
# In shell, convert from *nix to Windows format
# $ ../SACode_SNPL/mp_nix2win.sh snpl_list.txt
# Execute shell script

# ----------------------------------------------------------------------------------------------- #
# 12 March 2012
# I moved the files that I had been working on thus far into a directory
# called 'SampleSize_100'.  I am now going to run a few of these scenarios
# with 10,000 models created - see what happens with that.
#
# Based on some quick calculations, I can't fit this much data in my
# dropbox account, so I'm going to move it completely onto Linus
# The directory on Linus is now ~/Projects/SnowyPlover/SampleSize_10000 

# Processed mp files as for above - takes about 1/2 hour
# Starting shell script at 18:30 March 12

# I realize that I need to record the kch type in order to make sense of some
# of these results.  I made the adjustment to my mp.results script
# and am now going to rerun it
mp.mult.results(mp.file.list='/home/mlammens/Projects/SnowyPlover/SampleSize_10000/FLSNPL_2M_Ceiling/snpl_list.txt',out.csv='/home/mlammens/Projects/SnowyPlover/SampleSize_10000/FLSNPL_2M_Ceiling/2m_ceil_10000_res.csv',spatial=TRUE,mac=TRUE)
mp.mult.results(mp.file.list='/home/mlammens/Projects/SnowyPlover/SampleSize_10000/FLSNPL_NoCC_Ceiling/snpl_list.txt',out.csv='/home/mlammens/Projects/SnowyPlover/SampleSize_10000/FLSNPL_2M_Ceiling/nocc_ceil_10000_res.csv',spatial=TRUE,mac=TRUE)
# ----------------------------------------------------------------------------------------------- #
# 2 April 2012
# Creating URAND simulations - 10000 simulations first
setwd('/home/mlammens/Projects/SnowyPlover/SampleSize_10000_Unif/FLSNPL_NoCC_Ceiling/')
sensitivity('sens.cfg.snpl_nocc_ceiling.txt')
# In shell, move all of the files just created to the current working directory
# $ mv snpl_out/* .
snpl.l <- list.files(pattern="*.mp")
snpl.l <- snpl.l[-(1:3)] # Remove three templates
my.path <- getwd()
for ( sf in snpl.l ) { write( paste(my.path,sf,sep="/"), 'snpl_list.txt', append=TRUE) }
# In shell, convert from *nix to Windows format
# $ ../SACode_SNPL/mp_nix2win.sh snpl_list.txt
# Execute shell script

setwd('/home/mlammens/Projects/SnowyPlover/SampleSize_10000_Unif/FLSNPL_2M_Ceiling/')
sensitivity('sens.cfg.snpl_2m_ceiling.txt')

# Create CSV Files
mp.mult.results(mp.file.list='snpl_list.txt',out.csv='nocc_res_10000unif.csv',spatial=TRUE,mac=TRUE)
# Change working directory to 2m SLR directory
mp.mult.results(mp.file.list='snpl_list.txt',out.csv='slr2m_res_10000unif.csv',spatial=TRUE,mac=TRUE)


######## 1000 LHS #########
setwd('/home/mlammens/Projects/SnowyPlover/SampleSize_1000_LHS/FLSNPL_NoCC_Ceiling/')
sensitivity('sens.cfg.snpl_nocc_ceiling.txt')
# Then lines above to convert from nix to win

setwd('/home/mlammens/Projects/SnowyPlover/SampleSize_1000_LHS/FLSNPL_2M_Ceiling/')
sensitivity('sens.cfg.snpl_2m_ceiling.txt')
# Then lines above to convert from nix to win

# Create CSV Files
mp.mult.results(mp.file.list='snpl_list.txt',out.csv='slr2m_res_1000lhs.csv',spatial=TRUE,mac=TRUE)
# Change working directory to nocc directory
mp.mult.results(mp.file.list='snpl_list.txt',out.csv='nocc_res_1000lhs.csv',spatial=TRUE,mac=TRUE)

######### 100 LHS ########
# Starting from working directory ~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/
setwd('SampleSize_100/FLSNPL_NoCC_Ceiling_100/')
sensitivity(sens.config.file='sens.cfg.snpl_nocc_ceiling.txt')
# Get list of snpl files - was made on MAC!
snpl.l <- list.files(pattern="*.mp")
snpl.l <- snpl.l[-(1:3)] # Remove three templates
my.path <- getwd()
for ( sf in snpl.l ) { write( paste(my.path,sf,sep="/"), 'snpl_list.txt', append=TRUE) }
# In shell, convert from *nix to Windows format
# $ ../../SACode_SNPL/mp_nix2win.sh snpl_list.txt
# Make link file to MetaPop_Cmd
# $ln -s /User/mlammens/Dropbox/RAMAS/MetapopCmd.exe Metapop.exe
# Execute shell script

setwd('../../SampleSize_100/FLSNPL_2M_Ceiling_100/')
sensitivity(sens.config.file='sens.cfg.snpl_2m_ceiling.txt')
# Execute lines from above to get snpl list and convert files to nix
# After execution of shell script, extract results with mp.mult.results
# When doing this on a mac, it requires chaning the definition of 'wine' in the mp.results
# file
mp.mult.results(mp.file.list='snpl_list.txt',out.csv='slr2m_res_100lhs.csv',spatial=TRUE,mac=TRUE)
mp.mult.results(mp.file.list='snpl_list.txt',out.csv='nocc_res_100lhs.csv',spatial=TRUE,mac=TRUE)

######### 100 UNIF ########
# Starting from working directory ~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/
setwd('/home/mlammens/Projects/SnowyPlover/SampleSize_100_Unif/FLSNPL_NoCC_Ceiling/')
sensitivity(sens.config.file='sens.cfg.snpl_nocc_ceiling.txt')
# Move files is shell
snpl.l <- list.files(pattern="*.mp")
snpl.l <- snpl.l[-(1:3)] # Remove three templates
my.path <- getwd()
for ( sf in snpl.l ) { write( paste(my.path,sf,sep="/"), 'snpl_list.txt', append=TRUE) }
# In shell, convert from *nix to Windows format
# $ ../../SACode_SNPL/mp_nix2win.sh snpl_list.txt
# Make link file to MetaPop_Cmd
# $ln -s /User/mlammens/Dropbox/RAMAS/MetapopCmd.exe Metapop.exe
# Execute shell script

setwd('/home/mlammens/Projects/SnowyPlover/SampleSize_100_Unif/FLSNPL_2M_Ceiling/')
sensitivity(sens.config.file='sens.cfg.snpl_2m_ceiling.txt')
# Execute lines from above to get snpl list and convert files to nix
# After execution of shell script, extract results with mp.mult.results
# When doing this on a mac, it requires chaning the definition of 'wine' in the mp.results
# file
mp.mult.results(mp.file.list='snpl_list.txt',out.csv='slr2m_res_100unif.csv',spatial=TRUE,mac=TRUE)
mp.mult.results(mp.file.list='snpl_list.txt',out.csv='nocc_res_100unif.csv',spatial=TRUE,mac=TRUE)

############## END PREPARING AND RUNNING SIMULATIONS ###################

# ----------------------------------------------------------------------------------------------- #
############## ANALAYIS OF SIMULATION RESULTS - FIRST TRY ###################
## CSV files for all results are being stored in a dropbox directory:
## ~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results
## This requires that the CSV files in this folder are the most up-to-date
## versions.
# ----------------------------------------------------------------------------------------------- #

##############   MP PAR SET SAMPLE SIZE = 10000 LHS ##################

# Working on Mac
snpl.2m.10000lhs <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/slr2m_res_10000lhs.csv')
snpl.2m.10000lhs$kch.type <- factor( snpl.2m.10000lhs$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
snpl.nocc.10000lhs <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/nocc_res_10000lhs.csv')
snpl.nocc.10000lhs$kch.type <- factor( snpl.nocc.10000lhs$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))

# Check values are infact the same for input parameters
which(snpl.2m.10000lhs$EigenVal!=snpl.nocc.10000lhs$EigenVal)

# Looking at extinction risk
plot(snpl.nocc.10000lhs$ext.risk,snpl.2m.10000lhs$ext.risk)
ext.lm <- lm(snpl.2m.10000lhs$ext.risk~snpl.nocc.10000lhs$ext.risk)
summary(ext.lm)
abline(ext.lm,col='red')

# Examine delta EMA due to sea-level rise scenario (2m vs No SLR)
ema.delta <- snpl.2m.10000lhs$exp.min.n-snpl.nocc.10000lhs$exp.min.n
snpl.match.10000lhs <- cbind(snpl.nocc.10000lhs,ema.delta)
rm(ema.delta)
boxplot(snpl.match.10000lhs$ema.delta~snpl.match.10000lhs$kch.type)
title(xlab='Carrying Capacity Scenario',ylab='EMA: 2m SLR - No SLR',main='Change in EMA - 10000lhs Sims')

# Examine delat prob(50 indivdiauls) due to slr scenario
p50.delta <- snpl.2m.10000lhs$prob.50 - snpl.nocc.10000lhs$prob.50
snpl.match.10000lhs <- cbind(snpl.match.10000lhs,p50.delta)
rm(p50.delta)
# Box plot the p50.delta and observe that the biggest difference 
# occurs under the ME condition
boxplot(snpl.match.10000lhs$p50.delta~snpl.match.10000lhs$kch.type)
title(xlab='Carrying Capacity Scenario',ylab='Prob(50): 2m SLR - No SLR',main='Change in Prob to 50 - 10000lhs Sims')

# Make a combined data set of both nocc and 2mslr
snpl.10000lhs <- rbind(snpl.nocc.10000lhs,snpl.2m.10000lhs)
cc <- rep(c('nocc','slr2m'),each=10000)
snpl.10000lhs <- cbind(snpl.10000lhs,cc)
rm(cc)

# Plots with ggplot
library(ggplot2)
# Plot Growth Rate vs Delta_P50, colour points by KCH Type
ggplot() + geom_point(data=snpl.match.10000lhs,aes(x=GrowthRt,y=p50.delta,colour=kch.type))

# Plot Growth Rate vs P50, colour by KCH type, facet by SLR Scenario
ggplot() + geom_point(data=snpl.10000lhs,aes(x=GrowthRt,y=prob.50,colour=kch.type)) + facet_grid( .~ cc)

# Plot Growth Rate vs P50, colour by SLR Scenario, facet by KCH
ggplot() + geom_point(data=snpl.10000lhs,aes(x=GrowthRt,y=prob.50,colour=cc)) + facet_grid( kch.type~.)

# Examining histogram of Exp. Min. Abundance seperating by KCH type and SLR Scenario
ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.10000lhs) + facet_grid(cc~kch.type)
# Metapop Change
ggplot() + geom_histogram(aes(x=metapop.chng),data=snpl.10000lhs) + facet_grid(cc~.)
# Probability of decline to 50
ggplot() + geom_histogram(aes(x=prob.50),data=snpl.10000lhs) + facet_grid(cc~.)
# Probability of decline to 250
ggplot() + geom_histogram(aes(x=prob.250),data=snpl.10000lhs) + facet_grid(cc~.)

# Plot Growth Rate vs EMA
ggplot() + geom_point(data=snpl.match.10000lhs,aes(x=GrowthRt,y=ema.delta,colour=kch.type))
# Fecundity vs EMA
ggplot() + geom_point(data=snpl.match.10000lhs,aes(x=fecund,y=ema.delta,colour=kch.type))
# Adult Survival vs EMA
ggplot() + geom_point(data=snpl.match.10000lhs,aes(x=ad.surv,y=ema.delta,colour=kch.type))
# Variability vs EMA
ggplot() + geom_point(data=snpl.match.10000lhs,aes(x=stdev.avg,y=ema.delta,colour=kch.type))


# Box plot of Growth Rate based on different KCH types.  This is just to 
# examine if there are RANDOM differences between mean growth rates between 
# kch types
ggplot() + geom_boxplot(aes(x=kch.type,y=GrowthRt), data=snpl.10000lhs) + opts(title="10000lhs Simulations - LHS")

# Other development material
kruskal.test(snpl.match.10000lhs$p50.delta~snpl.match.10000lhs$kch.type)
boxplot(snpl.match.10000lhs$p50.delta~snpl.match.10000lhs$kch.type)

##############   MP PAR SET SAMPLE SIZE  = 10000 UNIF ##################

# Working on Mac
snpl.2m.10000unif <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/slr2m_res_10000unif.csv')
snpl.2m.10000unif$kch.type <- factor( snpl.2m.10000unif$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
snpl.nocc.10000unif <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/nocc_res_10000unif.csv')
snpl.nocc.10000unif$kch.type <- factor( snpl.nocc.10000unif$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))

# Check values are infact the same for input parameters
which(snpl.2m.10000unif$EigenVal!=snpl.nocc.10000unif$EigenVal)

# Examine delta EMA due to sea-level rise scenario (2m vs No SLR)
ema.delta <- snpl.2m.10000unif$exp.min.n-snpl.nocc.10000unif$exp.min.n
snpl.match.10000unif <- cbind(snpl.nocc.10000unif,ema.delta)
rm(ema.delta)
boxplot(snpl.match.10000unif$ema.delta~snpl.match.10000unif$kch.type)
title(xlab='Carrying Capacity Scenario',ylab='EMA: 2m SLR - No SLR',main='Change in EMA - 10000unif Sims')

# Examine delat prob(50 indivdiauls) due to slr scenario
p50.delta <- snpl.2m.10000unif$prob.50 - snpl.nocc.10000unif$prob.50
snpl.match.10000unif <- cbind(snpl.match.10000unif,p50.delta)
rm(p50.delta)
# Box plot the p50.delta and observe that the biggest difference 
# occurs under the ME condition
boxplot(snpl.match.10000unif$p50.delta~snpl.match.10000unif$kch.type)
title(xlab='Carrying Capacity Scenario',ylab='Prob(50): 2m SLR - No SLR',main='Change in Prob to 50 - 10000unif Sims')

# Make a combined data set of both nocc and 2mslr
snpl.10000unif <- rbind(snpl.nocc.10000unif,snpl.2m.10000unif)
cc <- rep(c('nocc','slr2m'),each=10000)
snpl.10000unif <- cbind(snpl.10000unif,cc)
rm(cc)

# Plots with ggplot
require(ggplot2)
# Plot Growth Rate vs Delta_P50, colour points by KCH Type
ggplot() + geom_point(data=snpl.match.10000unif,aes(x=GrowthRt,y=p50.delta,colour=kch.type))
# Plot Growth Rate vs P50, colour by KCH type, facet by SLR Scenario
ggplot() + geom_point(data=snpl.10000unif,aes(x=GrowthRt,y=prob.50,colour=kch.type)) + facet_grid( .~ cc)
# Plot Growth Rate vs P50, colour by SLR Scenario, facet by KCH
ggplot() + geom_point(data=snpl.10000unif,aes(x=GrowthRt,y=prob.50,colour=cc)) + facet_grid( kch.type~.)

# Examining histogram of Exp. Min. Abundance seperating by KCH type and SLR Scenario
ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.10000unif) + facet_grid(cc~kch.type)
# Metapop Change
ggplot() + geom_histogram(aes(x=metapop.chng),data=snpl.10000unif) + facet_grid(cc~.)
# Probability of decline to 50
ggplot() + geom_histogram(aes(x=prob.50),data=snpl.10000unif) + facet_grid(cc~.)
# Probability of decline to 250
ggplot() + geom_histogram(aes(x=prob.250),data=snpl.10000unif) + facet_grid(cc~.)

# Plot Growth Rate vs EMA
ggplot() + geom_point(data=snpl.match.10000unif,aes(x=GrowthRt,y=ema.delta,colour=kch.type))
# Fecundity vs EMA
ggplot() + geom_point(data=snpl.match.10000unif,aes(x=fecund,y=ema.delta,colour=kch.type))
# Adult Survival vs EMA
ggplot() + geom_point(data=snpl.match.10000unif,aes(x=ad.surv,y=ema.delta,colour=kch.type))
# Variability vs EMA
ggplot() + geom_point(data=snpl.match.10000unif,aes(x=stdev.avg,y=ema.delta,colour=kch.type))


# Box plot of Growth Rate based on different KCH types.  This is just to 
# examine if there are RANDOM differences between mean growth rates between 
# kch types
ggplot() + geom_boxplot(aes(x=kch.type,y=GrowthRt), data=snpl.10000unif) + opts(title="10000unif Simulations - LHS")


##############   MP PAR SET SAMPLE SIZE  = 1000 LHS ##################

# Working on Mac
snpl.2m.1000lhs <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/slr2m_res_1000lhs.csv')
snpl.2m.1000lhs$kch.type <- factor( snpl.2m.1000lhs$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
snpl.nocc.1000lhs <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/nocc_res_1000lhs.csv')
snpl.nocc.1000lhs$kch.type <- factor( snpl.nocc.1000lhs$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))

# Check values are infact the same for input parameters
which(snpl.2m.1000lhs$EigenVal!=snpl.nocc.1000lhs$EigenVal)

# Examine delta EMA due to sea-level rise scenario (2m vs No SLR)
ema.delta <- snpl.2m.1000lhs$exp.min.n-snpl.nocc.1000lhs$exp.min.n
snpl.match.1000lhs <- cbind(snpl.nocc.1000lhs,ema.delta)
rm(ema.delta)
boxplot(snpl.match.1000lhs$ema.delta~snpl.match.1000lhs$kch.type)
title(xlab='Carrying Capacity Scenario',ylab='EMA: 2m SLR - No SLR',main='Change in EMA - 1000lhs Sims')

# Examine delat prob(50 indivdiauls) due to slr scenario
p50.delta <- snpl.2m.1000lhs$prob.50 - snpl.nocc.1000lhs$prob.50
snpl.match.1000lhs <- cbind(snpl.match.1000lhs,p50.delta)
rm(p50.delta)
# Box plot the p50.delta and observe that the biggest difference 
# occurs under the ME condition
boxplot(snpl.match.1000lhs$p50.delta~snpl.match.1000lhs$kch.type)
title(xlab='Carrying Capacity Scenario',ylab='Prob(50): 2m SLR - No SLR',main='Change in Prob to 50 - 1000lhs Sims')

# Make a combined data set of both nocc and 2mslr
snpl.1000lhs <- rbind(snpl.nocc.1000lhs,snpl.2m.1000lhs)
cc <- rep(c('nocc','slr2m'),each=1000)
snpl.1000lhs <- cbind(snpl.1000lhs,cc)
rm(cc)

# Plot Growth Rate vs Delta_P50, colour points by KCH Type
ggplot() + geom_point(data=snpl.match.1000lhs,aes(x=GrowthRt,y=p50.delta,colour=kch.type))
# Plot Growth Rate vs P50, colour by KCH type, facet by SLR Scenario
ggplot() + geom_point(data=snpl.1000lhs,aes(x=GrowthRt,y=prob.50,colour=kch.type)) + facet_grid( .~ cc)
# Plot Growth Rate vs P50, colour by SLR Scenario, facet by KCH
ggplot() + geom_point(data=snpl.1000lhs,aes(x=GrowthRt,y=prob.50,colour=cc)) + facet_grid( kch.type~.)

# Examining histogram of Exp. Min. Abundance seperating by KCH type and SLR Scenario
ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.1000lhs) + facet_grid(cc~kch.type)
# Metapop Change
ggplot() + geom_histogram(aes(x=metapop.chng),data=snpl.1000lhs) + facet_grid(cc~.)
# Probability of decline to 50
ggplot() + geom_histogram(aes(x=prob.50),data=snpl.1000lhs) + facet_grid(cc~.)
# Probability of decline to 250
ggplot() + geom_histogram(aes(x=prob.250),data=snpl.1000lhs) + facet_grid(cc~.)

# Plot Growth Rate vs EMA
ggplot() + geom_point(data=snpl.match.1000lhs,aes(x=GrowthRt,y=ema.delta,colour=kch.type))
# Fecundity vs EMA
ggplot() + geom_point(data=snpl.match.1000lhs,aes(x=fecund,y=ema.delta,colour=kch.type))
# Adult Survival vs EMA
ggplot() + geom_point(data=snpl.match.1000lhs,aes(x=ad.surv,y=ema.delta,colour=kch.type))
# Variability vs EMA
ggplot() + geom_point(data=snpl.match.1000lhs,aes(x=stdev.avg,y=ema.delta,colour=kch.type))

# Box plot of Growth Rate based on different KCH types.  This is just to 
# examine if there are RANDOM differences between mean growth rates between 
# kch types
ggplot() + geom_boxplot(aes(x=kch.type,y=GrowthRt), data=snpl.1000lhs) + opts(title="1000lhs Simulations - LHS")

##############   MP PAR SET SAMPLE SIZE  = 1000 UNIF ##################

# Working on Mac
snpl.2m.1000unif <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/slr2m_res_1000unif.csv')
snpl.2m.1000unif$kch.type <- factor( snpl.2m.1000unif$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
snpl.nocc.1000unif <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/nocc_res_1000unif.csv')
snpl.nocc.1000unif$kch.type <- factor( snpl.nocc.1000unif$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))

# Check values are infact the same for input parameters
which(snpl.2m.1000unif$EigenVal!=snpl.nocc.1000unif$EigenVal)

# Examine delta EMA due to sea-level rise scenario (2m vs No SLR)
ema.delta <- snpl.2m.1000unif$exp.min.n-snpl.nocc.1000unif$exp.min.n
snpl.match.1000unif <- cbind(snpl.nocc.1000unif,ema.delta)
rm(ema.delta)
boxplot(snpl.match.1000unif$ema.delta~snpl.match.1000unif$kch.type)
title(xlab='Carrying Capacity Scenario',ylab='EMA: 2m SLR - No SLR',main='Change in EMA - 1000unif Sims')

# Examine delat prob(50 indivdiauls) due to slr scenario
p50.delta <- snpl.2m.1000unif$prob.50 - snpl.nocc.1000unif$prob.50
snpl.match.1000unif <- cbind(snpl.match.1000unif,p50.delta)
rm(p50.delta)
# Box plot the p50.delta and observe that the biggest difference 
# occurs under the ME condition
boxplot(snpl.match.1000unif$p50.delta~snpl.match.1000unif$kch.type)
title(xlab='Carrying Capacity Scenario',ylab='Prob(50): 2m SLR - No SLR',main='Change in Prob to 50 - 1000unif Sims')

# Make a combined data set of both nocc and 2mslr
snpl.1000unif <- rbind(snpl.nocc.1000unif,snpl.2m.1000unif)
cc <- rep(c('nocc','slr2m'),each=1000)
snpl.1000unif <- cbind(snpl.1000unif,cc)
rm(cc)

# Plot Growth Rate vs Delta_P50, colour points by KCH Type
ggplot() + geom_point(data=snpl.match.1000unif,aes(x=GrowthRt,y=p50.delta,colour=kch.type))
# Plot Growth Rate vs P50, colour by KCH type, facet by SLR Scenario
ggplot() + geom_point(data=snpl.1000unif,aes(x=GrowthRt,y=prob.50,colour=kch.type)) + facet_grid( .~ cc)
# Plot Growth Rate vs P50, colour by SLR Scenario, facet by KCH
ggplot() + geom_point(data=snpl.1000unif,aes(x=GrowthRt,y=prob.50,colour=cc)) + facet_grid( kch.type~.)

# Examining histogram of Exp. Min. Abundance seperating by KCH type and SLR Scenario
ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.1000unif) + facet_grid(cc~kch.type)
# Metapop Change
ggplot() + geom_histogram(aes(x=metapop.chng),data=snpl.1000unif) + facet_grid(cc~.)
# Probability of decline to 50
ggplot() + geom_histogram(aes(x=prob.50),data=snpl.1000unif) + facet_grid(cc~.)
# Probability of decline to 250
ggplot() + geom_histogram(aes(x=prob.250),data=snpl.1000unif) + facet_grid(cc~.)

# Plot Growth Rate vs EMA
ggplot() + geom_point(data=snpl.match.1000unif,aes(x=GrowthRt,y=ema.delta,colour=kch.type))
# Fecundity vs EMA
ggplot() + geom_point(data=snpl.match.1000unif,aes(x=fecund,y=ema.delta,colour=kch.type))
# Adult Survival vs EMA
ggplot() + geom_point(data=snpl.match.1000unif,aes(x=ad.surv,y=ema.delta,colour=kch.type))
# Variability vs EMA
ggplot() + geom_point(data=snpl.match.1000unif,aes(x=stdev.avg,y=ema.delta,colour=kch.type))

# Box plot of Growth Rate based on different KCH types.  This is just to 
# examine if there are RANDOM differences between mean growth rates between 
# kch types
ggplot() + geom_boxplot(aes(x=kch.type,y=GrowthRt), data=snpl.1000unif) + opts(title="1000unif Simulations - LHS")

##############   MP PAR SET SAMPLE SIZE  = 100 LHS ##################

# Lets look at the results of these simulations
snpl.nocc.100lhs <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/nocc_res_100lhs.csv')
snpl.nocc.100lhs$kch.type <- factor( snpl.nocc.100lhs$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
snpl.2m.100lhs <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/slr2m_res_100lhs.csv')
snpl.2m.100lhs$kch.type <- factor( snpl.2m.100lhs$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
snpl.100lhs <- rbind( snpl.nocc.100lhs, snpl.2m.100lhs )
cc <- rep(c('nocc','slr2m'),each=100)
snpl.100lhs <- cbind(snpl.100lhs, cc)

# Check that input parameters match
which( snpl.nocc.100lhs$GrowthRt != snpl.2m.100lhs$GrowthRt )

boxplot(snpl.100lhs$ext.risk~snpl.100lhs$cc)
p <- ggplot( snpl.100lhs, aes( GrowthRt, prob.50) )
p + geom_point(aes(colour = kch.type)) + facet_grid(.~ cc)

p50.delta <- snpl.2m.100lhs$prob.50 - snpl.nocc.100lhs$prob.50
ema.delta <- snpl.2m.100lhs$exp.min.n - snpl.nocc.100lhs$exp.min.n
snpl.match.100lhs <- cbind(snpl.nocc.100lhs,ema.delta,p50.delta)
p.100lhs.mat <- ggplot( snpl.match.100lhs, aes( GrowthRt, p50.delta) )
p.100lhs.mat + geom_point(aes(colour = kch.type))

boxplot(snpl.match.100lhs$p50.delta~snpl.match.100lhs$kch.type)
title(xlab='Carrying Capacity Scenario',ylab='Prob(50): 2m SLR - No SLR',main='Change in  P-decline to 50 - 100lhs reps')
boxplot(snpl.match.100lhs$ema.delta~snpl.match.100lhs$kch.typ)
title(xlab='Carrying Capacity Scenario',ylab='EMA: 2m SLR - No SLR',main='Change in EMA: 100lhs reps')

# Box plot of Growth Rate based on different KCH types.  This is just to 
# examine if there are RANDOM differences between mean growth rates between 
# kch types
ggplot() + geom_boxplot(aes(x=kch.type,y=GrowthRt), data=snpl.100lhs) + opts(title="100lhs Simulations - LHS")

# Historgrams of Exp. Min. Abundance seperated by kch type and slr scenario
ggplot() + geom_histogram(aes(x=exp.min.n, fill=kch.type),data=snpl.100lhs)
ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.100lhs) + facet_grid(cc~kch.type)

# Check if KCH is significant in this case
kruskal.test(snpl.2m.100lhs$prob.50~snpl.2m.100lhs$kch.type)
kruskal.test(snpl.nocc.100lhs$prob.50~snpl.nocc.100lhs$kch.type)
kruskal.test(snpl.match.100lhs$p50.delta~snpl.match.100lhs$kch.type)

##############   MP PAR SET SAMPLE SIZE  = 100 UNIF ##################

# Lets look at the results of these simulations
snpl.nocc.100unif <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/nocc_res_100unif.csv')
snpl.nocc.100unif$kch.type <- factor( snpl.nocc.100unif$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
snpl.2m.100unif <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/slr2m_res_100unif.csv')
snpl.2m.100unif$kch.type <- factor( snpl.2m.100unif$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
snpl.100unif <- rbind( snpl.nocc.100unif, snpl.2m.100unif )
cc <- rep(c('nocc','slr2m'),each=100)
snpl.100unif <- cbind(snpl.100unif, cc)

# Check that input parameters match
which( snpl.nocc.100unif$GrowthRt != snpl.2m.100unif$GrowthRt )

boxplot(snpl.100unif$ext.risk~snpl.100unif$cc)
ggplot( snpl.100unif, aes( GrowthRt, prob.50) ) + geom_point(aes(colour = kch.type)) + facet_grid(.~ cc)

p50.delta <- snpl.2m.100unif$prob.50 - snpl.nocc.100unif$prob.50
ema.delta <- snpl.2m.100unif$exp.min.n - snpl.nocc.100unif$exp.min.n
snpl.match.100unif <- cbind(snpl.nocc.100unif,ema.delta,p50.delta)
ggplot( snpl.match.100unif, aes( GrowthRt, p50.delta) ) + geom_point(aes(colour = kch.type))

boxplot(snpl.match.100unif$p50.delta~snpl.match.100unif$kch.type)
title(xlab='Carrying Capacity Scenario',ylab='Prob(50): 2m SLR - No SLR',main='Change in  P-decline to 50 - 100unif reps')
boxplot(snpl.match.100unif$ema.delta~snpl.match.100unif$kch.typ)
title(xlab='Carrying Capacity Scenario',ylab='EMA: 2m SLR - No SLR',main='Change in EMA: 100unif reps')

# Box plot of Growth Rate based on different KCH types.  This is just to 
# examine if there are RANDOM differences between mean growth rates between 
# kch types
ggplot() + geom_boxplot(aes(x=kch.type,y=GrowthRt), data=snpl.100unif) + opts(title="100unif Simulations - LHS")

# Historgrams of Exp. Min. Abundance seperated by kch type and slr scenario
ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.100unif) + facet_grid(cc~kch.type)
# Historgrams of Growth Rate seperated by kch type 
ggplot() + geom_histogram(aes(x=GrowthRt),data=snpl.100unif) + facet_grid(kch.type~.)


############## EXAMINING RESULTS BASED ON REPLICATIONS ########
snpl.match.100 <- cbind(snpl.match.100, rep(factor(100),100))
names(snpl.match.100)[67]<-'reps'
snpl.match.1000 <- cbind(snpl.match.1000, rep(factor(1000),1000))
names(snpl.match.1000)[67]<-'reps'
snpl.match.10000 <- cbind(snpl.match.10000, rep(factor(10000),10000))
names(snpl.match.10000)[67]<-'reps'
snpl <- rbind(snpl.match.100,snpl.match.1000,snpl.match.10000)

ggplot() + geom_boxplot(aes(x=reps,y=ema.delta), data=snpl)
ggplot() + geom_boxplot(aes(x=reps,y=p50.delta), data=snpl)

# Putting together plot that Resit and I talked about (Look up if at desk)
snpl.p50.mean <- tapply(snpl$p50.delta,snpl$reps,mean)
snpl.p50.sd <- tapply(snpl$p50.delta,snpl$reps,sd)
snpl.p50.se <- snpl.p50.sd / sqrt(c(100,1000,10000))

sims <- factor(c(100,1000,10000))
df <- data.frame( sims = sims, p50.mean = snpl.p50.mean, p50.se = snpl.p50.se )
limits <- aes(ymax = p50.mean + p50.se, ymin = p50.mean - p50.se)
ggplot(df, aes(y=p50.mean,x=sims)) + geom_point() + geom_errorbar(limits,width=0.2)

# Development Work
ggplot() + geom_histogram(aes(x=fecund),data=snpl.100)
ggplot() + geom_histogram(aes(x=fecund),data=snpl.100unif)
ggplot() + geom_histogram(aes(x=fecund),data=snpl.10000unif)
ggplot() + geom_histogram(aes(x=fecund),data=snpl.nocc.10000unif)
ggplot() + geom_histogram(aes(x=fecund),data=snpl.nocc.10000)
ggplot() + geom_point(data=snpl.nocc.10000unif, aes(x=fecund,y=ad.surv,colour=kch.type))
ggplot() + geom_point(data=snpl.nocc.100unif, aes(x=fecund,y=ad.surv,colour=kch.type))
ggplot() + geom_point(data=snpl.nocc.100lhs, aes(x=fecund,y=ad.surv,colour=kch.type))
ggplot() + geom_point(data=snpl.nocc.100, aes(x=fecund,y=ad.surv,colour=kch.type))

# More Development stuff here #### COMPARISON OF SAMPLING METHODS #########

## 10000 SIMULATIONS
snpl.10000 <- rbind(snpl.match.10000lhs,snpl.match.10000unif)
samp <- rep(factor(c('lhs','unif')),each=10000)
snpl.10000 <- cbind(snpl.10000,samp)
sims <- rep(factor(10000),10000)
snpl.10000 <- cbind( snpl.10000, sims )
# Historgrams of Exp. Min. Abundance facet by 'samp'ling scheme
ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.10000) + facet_grid(samp~.) + opts(title="EMA 10000 Sims, facet by Sampling")
# Historgrams of fecundity seperated by sampling scheme 
ggplot() + geom_histogram(aes(x=fecund),data=snpl.10000) + facet_grid(samp~.)

## 1000 SIMULATIONS
snpl.1000 <- rbind(snpl.match.1000lhs,snpl.match.1000unif)
samp <- rep(factor(c('lhs','unif')),each=1000)
snpl.1000 <- cbind(snpl.1000,samp)
sims <- rep(factor(1000),1000)
snpl.1000 <- cbind( snpl.1000, sims )
# Historgrams of Exp. Min. Abundance facet by 'samp'ling scheme
ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.1000) + facet_grid(samp~.)
# Historgrams of fecundity seperated by sampling scheme 
ggplot() + geom_histogram(aes(x=fecund),data=snpl.1000) + facet_grid(samp~.)

## 100 SIMULATIONS
snpl.100 <- rbind(snpl.match.100lhs,snpl.match.100unif)
samp <- rep(factor(c('lhs','unif')),each=100)
snpl.100 <- cbind(snpl.100,samp)
sims <- rep(factor(100),100)
snpl.100 <- cbind( snpl.100, sims )
# Historgrams of Exp. Min. Abundance facet by 'samp'ling scheme
ggplot() + geom_histogram(aes(x=exp.min.n),data=snpl.100) + facet_grid(samp~.)
# Historgrams of fecundity seperated by sampling scheme 
ggplot() + geom_histogram(aes(x=fecund),data=snpl.100) + facet_grid(samp~.)


snpl.100.p50.mean <- tapply(snpl.100$p50.delta,snpl.100$samp,mean)
snpl.100.p50.sd <- tapply(snpl.100$p50.delta,snpl.100$samp,sd)
snpl.100.p50.se <- snpl.100.p50.sd / sqrt(100)
#
snpl.1000.p50.mean <- tapply(snpl.1000$p50.delta,snpl.1000$samp,mean)
snpl.1000.p50.sd <- tapply(snpl.1000$p50.delta,snpl.1000$samp,sd)
snpl.1000.p50.se <- snpl.1000.p50.sd / sqrt(1000)
#
snpl.10000.p50.mean <- tapply(snpl.10000$p50.delta,snpl.10000$samp,mean)
snpl.10000.p50.sd <- tapply(snpl.10000$p50.delta,snpl.10000$samp,sd)
snpl.10000.p50.se <- snpl.10000.p50.sd / sqrt(10000)

# Make a super-matrix
snpl <- rbind(snpl.10000,snpl.1000,snpl.100)
# Histogram of fecundity values faceted by sampling scheme and number of simulations
ggplot() + geom_histogram(aes(fecund),data=snpl) + facet_grid(samp~sims)
# Histogram of Delta EMA values facted by sampling scheme and number of simulations
ggplot() + geom_histogram(aes(x=ema.delta,y=..density..),data=snpl) + facet_grid(samp~sims)

snpl.p50.mean <- tapply(snpl$p50.delta, INDEX=list(snpl$samp,snpl$sims), FUN=mean)
snpl.p50.sd <- tapply(snpl$p50.delta, INDEX=list(snpl$samp,snpl$sims), FUN=sd)
sims <- matrix(rep(c(10000,1000,100),2),byrow=TRUE,nrow=2)
snpl.p50.se <- snpl.p50.sd / sims

p50.delta.mean <- as.vector(snpl.p50.mean)
p50.delta.se <- as.vector(snpl.p50.se)
sims <- rep(factor(c(10000,1000,100)),each=2)
samp <- rep(c('lhs','unif'),3)
df <- data.frame( p50.delta.mean, p50.delta.se, sims, samp )

limits <- aes(ymax=p50.delta.mean+p50.delta.se, ymin=p50.delta.mean-p50.delta.se)

ggplot(df, aes(y=p50.delta.mean,x=sims,colour=samp)) + geom_point() + geom_errorbar(limits,width=0.2)

# EMA
snpl.ema.mean <- tapply(snpl$ema.delta, INDEX=list(snpl$samp,snpl$sims), FUN=mean)
snpl.ema.sd <- tapply(snpl$ema.delta, INDEX=list(snpl$samp,snpl$sims), FUN=sd)
sims <- matrix(rep(c(10000,1000,100),2),byrow=TRUE,nrow=2)
snpl.ema.se <- snpl.ema.sd / sims

ema.delta.mean <- as.vector(snpl.ema.mean)
ema.delta.sd <- as.vector(snpl.ema.sd)
sims <- rep(factor(c(10000,1000,100)),each=2)
samp <- rep(c('lhs','unif'),3)
df.ema <- data.frame( ema.delta.mean, ema.delta.sd, sims, samp )

limits.ema <- aes(ymax=ema.delta.mean+ema.delta.sd, ymin=ema.delta.mean-ema.delta.sd)

ggplot(df.ema, aes(y=ema.delta.mean,x=sims,colour=samp)) + geom_point() + geom_errorbar(limits.ema,width=0.2)


# More DEV stuff
snpl.100.temp <- rbind(snpl.100lhs,snpl.100unif)
samp <- rep(c('lhs','unif'),each=200)
snpl.100.temp <- cbind(snpl.100.temp,samp)
ggplot() + geom_histogram( aes(x=exp.min.n), data=snpl.100.temp ) + facet_grid(cc~samp)

snpl.10000.temp <- rbind(snpl.10000lhs,snpl.10000unif)
samp <- rep(c('lhs','unif'),each=20000)
snpl.10000.temp <- cbind(snpl.10000.temp, samp)
ggplot() + geom_histogram( aes(x=exp.min.n), data=snpl.10000.temp ) + facet_grid(cc~samp)
ggplot() + geom_histogram( aes(x=exp.min.n), data=snpl.10000.temp ) + facet_grid(samp~cc)

##########################################################################################
############## RUNNING MULTIPLE REPLICATIONS OF SAMPLE SIZE SETS ########
## Running multiple sensitivity analyses at once. 
## The little pieces of script below are executed by
## highlighting the appropriate section, and then executing
## them, rather than via some funtion.
## IMPORTANT: Many of the pieces below must be 
## modified slightly between SampleSize directories.
## Be mindful of any references to particular numbers 
## of replicate simulations.

## Remember to source all of the relavent sensitivity analysis 
## scripts before running the lines below

# Run sensitivity.r for each file in a list of sens.cfg* files
cfg_list <- read.table('cfg_list.txt')
pres.wd <- getwd()
for (cfg in cfg_list$V1){
  print(cfg)
  cfg.dir <- sub('sens.*','',cfg)
  setwd(cfg.dir)
  sensitivity(cfg)
  setwd(pres.wd)
}

# Create new snpl_list.txt files for each of the directories
# in which sensitivity analyses were just run
for (cfg in cfg_list$V1){
  print(cfg)
  cfg.dir <- sub('sens.*','',cfg)
  setwd(cfg.dir)
  snpl.l <- list.files(pattern="*.mp")
  snpl.l <- snpl.l[-(1:3)] # Remove three templates
  # Get current path
  my.path <- getwd()
  # Write text file of list of mp files
  for ( sf in snpl.l ) { write( paste(my.path,sf,sep="/"), 'snpl_list.txt', append=TRUE) }
  setwd(pres.wd)
}

# Run mp.mult.results.r in each of the directories in which
# a sensitivity analysis was just run
snpl.lists <- read.table('snpl_list_mult.txt')
for (list in snpl.lists$V1){
  print(list)
  out.csv <- sub( 'snpl_list.txt', 'res_10k.csv', list)
  print(paste('extracting mp results for:', out.csv))
  mp.mult.results(mp.file.list=list,out.csv=out.csv,spatial=TRUE,mac=FALSE)
}

##########################################################################################
############## LOADING REPLICATE SAMPLE SIZE 1000 RESULTS ########
## WARNING: This section contains many variable names that
## are repeated from above - Be *careful* when executing code
## in this section

# Read file that is a list of all of the res.csv files for 
# the sample size 1000 restuls
snpl.1000.rf <- read.table('/home/mlammens/Projects/SnowyPlover/SampleSize_1000_Rep/res_list.txt')

# Read in each files results
snpl.1000.rep <- vector()
for ( rf in snpl.1000.rf$V1 ){
  # Set the file name as a character
  rf.name <- as.character(rf)
  # Split the various parts of the file name. These 
  # will be used to assign factor values.
  rf.name.vect <- unlist(strsplit(rf.name,split="/"))
  # Get Randomization technique and replication number
  rf.rand.rep <- unlist(strsplit(rf.name.vect[7],split="_"))
  RandType <- rf.rand.rep[1]
  RepNumber <- rf.rand.rep[2]
  # Get SLR scenario
  rf.slr <- unlist(strsplit(rf.name.vect[8],split="_"))
  SLR <- rf.slr[2]
  # In this 'for' loop, fix the Sample Size to 1000
  SampSize <- "1000"
  
  # Read results file
  res.temp <- read.csv(rf)
  # Get the number of rows in this file
  rf.rows <- nrow(res.temp)
  # Make a data frame with the Sample Size, RandType, RepNumber, etc. information
  samp.info <- data.frame(RandType=rep(RandType,rf.rows), RepNumber=rep(RepNumber,rf.rows), SLR=rep(SLR,rf.rows), SampSize=rep(SampSize,rf.rows))
  # Combine the two data.frames
  res.temp <- cbind(res.temp,samp.info)
  
  # Bind temporary df to full df
  snpl.1000.rep <- rbind(snpl.1000.rep,res.temp)
}

# Change order of kch factor type
snpl.1000.rep$kch.type <- factor( snpl.1000.rep$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
# Have an exploratory look at the data
ggplot() + geom_point(data=snpl.1000.rep, aes(x=fecund,y=exp.min.n, colour=kch.type)) + facet_grid(SLR~RandType)

## Calculate Delta Values 
# First check for consistency of values
which(snpl.1000.rep$EigenVal[1:10000]!=snpl.1000.rep$EigenVal[10001:20000])
which(snpl.1000.rep$EigenVal[20001:30000]!=snpl.1000.rep$EigenVal[30001:40000])

# Integer bounds of different SLR and RandType treatments
lhs.nocc <- 1:10000
lhs.2m <- 10001:20000
unif.nocc <- 20001:30000
unif.2m <- 30001:40000

# Calculate Delta-P50 and Delta-EMA
p50.delta <- snpl.1000.rep$prob.50[c(lhs.2m,unif.2m)] - snpl.1000.rep$prob.50[c(lhs.nocc,unif.nocc)]
ema.delta <- snpl.1000.rep$exp.min.n[c(lhs.2m,unif.2m)] - snpl.1000.rep$exp.min.n[c(lhs.nocc,unif.nocc)]

# A quick look at ema split by slr, rep.num, and rand.type
ggplot(snpl.1000.rep,aes(SLR,exp.min.n)) + geom_boxplot() + facet_grid(RandType~RepNumber)

# Make 'matched' data.frame
snpl.1000.rep.match <- cbind(snpl.1000.rep[c(lhs.nocc,unif.nocc),], p50.delta, ema.delta)

# A quick look at Delta-P50
ggplot( snpl.1000.rep.match, aes( GrowthRt, p50.delta)) + geom_point( aes( colour=kch.type)) + facet_grid(RepNumber~RandType)

# Calculate means for each rep and have a look
p50.mean.1000 <- tapply( snpl.1000.rep.match$p50.delta, INDEX = list(snpl.1000.rep.match$RepNumber,snpl.1000.rep.match$RandType),mean)
plot(p50.mean.1000[,1])
points(p50.mean.1000[,2],col='red')
title('Delta-P50 Sample Size 1000')

############## LOADING REPLICATE SAMPLE SIZE 10000 - SA2 RESULTS ########
## WARNING: This section contains many variable names that
## are repeated from above - Be *careful* when executing code
## in this section

# Read file that is a list of all of the res.csv files for 
# the sample size 10000.SA2 restuls
snpl.10000.SA2.rf <- read.table('C:/Projects/SnowyPlover/res_list.txt')

# Read in each files results
snpl.10000.SA2.rep <- vector()
for ( rf in snpl.10000.SA2.rf$V1 ){
  # Set the file name as a character
  rf.name <- as.character(rf)
  # Split the various parts of the file name. These 
  # will be used to assign factor values.
  rf.name.vect <- unlist(strsplit(rf.name,split="/"))
  # Get Randomization technique and replication number
  rf.rand.rep <- unlist(strsplit(rf.name.vect[4],split="_"))
  RandType <- rf.rand.rep[3]
  ###RepNumber <- rf.rand.rep[2]
  RepNumber <- "1"
  # Get SLR scenario
  rf.slr <- unlist(strsplit(rf.name.vect[5],split="_"))
  SLR <- rf.slr[2]
  # In this 'for' loop, fix the Sample Size to 10000.SA2
  SampSize <- "10000.SA2"
  
  # Read results file
  res.temp <- read.csv(rf)
  # Get the number of rows in this file
  rf.rows <- nrow(res.temp)
  # Make a data frame with the Sample Size, RandType, RepNumber, etc. information
  samp.info <- data.frame(RandType=rep(RandType,rf.rows), RepNumber=rep(RepNumber,rf.rows), SLR=rep(SLR,rf.rows), SampSize=rep(SampSize,rf.rows))
  # Combine the two data.frames
  res.temp <- cbind(res.temp,samp.info)
  
  # Bind temporary df to full df
  snpl.10000.SA2.rep <- rbind(snpl.10000.SA2.rep,res.temp)
}
write.csv(snpl.10000.SA2.rep,file="snpl.10000.rep.SA2.csv",row.names=FALSE)

# Change order of kch factor type
snpl.10000.SA2.rep$kch.type <- factor( snpl.10000.SA2.rep$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
# Have an exploratory look at the data
ggplot() + geom_point(data=snpl.10000.SA2.rep, aes(x=fecund,y=exp.min.n, colour=kch.type)) + facet_grid(SLR~RandType)

## Calculate Delta Values 
# First check for consistency of values
which(snpl.10000.SA2.rep$EigenVal[1:10000.SA20]!=snpl.10000.SA2.rep$EigenVal[10000.SA21:20000])
which(snpl.10000.SA2.rep$EigenVal[20001:30000]!=snpl.10000.SA2.rep$EigenVal[30001:40000])

# Integer bounds of different SLR and RandType treatments
lhs.nocc <- 1:10000
lhs.2m <- 10001:20000
unif.nocc <- 20001:30000
unif.2m <- 30001:40000

# Calculate Delta-P50 and Delta-EMA
p50.delta <- snpl.10000.SA2.rep$prob.50[c(lhs.2m,unif.2m)] - snpl.10000.SA2.rep$prob.50[c(lhs.nocc,unif.nocc)]
ema.delta <- snpl.10000.SA2.rep$exp.min.n[c(lhs.2m,unif.2m)] - snpl.10000.SA2.rep$exp.min.n[c(lhs.nocc,unif.nocc)]

# A quick look at ema split by slr, rep.num, and rand.type
ggplot(snpl.10000.SA2.rep,aes(SLR,exp.min.n)) + geom_boxplot() + facet_grid(RandType~RepNumber)

# Make 'matched' data.frame
snpl.10000.SA2.rep.match <- cbind(snpl.10000.SA2.rep[c(lhs.nocc,unif.nocc),], p50.delta, ema.delta)

# A quick look at Delta-P50
ggplot( snpl.10000.SA2.rep.match, aes( GrowthRt, p50.delta)) + geom_point( aes( colour=kch.type)) + facet_grid(RepNumber~RandType)

# Calculate means for each rep and have a look
p50.mean.10000.SA2 <- tapply( snpl.10000.SA2.rep.match$p50.delta, INDEX = list(snpl.10000.SA2.rep.match$RepNumber,snpl.10000.SA2.rep.match$RandType),mean)
plot(p50.mean.10000.SA2[,1])
points(p50.mean.10000.SA2[,2],col='red')
title('Delta-P50 Sample Size 10000.SA2')

############## LOADING REPLICATE SAMPLE SIZE 500 RESULTS ########
## WARNING: This section contains many variable names that
## are repeated from above - Be *careful* when executing code
## in this section

# Read file that is a list of all of the res.csv files for 
# the sample size 500 restuls
snpl.500.rf <- read.table('/home/mlammens/Projects/SnowyPlover/SampleSize_500_Rep/res_list.txt')

# Read in each files results
snpl.500.rep <- vector()
for ( rf in snpl.500.rf$V1 ){
  # Set the file name as a character
  rf.name <- as.character(rf)
  # Split the various parts of the file name. These 
  # will be used to assign factor values.
  rf.name.vect <- unlist(strsplit(rf.name,split="/"))
  # Get Randomization technique and replication number
  rf.rand.rep <- unlist(strsplit(rf.name.vect[7],split="_"))
  RandType <- rf.rand.rep[1]
  RepNumber <- rf.rand.rep[2]
  # Get SLR scenario
  rf.slr <- unlist(strsplit(rf.name.vect[8],split="_"))
  SLR <- rf.slr[2]
  # In this 'for' loop, fix the Sample Size to 500
  SampSize <- "500"
  
  # Read results file
  res.temp <- read.csv(rf)
  # Get the number of rows in this file
  rf.rows <- nrow(res.temp)
  # Make a data frame with the Sample Size, RandType, RepNumber, etc. information
  samp.info <- data.frame(RandType=rep(RandType,rf.rows), RepNumber=rep(RepNumber,rf.rows), SLR=rep(SLR,rf.rows), SampSize=rep(SampSize,rf.rows))
  # Combine the two data.frames
  res.temp <- cbind(res.temp,samp.info)
  
  # Bind temporary df to full df
  snpl.500.rep <- rbind(snpl.500.rep,res.temp)
}

# Write a new *.csv file with compiled results
write.csv(snpl.500.rep,'~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.500.rep.csv', row.names=FALSE)

snpl.500.rep <- read.csv('~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.500.rep.csv')

# Change order of kch factor type
snpl.500.rep$kch.type <- factor( snpl.500.rep$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
# Have an exploratory look at the data
ggplot() + geom_point(data=snpl.500.rep, aes(x=fecund,y=exp.min.n, colour=kch.type)) + facet_grid(SLR~RandType)

## Calculate Delta Values 
# First check for consistency of values
which(snpl.500.rep$EigenVal[1:10000]!=snpl.500.rep$EigenVal[10001:20000])
which(snpl.500.rep$EigenVal[20001:30000]!=snpl.500.rep$EigenVal[30001:40000])

# Integer bounds of different SLR and RandType treatments
lhs.nocc <- 1:10000
lhs.2m <- 10001:20000
unif.nocc <- 20001:30000
unif.2m <- 30001:40000

# Calculate Delta-P50 and Delta-EMA
p50.delta <- snpl.500.rep$prob.50[c(lhs.2m,unif.2m)] - snpl.500.rep$prob.50[c(lhs.nocc,unif.nocc)]
ema.delta <- snpl.500.rep$exp.min.n[c(lhs.2m,unif.2m)] - snpl.500.rep$exp.min.n[c(lhs.nocc,unif.nocc)]

# A quick look at ema split by slr, rep.num, and rand.type
ggplot(snpl.500.rep,aes(SLR,exp.min.n)) + geom_boxplot() + facet_grid(RandType~RepNumber)

# Make 'matched' data.frame
snpl.500.rep.match <- cbind(snpl.500.rep[c(lhs.nocc,unif.nocc),], p50.delta, ema.delta)

# A quick look at Delta-P50
ggplot( snpl.500.rep.match, aes( GrowthRt, p50.delta)) + geom_point( aes( colour=kch.type)) + facet_grid(RepNumber~RandType)

# Calculate means for each rep and have a peak
p50.mean.500 <- tapply( snpl.500.rep.match$p50.delta, INDEX = list(snpl.500.rep.match$RepNumber,snpl.500.rep.match$RandType),mean)
plot(p50.mean.500[,1])
points(p50.mean.500[,2],col='red')
title('Delta-P50 Sample Size 500')

##########################################################################################
############## LOADING REPLICATE SAMPLE SIZE 250 RESULTS ########
## WARNING: This section contains many variable names that
## are repeated from above - Be *careful* when executing code
## in this section

# Read file that is a list of all of the res.csv files for 
# the sample size 250 restuls
snpl.250.rf <- read.table('/home/mlammens/Projects/SnowyPlover/SampleSize_250_Rep/res_list.txt')

# Read in each files results
snpl.250.rep <- vector()
for ( rf in snpl.250.rf$V1 ){
  # Set the file name as a character
  rf.name <- as.character(rf)
  # Split the various parts of the file name. These 
  # will be used to assign factor values.
  rf.name.vect <- unlist(strsplit(rf.name,split="/"))
  # Get Randomization technique and replication number
  rf.rand.rep <- unlist(strsplit(rf.name.vect[7],split="_"))
  RandType <- rf.rand.rep[1]
  RepNumber <- rf.rand.rep[2]
  # Get SLR scenario
  rf.slr <- unlist(strsplit(rf.name.vect[8],split="_"))
  SLR <- rf.slr[2]
  # In this 'for' loop, fix the Sample Size to 250
  SampSize <- "250"
  
  # Read results file
  res.temp <- read.csv(rf)
  # Get the number of rows in this file
  rf.rows <- nrow(res.temp)
  # Make a data frame with the Sample Size, RandType, RepNumber, etc. information
  samp.info <- data.frame(RandType=rep(RandType,rf.rows), RepNumber=rep(RepNumber,rf.rows), SLR=rep(SLR,rf.rows), SampSize=rep(SampSize,rf.rows))
  # Combine the two data.frames
  res.temp <- cbind(res.temp,samp.info)
  
  # Bind temporary df to full df
  snpl.250.rep <- rbind(snpl.250.rep,res.temp)
}

# Write a new *.csv file with compiled results
write.csv(snpl.250.rep,'~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.250.rep.csv', row.names=FALSE)

snpl.250.rep <- read.csv('~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.250.rep.csv')


# Change order of kch factor type
snpl.250.rep$kch.type <- factor( snpl.250.rep$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
# Have an exploratory look at the data
ggplot() + geom_point(data=snpl.250.rep, aes(x=fecund,y=exp.min.n, colour=kch.type)) + facet_grid(SLR~RandType)

## Calculate Delta Values 
# First check for consistency of values
which(snpl.250.rep$EigenVal[1:10000]!=snpl.250.rep$EigenVal[10001:20000])
which(snpl.250.rep$EigenVal[20001:30000]!=snpl.250.rep$EigenVal[30001:40000])

############## LOADING REPLICATE SAMPLE SIZE 100 RESULTS ########
## WARNING: This section contains many variable names that
## are repeated from above - Be *careful* when executing code
## in this section

# Read file that is a list of all of the res.csv files for 
# the sample size 100 restuls
snpl.100.rf <- read.table('/home/mlammens/Projects/SnowyPlover/SampleSize_100_Rep/res_list.txt')

# Read in each files results
snpl.100.rep <- vector()
for ( rf in snpl.100.rf$V1 ){
  # Set the file name as a character
  rf.name <- as.character(rf)
  # Split the various parts of the file name. These 
  # will be used to assign factor values.
  rf.name.vect <- unlist(strsplit(rf.name,split="/"))
  # Get Randomization technique and replication number
  rf.rand.rep <- unlist(strsplit(rf.name.vect[7],split="_"))
  RandType <- rf.rand.rep[1]
  RepNumber <- rf.rand.rep[2]
  # Get SLR scenario
  rf.slr <- unlist(strsplit(rf.name.vect[8],split="_"))
  SLR <- rf.slr[2]
  # In this 'for' loop, fix the Sample Size to 100
  SampSize <- "100"
  
  # Read results file
  res.temp <- read.csv(rf)
  # Get the number of rows in this file
  rf.rows <- nrow(res.temp)
  # Make a data frame with the Sample Size, RandType, RepNumber, etc. information
  samp.info <- data.frame(RandType=rep(RandType,rf.rows), RepNumber=rep(RepNumber,rf.rows), SLR=rep(SLR,rf.rows), SampSize=rep(SampSize,rf.rows))
  # Combine the two data.frames
  res.temp <- cbind(res.temp,samp.info)
  
  # Bind temporary df to full df
  snpl.100.rep <- rbind(snpl.100.rep,res.temp)
}

# Write a new *.csv file with compiled results
write.csv(snpl.100.rep,'~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.100.rep.csv', row.names=FALSE)

snpl.100.rep <- read.csv('~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.100.rep.csv')

# Change order of kch factor type
snpl.100.rep$kch.type <- factor( snpl.100.rep$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
# Have an exploratory look at the data
ggplot() + geom_point(data=snpl.100.rep, aes(x=fecund,y=exp.min.n, colour=kch.type)) + facet_grid(SLR~RandType)

## Calculate Delta Values 
# First check for consistency of values
which(snpl.100.rep$EigenVal[1:10000]!=snpl.100.rep$EigenVal[10001:20000])
which(snpl.100.rep$EigenVal[20001:30000]!=snpl.100.rep$EigenVal[30001:40000])

# Integer bounds of different SLR and RandType treatments
lhs.nocc <- 1:10000
lhs.2m <- 10001:20000
unif.nocc <- 20001:30000
unif.2m <- 30001:40000

# Calculate Delta-P50 and Delta-EMA
p50.delta <- snpl.100.rep$prob.50[c(lhs.2m,unif.2m)] - snpl.100.rep$prob.50[c(lhs.nocc,unif.nocc)]
ema.delta <- snpl.100.rep$exp.min.n[c(lhs.2m,unif.2m)] - snpl.100.rep$exp.min.n[c(lhs.nocc,unif.nocc)]

# A quick look at ema split by slr, rep.num, and rand.type
ggplot(snpl.100.rep,aes(SLR,exp.min.n)) + geom_boxplot() + facet_grid(RandType~RepNumber)

# Make 'matched' data.frame
snpl.100.rep.match <- cbind(snpl.100.rep[c(lhs.nocc,unif.nocc),], p50.delta, ema.delta)
rm( ema.delta, p50.delta )

# A quick look at Delta-P50
ggplot( snpl.100.rep.match, aes( GrowthRt, p50.delta)) + geom_point( aes( colour=kch.type)) + facet_grid(RepNumber~RandType)

# Calculate means for each rep and have a peak
p50.mean.100 <- tapply( snpl.100.rep.match$p50.delta, INDEX = list(snpl.100.rep.match$RepNumber,snpl.100.rep.match$RandType),mean)
plot(p50.mean.100[,1])
points(p50.mean.100[,2],col='red')
title('Delta-P50 Sample Size 100')


############## LOADING REPLICATE SAMPLE SIZE 10 RESULTS ########
## WARNING: This section contains many variable names that
## are repeated from above - Be *careful* when executing code
## in this section

## Make Combined File ##
# Read file that is a list of all of the res.csv files for 
# the sample size 10 restuls
snpl.10.rf <- read.table('/home/mlammens/Projects/SnowyPlover/SampleSize_10_Rep/res_list.txt')
# Read in each files results
snpl.10.rep <- vector()
for ( rf in snpl.10.rf$V1 ){
  # Set the file name as a character
  rf.name <- as.character(rf)
  # Split the various parts of the file name. These 
  # will be used to assign factor values.
  rf.name.vect <- unlist(strsplit(rf.name,split="/"))
  # Get Randomization technique and replication number
  rf.rand.rep <- unlist(strsplit(rf.name.vect[7],split="_"))
  RandType <- rf.rand.rep[1]
  RepNumber <- rf.rand.rep[2]
  # Get SLR scenario
  rf.slr <- unlist(strsplit(rf.name.vect[8],split="_"))
  SLR <- rf.slr[2]
  # In this 'for' loop, fix the Sample Size to 10
  SampSize <- "10"
  
  # Read results file
  res.temp <- read.csv(rf)
  # Get the number of rows in this file
  rf.rows <- nrow(res.temp)
  # Make a data frame with the Sample Size, RandType, RepNumber, etc. information
  samp.info <- data.frame(RandType=rep(RandType,rf.rows), RepNumber=rep(RepNumber,rf.rows), SLR=rep(SLR,rf.rows), SampSize=rep(SampSize,rf.rows))
  # Combine the two data.frames
  res.temp <- cbind(res.temp,samp.info)
  
  # Bind temporary df to full df
  snpl.10.rep <- rbind(snpl.10.rep,res.temp)
}

# Write a new *.csv file with compiled results
write.csv(snpl.10.rep,'~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.10.rep.csv', row.names=FALSE)
## End Making Rep File

snpl.10.rep <- read.csv('~/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.10.rep.csv')

# Change order of kch factor type
snpl.10.rep$kch.type <- factor( snpl.10.rep$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))
# Have an exploratory look at the data
ggplot() + geom_point(data=snpl.10.rep, aes(x=fecund,y=exp.min.n, colour=kch.type)) + facet_grid(SLR~RandType)

## Calculate Delta Values 
# First check for consistency of values
which(snpl.10.rep$EigenVal[1:10000]!=snpl.10.rep$EigenVal[10001:20000])
which(snpl.10.rep$EigenVal[20001:30000]!=snpl.10.rep$EigenVal[30001:40000])

# Integer bounds of different SLR and RandType treatments
lhs.nocc <- 1:10000
lhs.2m <- 10001:20000
unif.nocc <- 20001:30000
unif.2m <- 30001:40000

# Calculate Delta-P50 and Delta-EMA
p50.delta <- snpl.10.rep$prob.50[c(lhs.2m,unif.2m)] - snpl.10.rep$prob.50[c(lhs.nocc,unif.nocc)]
ema.delta <- snpl.10.rep$exp.min.n[c(lhs.2m,unif.2m)] - snpl.10.rep$exp.min.n[c(lhs.nocc,unif.nocc)]

# A quick look at ema split by slr, rep.num, and rand.type
ggplot(snpl.10.rep,aes(SLR,exp.min.n)) + geom_boxplot() + facet_grid(RandType~RepNumber)

# Make 'matched' data.frame
snpl.10.rep.match <- cbind(snpl.10.rep[c(lhs.nocc,unif.nocc),], p50.delta, ema.delta)

# A quick look at Delta-P50
ggplot( snpl.10.rep.match, aes( GrowthRt, p50.delta)) + geom_point( aes( colour=kch.type)) + facet_grid(RepNumber~RandType)

# Calculate means for each rep and have a peak
p50.mean.10 <- tapply( snpl.10.rep.match$p50.delta, INDEX = list(snpl.10.rep.match$RepNumber,snpl.10.rep.match$RandType), mean)
plot(p50.mean.10[,1])
points(p50.mean.10[,2],col='red')
title('Delta-P50 Sample Size 10')

##########################################################################################
## Combine the p50.mean.X tables
# Make sample size factor column
samp.size <- c( rep(10,1000), rep(100,100), rep(500,20), rep(1000,10) ) 
# rbind tables
p50.mean <- rbind(p50.mean.10, p50.mean.100, p50.mean.500, p50.mean.1000)
# Make data frame
p50.mean.df <- data.frame( lhs=p50.mean[,1], unif=p50.mean[,2], samp.size=as.factor(samp.size) ) 

lhs.p50.mean <- tapply(p50.mean.df$lhs, p50.mean.df$samp.size, mean)
lhs.p50.sd <- tapply(p50.mean.df$lhs, p50.mean.df$samp.size, sd)
unif.p50.mean <- tapply(p50.mean.df$unif, p50.mean.df$samp.size, mean)
unif.p50.sd <- tapply(p50.mean.df$unif, p50.mean.df$samp.size, sd)

rand = rep(c('lhs','unif'),each=4)
sims = rep( factor(c(10,100,500,1000)), 2)
#sims = factor(c(9,99,499,999,11,101,501,1001))
df <- data.frame( rand = rand, sims = sims, p50.mean = c(lhs.p50.mean,unif.p50.mean) , p50.se = c(lhs.p50.sd,unif.p50.sd) )
limits <- aes(ymax = p50.mean + p50.se, ymin = p50.mean - p50.se)
#ggplot(df, aes(y=p50.mean,x=sims, colour=rand)) + geom_point() + geom_errorbar(limits,width=0.2) 

#ggplot() + geom_pointrange(data=df, aes(x=sims,y=p50.mean,ymin=(p50.mean-p50.se),ymax=(p50.mean+p50.se),colour=rand), position=position_jitter(w=.3))
#
# Look at p50.delta's in boxplot form
ggplot( p50.mean.df, aes(samp.size,lhs)) + geom_boxplot()
ggplot( p50.mean.df, aes(samp.size,unif)) + geom_boxplot()



##########################################################################################
############## LOADING 10 REPLICATIONS OF SAMP SIZE 100 ########
## WARNING: This section contains many variable names that
## are repeated from above - Be *careful* when executing code
## in this section

# This work done on Linus-Mac
# Read file in dropbox of all csv files
snpl.100lhs.rf <- read.table('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/SampleSize_100_Rep/res_list.txt')
snpl.nocc.100lhs.rf <- as.character(snpl.100lhs.rf$V1[11:20])
snpl.2m.100lhs.rf <- as.character(snpl.100lhs.rf$V1[1:10])
rm(snpl.100lhs.rf)

# Load NoCC results
snpl.nocc.100lhs <- vector()
for ( rf in snpl.nocc.100lhs.rf ){
  res.temp <- read.csv(rf)
  snpl.nocc.100lhs <- rbind(snpl.nocc.100lhs,res.temp)
}
# Add a factor for the replication number
rep.num <- rep(factor(1:10),each=100)
snpl.nocc.100lhs <- cbind(snpl.nocc.100lhs,rep.num)
snpl.nocc.100lhs$kch.type <- factor( snpl.nocc.100lhs$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))

# Load 2m results
snpl.2m.100lhs <- vector()
for ( rf in snpl.2m.100lhs.rf ){
  res.temp <- read.csv(rf)
  snpl.2m.100lhs <- rbind(snpl.2m.100lhs,res.temp)
}
rep.num <- rep(factor(1:10),each=100)
snpl.2m.100lhs <- cbind(snpl.2m.100lhs,rep.num)
snpl.2m.100lhs$kch.type <- factor( snpl.2m.100lhs$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))

# Check values are infact the same for input parameters
which(snpl.2m.100lhs$EigenVal!=snpl.nocc.100lhs$EigenVal)

snpl.100lhs <- rbind(snpl.nocc.100lhs,snpl.2m.100lhs)
cc <- rep(c('nocc','slr2m'),each=1000)
snpl.100lhs <- cbind(snpl.100lhs,cc)

# A quick look at the expected min abundance split by cc scenario and rep number
ggplot(data=snpl.100lhs,aes(cc,exp.min.n)) + geom_boxplot() + facet_grid(.~rep.num)

# Calculate differences between nocc and slr2m measurements
p50.delta <- snpl.2m.100lhs$prob.50 - snpl.nocc.100lhs$prob.50
ema.delta <- snpl.2m.100lhs$exp.min.n - snpl.nocc.100lhs$exp.min.n
snpl.match.100lhs <- cbind(snpl.nocc.100lhs,ema.delta,p50.delta)

# Another quick look at some of these results - P50 vs Growth Rt, colored by KCH and faceted by Rep Num
ggplot( snpl.match.100lhs, aes( GrowthRt, p50.delta) ) + geom_point(aes(colour = kch.type)) + facet_grid(rep.num~.)

# Looking at means and standard deviations of P50 for each replication
p50.mean <- tapply(snpl.match.100lhs$p50.delta,snpl.match.100lhs$rep.num,mean)
# Is this next line meaningful?
p50.sd <- tapply(snpl.match.100lhs$p50.delta,snpl.match.100lhs$rep.num,sd)

df <- data.frame(rep.num=factor(1:10), p50.mean)
ggplot(df, aes(y=p50.mean,x=rep.num)) + geom_point()


##########################################################################################
#### BEGIN BRT ANALYSIS ####
##
## 2 June 2012
## Analysis of importance of variables using Boosted Regresion Trees
## sensu Elith and Leathwick
require(dismo)
require(gbm)

## Also load ggplot2 if not already in:
require(ggplot2)

#### SAMPLE SIZE 10000 ####
## First, read in data.  I began by using lines above beginning around 220
## to read in the SampleSize 10,000 results for both the lhs and unif
## randomization results.  

## Get the column names for the data set
names(snpl.match.10000lhs)

## Setup and try a BRT Model

## Set predictor variables
gbm.predictors <- c(2,10,11,63,64,62,48,61)
## Set response variable
# 65 = Delta EMA
# 66 = Delta Prob Delcine to 50
gbm.response <- 66

snpl.lhs.brt <- gbm.step( data=snpl.match.10000lhs, gbm.x=gbm.predictors, gbm.y=gbm.response,
                          family="gaussian", tree.complexity=4, learning.rate=0.01,
                          bag.fraction=0.5) # Consider what learning.rate we should use
                          # Originally used 0.01, but tried 0.001 to be consistent with other runs
                          # 0.001 did not produce a gain as good as 0.01 for this model

snpl.unif.brt <- gbm.step( data=snpl.match.10000unif, gbm.x=gbm.predictors, gbm.y=gbm.response,
                          family="gaussian", tree.complexity=4, learning.rate=0.01,
                          bag.fraction=0.5)

rel.inf.10k.lhs <- summary(snpl.lhs.brt)
rel.inf.10k.unif <- summary(snpl.unif.brt)

par(mfrow=c(3,3))
gbm.plot( snpl.lhs.brt, n.plots=8, write.title=FALSE)
gbm.plot.fits( snpl.lhs.brt)
find.int <- gbm.interactions( snpl.lhs.brt )
find.int$interactions
find.int$rank.list
gbm.perspec( snpl.lhs.brt, 5, 4, y.range=c(0,1), z.range=c(0,1) )

## Look at absolute measures for four scenarios - nocc-lhs, nocc-unif, 2m-lhs, 2m-unif
gbm.response = 25 # Prob decline to 50
## nocc-lhs
snpl.nocc.10k.lhs.brt <- gbm.step( data=snpl.nocc.10000lhs, gbm.x=gbm.predictors, gbm.y=gbm.response,
                                 family="gaussian", tree.complexity=4, learning.rate=0.01,
                                 bag.fraction=0.5)
## nocc-unif
snpl.nocc.10k.unif.brt <- gbm.step( data=snpl.nocc.10000unif, gbm.x=gbm.predictors, gbm.y=gbm.response,
                                   family="gaussian", tree.complexity=4, learning.rate=0.01,
                                   bag.fraction=0.5)
## 2m-lhs
snpl.2m.10k.lhs.brt <- gbm.step( data=snpl.2m.10000lhs, gbm.x=gbm.predictors, gbm.y=gbm.response,
                              family="gaussian", tree.complexity=4, learning.rate=0.01,
                              bag.fraction=0.5)
## 2m-unif
snpl.2m.10k.unif.brt <- gbm.step( data=snpl.2m.10000unif, gbm.x=gbm.predictors, gbm.y=gbm.response,
                                 family="gaussian", tree.complexity=4, learning.rate=0.01,
                                 bag.fraction=0.5)

## Save brts for later use
save( snpl.nocc.10k.lhs.brt, snpl.nocc.10k.unif.brt,
      snpl.2m.10k.lhs.brt, snpl.2m.10k.unif.brt, 
      file = "Results/snpl.10k.absolute.brt.RData")

## First bring in the data and do a few minipulations. Some of these are the same
## as in an above section, but repeated here
snpl.1000.rep <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.1000.rep.csv')
snpl.100.rep <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.100.rep.csv')
snpl.10.rep <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.10.rep.csv')
snpl.500.rep <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.500.rep.csv')

## This is a bit of a hack, but what I am doing below is setting one of the
## data sets to a 'current' data set, and running all of the brt analyses 
## on that particular data set at that time.
snpl.curr.rep <- snpl.100.rep
snpl.curr.rep$kch.type <- factor( snpl.curr.rep$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))

lhs.nocc <- 1:10000
lhs.2m <- 10001:20000
unif.nocc <- 20001:30000
unif.2m <- 30001:40000
## Seperate out lhs and unif into different data sets
snpl.curr.rep.lhs.nocc <- snpl.curr.rep[lhs.nocc,]
snpl.curr.rep.lhs.2m <- snpl.curr.rep[lhs.2m,]
snpl.curr.rep.unif.nocc <- snpl.curr.rep[unif.nocc,]
snpl.curr.rep.unif.2m <- snpl.curr.rep[unif.2m,]
## Make matched data set
p50.delta <- snpl.curr.rep$prob.50[c(lhs.2m,unif.2m)] - snpl.curr.rep$prob.50[c(lhs.nocc,unif.nocc)]
ema.delta <- snpl.curr.rep$exp.min.n[c(lhs.2m,unif.2m)] - snpl.curr.rep$exp.min.n[c(lhs.nocc,unif.nocc)]
snpl.curr.rep.match <- cbind(snpl.curr.rep[c(lhs.nocc,unif.nocc),], p50.delta, ema.delta)
## Clean up a little
rm(p50.delta)
rm(ema.delta)
## Seperate out lhs and unif into different MATCHED data sets
snpl.curr.rep.match.lhs <- snpl.curr.rep.match[1:10000,]
snpl.curr.rep.match.unif <- snpl.curr.rep.match[10001:20000,]

## gbm.predictors are the same as previous
## gbm.response: 
## 69 = p50.delta
## 70 = ema.delta
gbm.response <- 69

## For Absolute meausres
## 25 = Prob.50
## 29 = EMA
gbm.response <- 25

## Here's another little hack, setting the brt.run df to a desired data.frame
#snpl.brt.run <- snpl.curr.rep.match.lhs
snpl.brt.run <- snpl.curr.rep.match.lhs

RepNum <- unique( snpl.brt.run$RepNumber )

snpl.brt.summ.list <- vector(mode="list",length=0)
snpl.brt.int.list <- vector(mode="list",length=0)
for ( Rep in 1:length(RepNum) ) {
  print('### REP ###');print(Rep)
  snpl.sub <- subset( snpl.brt.run, subset=RepNumber==RepNum[Rep] )
  snpl.sub.brt <- gbm.step( snpl.sub, gbm.x=gbm.predictors, gbm.y=gbm.response,
                            family="gaussian", tree.complexity=4, learning.rate=0.001,
                            bag.fraction=0.5)#, tolerance.method="fixed",tolerance=0.001)
  # Put a while loop here that checks for whether the brt actually fit the
  # data. If it did not, then the snpl.sub.brt variable will be NULL
  while( is.null( snpl.sub.brt ) ){
    # Print a warning message
    print('### Warning: no model fit - retrying now ###')
    snpl.sub.brt <- gbm.step( snpl.sub, gbm.x=gbm.predictors, gbm.y=gbm.response,
                              family="gaussian", tree.complexity=4, learning.rate=0.001,
                              bag.fraction=0.5)#, tolerance.method="fixed",tolerance=0.001)
  }
  
  snpl.brt.summ.list[[ Rep ]] <- summary( snpl.sub.brt )
  snpl.brt.int.list[[ Rep ]] <- gbm.interactions( snpl.sub.brt )
}
# Grab the 'names' of the predictor variables
pred.vars <- names(snpl.brt.run)[gbm.predictors]
## The above for loop took about 15 minutes, but required 2GB of RAM

snpl.brt.summ.1000.unif <- snpl.brt.summ.list
#snpl.brt.summ.1000.lhs <- snpl.brt.summ.list
#snpl.brt.summ.100.unif <- snpl.brt.summ.list
#snpl.brt.summ.100.lhs <- snpl.brt.summ.list
#snpl.brt.summ.500.lhs <- snpl.brt.summ.list
#snpl.brt.summ.500.unif <- snpl.brt.summ.list

## Function to extract rank.list interaction values from a list of gmb.interaction results
rank.list.extract <- function( int.list ){
  return( int.list$rank.list )
}
lapply( snpl.brt.int.list,FUN=rank.list.extract)

## Here's another way to extract variable importance values, creating
## a data.frame.

# Allow for assignment of snpl.brt.summ.list here
snpl.brt.summ.list <- snpl.brt.summ.1000.unif
#snpl.brt.summ.list <- snpl.brt.summ.1000.lhs
#snpl.brt.summ.list <- snpl.brt.summ.100.unif
#snpl.brt.summ.list <- snpl.brt.summ.100.lhs
#snpl.brt.summ.list <- snpl.brt.summ.500.lhs
#snpl.brt.summ.list <- snpl.brt.summ.500.unif

# Put together a data.frame of the relative influence values
# This hack throws back a few naming warnings, but works none the less
# Start by grabbing the first data.frame
pred.rel.inf.df <- snpl.brt.summ.list[1]
# Use a for loop and merge function to put togehter the df
for ( rel.inf.index in 2:length(snpl.brt.summ.list) ) {
  pred.rel.inf.df <- merge( pred.rel.inf.df,snpl.brt.summ.list[rel.inf.index], by="var")
}

# Add on the LHS 10K results 
pred.rel.inf.df <- merge(pred.rel.inf.df, rel.inf.10k.lhs, by="var")

pred.rel.inf.df <- merge(pred.rel.inf.df, summary(snpl.nocc.10k.lhs.brt), by="var")
# The first column of the new data.frame is the variable names.
# We should use this at the row names, and get rid of this column instead.
row.names(pred.rel.inf.df) <- pred.rel.inf.df[[1]]
pred.rel.inf.df <- pred.rel.inf.df[-1]

pred.rel.inf.colNames <- apply( cbind( rep("rel.inf", length(snpl.brt.summ.list)), RepNum ),
                                1, paste, collapse=".")
pred.rel.inf.colNames <- c(pred.rel.inf.colNames,"rel.inf.10k.lhs")
names(pred.rel.inf.df) <- pred.rel.inf.colNames



# Create a function that does a simple correlation analysis between
# the each brts relative influence values and the 'Assumed True' 
# values (i.e. the 10k scenarios)

snpl.1000.lhs.corr.mat <- cor(pred.rel.inf.df)
snpl.1000.unif.corr.mat <- cor(pred.rel.inf.df)
snpl.100.unif.corr.mat <- cor(pred.rel.inf.df)
snpl.100.lhs.corr.mat <- cor(pred.rel.inf.df)
snpl.500.lhs.cor.mat <- cor(pred.rel.inf.df)
snpl.500.unif.cor.mat <- cor(pred.rel.inf.df)

snpl.nocc.1k.lhs.corr.mat <- cor(pred.rel.inf.df)
snpl.nocc.100.lhs.corr.mat <- cor(pred.rel.inf.df)
snpl.nocc.100.unif.cor.mat <- cor(pred.rel.inf.df)

##########################################################################################
# Reading in csv files in which I saved the correlation matrices for the 
# 1000, 500, and 100 rep scenarios
#
base.dir <- '/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results'
# File names with paths
cor.mat.list.names <- list.files( path = base.dir, pattern = ".*cor.mat.csv", full.name=TRUE )
# File names without paths
cor.mat.list.names.short <- list.files( path = base.dir, pattern = ".*cor.mat.csv" )
# Clean the names a bit more
cor.mat.list.names.short <- sapply(cor.mat.list.names.short, gsub, pattern=".cor.mat.csv", replacement="")

# Make list of csv tables
cor.mat.list <- lapply(cor.mat.list.names,read.csv, row.names=1)
names(cor.mat.list) <- cor.mat.list.names.short

# FUNCTION: Extract last row of the correlation matrix, trip the last element in the vector,
# and return the vector
trim.cor.mat <- function( cor.mat ) {
  # Get the number of columns in the correlation matrix
  cor.mat.ncol <- ncol( cor.mat )
  # Extract last column, all rows except last row
  cor.vec <- cor.mat[1:(cor.mat.ncol-1),cor.mat.ncol]
  # Return cor.vec
  return(cor.vec)
}

# Use the trim.cor.mat function to extract the last column of each matrix
cor.vec.list <- lapply( cor.mat.list, trim.cor.mat )

# Calculate the mean and standard deviation of the correlation vectors
cor.vec.mean <- sapply( cor.vec.list, mean )
cor.vec.sd <- sapply( cor.vec.list, sd )

# Create a data.frame with all of the results
cor.vec.df <- vector(length=0)
for ( vec.index in 1:length(cor.vec.list) ) {
  vec <- cor.vec.list[[vec.index]]
  vec.name <- cor.mat.list.names.short[vec.index]
  cor.vec.df.temp <- data.frame( samp.dim = rep( vec.name, length(vec) ),   cor.val = vec )
  cor.vec.df <- rbind(cor.vec.df,cor.vec.df.temp)
}

cor.vec.df$samp.dim <- factor( cor.vec.df$samp.dim, c('snpl.100.lhs','snpl.100.unif',
                                                      'snpl.500.lhs','snpl.500.unif',
                                                      'snpl.1000.lhs','snpl.1000.unif'))

ggplot() + geom_boxplot(aes(x=samp.dim,y=cor.val), data=cor.vec.df) +
  opts(title="Correlation of Importance Values") + xlab("Sampling type and number of dimensions") +
  ylab("Correlation value with LHS dims = 10K")


#### Development Work ####
snpl.lhs.brt <- gbm.step( data=snpl.lhs.10k, gbm.x=gbm.predictors, gbm.y=gbm.response,
                          family="gaussian", tree.complexity=4, learning.rate=0.01,
                          bag.fraction=0.5)

snpl.lhs.brt <- gbm.step( data=snpl.lhs.10k, gbm.x=gbm.predictors, gbm.y=gbm.response,
                          family="gaussian", tree.complexity=4, learning.rate=0.1,
                          bag.fraction=0.5, step.size=2, n.trees=1, tolerance.method="fixed",
                          tolerance=0.1, max.trees=100)


## Function to extract particular rel.inf values based on variable (var) names
## This section of code extracts values from each of the snpl.brt.summ.list 
## elements, which are data.frames, for individual variables.
rel.inf.extract <- function( var, df ){
  return( df$rel.inf[ which( df$var==var ) ] )
}
pred.rel.inf.vals <- vector()
for ( pvar in pred.vars ){
  print(pvar)
  p.rel.inf.temp <- vector()
  for ( summ in snpl.brt.summ.list ){
    print(rel.inf.extract(var=pvar,df=summ))
    p.rel.inf.temp <- c( p.rel.inf.temp, rel.inf.extract(var=pvar,df=summ))
  }
  pred.rel.inf.vals <- cbind(pred.rel.inf.vals,p.rel.inf.temp)
}
pred.rel.inf.vals <- as.data.frame(pred.rel.inf.vals)
names(pred.rel.inf.vals) <- pred.vars
