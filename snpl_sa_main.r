## **************************************************************##
## File: snpl_sa_main.r
## Author: Matthew Aiello-Lammens
## Data: 2 January 2013
##
## Purpose: This file includes both R code and references to 
## other R functions to create the files and run the analyses
## for the sensitivity analysis investigation of the snowy
## plover model, utilizing the sensitivity analysis scripts
## I have been writing and implementing over the past year.
##
## Much of the work that is presented here was originally
## developed and documented in snpl_sensitivity_worklog.R
##
## Warnings and Caveats:
## - The execution of this script was carried out on the Akcakaya
##   Group lab computer. Some of the commands are Windows 
##   specific (e.g. 'shell'). Also, *.mp files created by
##   the 'sensitivity.r' script run on a Windows machine
##   have MS TXT format, which is necessary for execution
##   by the Metapop.exe module. If this is run on a UNIX
##   or Mac machine, the format is *nix and must first be 
##   converted to Windows format.
##
## Major Sections of this Script:
##   Section 2: Make replicate directories 
##   Set up directory structure and copy files into directories. 
##
##   Section 3: Run sensitivity.r for each file in the list of
##   sens.cfg files 
## 
##   Section 4: Run all of the *.mp files created in previous step
##
## **************************************************************##
## Section 1: Preliminaries and Parameter Specification
##
## Setting the appropriate working directory:
## ------------------------------------------
## I have been working on this project over three computers - my
## MacBook, the lab Windows box, and my personal Linux box. Currently
## all of the files are stored in three locations:
## 1 - Akcakaya Lab group computer - /Projects/SnowyPlover
## 2 - Seagate External Disk labeled Akcakaya-2 - [Volume name]/Projects/SnowyPlover
## 3 - Personal Linux Box - /Projects/SnowyPlover
## Because of this distributed computer use, the main working directory
## often changes, but thus far it always includes the main SnowyPlover
## directory (folder)
#snpl.project.dir <- '/Volumes/AKCAKAYA-2/Projects/SnowyPlover/'
#snpl.project.dir <- '/media/AKCAKAYA-2/Projects/SnowyPlover/'
#snpl.project.dir <- '/Projects/SnowyPlover/'
#setwd( snpl.project.dir )

## Snowy Plover Sensitivity Analysis Code directory:
## -------------------------------------------------
## At the beginning of this project I branched off a copy of the 
## SACode to be Snowy Plover specific.  In general, this specificity
## is no longer required, but I am continuing to work from this
## spin-off directory.  This directory is in my DropBox, which 
## means I can access it from any computer I'm working on, but the
## path changes depending on what system I'm on.
sacode.snpl <- '/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/SACode_SNPL/'
#sacode.snpl <- '/Users/Matthew/Documents/My Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/SACode_SNPL/'

## Snowy Plover Sensitivity Analysis  directory:
## ----------------------------------------------------
## This is the directory/folder that the main SNPL SA
## scripts (ie this one) are being stored in. Currently this resides
## in my DropBox. This var name here matches what is used 
## in snpl_sa_development.r as well.
sa.sens.dir <- '/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/' # Linus (Mac)
#sa.sens.dir <- '/Users/Matthew/Documents/My Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/' # Akcakaya-Lab (Windows)

## Snowy Plover Sensitivity Analysis Results directory:
## ----------------------------------------------------
## This is the directory/folder that all SNPL SA
## results are being stored in. Currently this resides
## in my DropBox
snpl.SAres.dir <- '/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/'
#snpl.SAres.dir <- '/Users/Matthew/Documents/My Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/'

## The following is a list of procedures that have been carried
## out outside of this script, that might be incorporated later.
## 1. Template directories were created which contain the *.mp,
##   *.kch, sens.config, and Metapop symbolic link files needed
##   for each subdirectory

## **************************************************************##
## Section 2: Make replicate directories
##
## Location of template files, from snpl.project.dir
## -------------------------------------------------
## Two sensitivity analyses have been/ will be run for the snpl
## project.  One using the range of parameters as outlined in 
## Aiello-Lammens et al. 2011 GCB (estimated based on best available
## data - SA1) and a second that uses constrained parameters (some parameter
## values are constricted in their possible values). The second
## analysis (SA2) is being run so that a set of *.mps is generated in which 
## uncertainty is not *driven* by only a small set of parameter 
## uncertainties.
## In both analyses there are two sea-level rise scenarios that 
## affect carrying capacity through time (represented in the KCH files):
##   NoCC: No Climate-change - there is no change in carrying capacity
##   through time
##   2M: 2m sea-level rise - this was the extreme scenario in our analyses
##   for the SERDP project, but is quite reasonable
##
# Templates directory
template.dir <- paste( snpl.project.dir, 'Templates', sep="" )
# Templates for SA1
nocc.template.dir <- paste( template.dir, 'FLSNPL_NoCC_Ceiling', sep="/" )
nocc.temp.files <- list.files( path=nocc.template.dir, full.names=TRUE )
slr.template.dir <- paste( template.dir, 'FLSNPL_2M_Ceiling', sep="/" )
slr.temp.files <- list.files( path=slr.template.dir, full.names=TRUE )
# Templates for SA2
nocc.SA2.template.dir <- paste( template.dir, 'FLSNPL_NoCC_Ceiling_SA2', sep="/" )
nocc.SA2.temp.files <- list.files( path=nocc.SA2.template.dir, full.names=TRUE )
slr.SA2.template.dir <- paste( template.dir, 'FLSNPL_2M_Ceiling_SA2', sep="/" )
slr.SA2.temp.files <- list.files( path=slr.SA2.template.dir, full.names=TRUE )

## Sensitivity analysis configuration file:
## This file provides several important parameters to the sensitivity.r 
## script (the main sensitivity analysis script). For the purposes of
## this project, the following parameters are changed between simulation
## replications: 
##   sens.iter = number of *.mp files created, equal to the 'Partition 
##   Number' in this project
##   rand.samp = Sampling scheme for random variables, either 'lhs' or 'urand'
##   for this project
##   use.rv.file = TRUE/FALSE - allows for the use of random variables
##   previously generated. This is used to match rvs between NoCC and
##   2M-SLR scenarios in our case.
sens.cfg.file <- paste( sacode.snpl, 'sens.cfg.snpl.txt', sep="" )
# Check that file exists
if( !file.exists( sens.cfg.file ) ) {
  stop( paste( "Could not find sens.config file: ", sens.cfg.file ) )
}

## List of Partition Number scenarios to run.  In previous runs,
## particularly SA1, Partition Number was called 'SampleSize'.  I 
## have decided that Partition Number, or 'PartNum', is a more
## appropriate name, and will use these naming convention for SA2.
part.num.toRun <- c(1000, 500, 250, 100)
#### Sensitivity Analysis run
###sa.toRun <- 'SA2'
# SLR Scenarios to run
slr.scen.toRun <- c('LHS','Unif')

## Make directory names to create:
## The directory structure should be as follows -
## [snpl.project.dir]/PartNum_[part.num]_Rep/[LHS or Unif]_[rep.num]/FLSNPL_[NoCC or 2M]_Ceiling

# First make new PartNum dirs - **here for SA2**
new.PartNum.dirs <- cbind( rep( "PartNum", length(part.num.toRun)), part.num.toRun,
                           rep( "SA2", length(part.num.toRun)) )
new.PartNum.dirs <- apply( new.PartNum.dirs, 1, paste, collapse="_" )

# A function to the parameters of the sens.cfg file
set.sens.cfg <- function( dir.name, sens.cfg ){
  # Get the template *.mp files from this dir
  mp.files <- list.files( path=dir.name, pattern='FLSNPL.*mp',
                          full.names=TRUE)
  # mp files should be in the following order FLSNPL_LO.mp, FLSNPL_ME.mp, FLSNPL_HI.mp
  # WARNING: This next step assumes the naming convention has remained constant
  # so the order of the mp.files is known before hand.
  mp.files <- mp.files[c(2,3,1)]  
  mp.files <- paste( mp.files, collapse="," )
  # Change out the mp files in the sens.cfg file
  sens.cfg <- sub('^mp.file.names.*', paste('mp.file.names = ', mp.files, sep=""), sens.cfg)
  
  # Change the out.dir
  sens.cfg <- sub('^out.dir.*', paste('out.dir = ', dir.name, sep=""), sens.cfg)
  
  # Change the location of the rv.file
  rv.file.dir <- dirname( dir.name )
  rv.file.full <- paste( rv.file.dir, 'snpl_rv.csv', sep="/" )
  sens.cfg <- sub('^rv.file.*', paste('rv.file = ', rv.file.full, sep=""), sens.cfg)
  
  # Set the appropriate sampling scheme
  if( grepl( "LHS", dir.name ) ){
    sens.cfg <- sub('^rand.samp.*', paste('rand.samp = ', 'lhs', sep=""), sens.cfg)
  } else if( grepl( "Unif", dir.name ) ){
    sens.cfg <- sub('^rand.samp.*', paste('rand.samp = ', 'urand', sep=""), sens.cfg)
  } else {
    stop( paste( "Directory path did not contain LHS or Unif tag: ", dir.name) )
  }
  return( sens.cfg )
}

## Loop through part.num.toRun, creating directories and copying
## necessary files
for ( PartNum in 1:length(part.num.toRun) ){
  ## ************************************************************##
  ## Create Appropriate Directories
  ##
  # Create the PartNum directory if it does not exist
  if ( !file.exists(new.PartNum.dirs[ PartNum ]) ){
    dir.create( new.PartNum.dirs[ PartNum ] )
  }
  # Determine the appropriate number of replicate directories
  PartNum.reps <- 10000/part.num.toRun[ PartNum ]
  # Make replicate directory names to create
  new.PartNumRep.dirs <- cbind( rep( slr.scen.toRun, each=PartNum.reps ),
                                rep( 1:PartNum.reps, length(slr.scen.toRun) ))
  new.PartNumRep.dirs <- apply( new.PartNumRep.dirs, 1, paste, collapse="_" )
  # Add the PartNum directory to this path
  new.PartNumRep.dirs <- cbind( rep( new.PartNum.dirs[ PartNum ], length(new.PartNumRep.dirs)),
                                new.PartNumRep.dirs)
  new.PartNumRep.dirs <- apply( new.PartNumRep.dirs, 1, paste, collapse="/" )
  sapply( new.PartNumRep.dirs, dir.create )
  # Repeat this list once each the two SLR scenarios
  new.PartNumRep.dirs <- cbind( rep( new.PartNumRep.dirs, 2 ),
                                rep( c("FLSNPL_NoCC_Ceiling","FLSNPL_2M_Ceiling"),
                                     each=length(new.PartNumRep.dirs)))
  new.PartNumRep.dirs <- apply( new.PartNumRep.dirs, 1, paste, collapse="/" )
  sapply( new.PartNumRep.dirs, dir.create )
  ## ************************************************************##
  ## Copy template files to appropriate directories
  ##
  # Read in the sens.cfg file and make a few replacements
  sens.cfg <- readLines( sens.cfg.file )
  # Change the sens.iter line
  sens.cfg <- sub('^sens.iter.*', paste('sens.iter = ', part.num.toRun[PartNum], sep=""), sens.cfg)
  # The first half of the new.PartNumRep.dirs vector is paths
  # to FLSNPL_NoCC_Ceiling directories and second half is paths
  # to FLSNPL_2M_Ceiling directories.
  nocc.dirs <- new.PartNumRep.dirs[1:length(new.PartNumRep.dirs)/2]
  slr.dirs <- new.PartNumRep.dirs[(length(new.PartNumRep.dirs)/2+1):length(new.PartNumRep.dirs)]
  # Loop through these two lists, copying the appropriate files
  # Also change sens.cfg file based on nocc vs slr
  # NoCC runs first, and creates the rv.file to be used
  sens.cfg <- sub('^use.rv.file.*', paste('use.rv.file = ', 'FALSE', sep=""), sens.cfg)
  for ( nocc.dir in nocc.dirs ){ 
    # Copy *.mp and *.kch files
    file.copy( nocc.SA2.temp.files, nocc.dir )
    # Change sens.cfg for appropriate sampling scheme
    sens.cfg <- set.sens.cfg( nocc.dir, sens.cfg )
    # Write new sens.cfg file
    writeLines( sens.cfg, paste( nocc.dir, 'sens.cfg.txt', sep="/" ) )
  }
  # 2M SLR runs use already created rv.file
  sens.cfg <- sub('^use.rv.file.*', paste('use.rv.file = ', 'TRUE', sep=""), sens.cfg)  
  for ( slr.dir in slr.dirs ){ 
    # Copy *.mp and *.kch files
    file.copy( slr.SA2.temp.files, slr.dir ) 
    # Change sens.cfg for appropriate sampling scheme
    sens.cfg <- set.sens.cfg( slr.dir, sens.cfg )
    # Write new sens.cfg file
    writeLines( sens.cfg, paste( slr.dir, 'sens.cfg.txt', sep="/" ) )
  }
}

## **************************************************************##
## Section 3: Run sensitivity.r for each file in the list of
## sens.cfg files
## This analysis was run on Jan 6 & 7 2013 on the Akcakaya Group Computer

# To do this process more quickly, run on multiple cores
# using doSnow and foreach
require(doSNOW)
require(foreach)
# Setup cluster
#cl <- makeCluster(2,"SOCK") # For laptop - devel work
cl <- makeCluster(7,"SOCK") # For group computer
registerDoSNOW(cl)

# Run NoCC files first, since these create the snpl_rv.csv files
# Get list of all sens.cfg.nocc files
sens.cfg.nocc.list <- Sys.glob(paths="PartNum*/*/*NoCC*/sens.cfg*")
foreach( sa = 1:length(sens.cfg.nocc.list) ) %dopar% {
  
  # Set up SA scripts
  source( paste(sacode.snpl,'sensitivity.setup.r',sep="") )
  sensitivity.setup( sacode.snpl )
  
  # Start sensitivity analysis
  sensitivity( sens.cfg.nocc.list[sa] )
}
# Get list of all sens.cfg.2m files
sens.cfg.2m.list <- Sys.glob(paths="PartNum*/*/*2M*/sens.cfg*")
foreach( sa = 1:length(sens.cfg.2m.list) ) %dopar% {
  
  # Set up SA scripts
  source( paste(sacode.snpl,'sensitivity.setup.r',sep="") )
  sensitivity.setup( sacode.snpl )
  
  # Start sensitivity analysis
  sensitivity( sens.cfg.2m.list[sa] )
}

## **************************************************************##
## Section 4: Run all of the *.mp files created in previous step
## WARNING: This section is in development!

## It is possible to call the RAMAS Metapop program from within 
## an R script.  Here, I am writing this script so that it works on
## the Akcakaya Group computer (Windows 7), using the 'shell' function,
## as opposed to the 'system' function. 'shell' appears to be a Windows
## specific function in R.
## Also note that I am using the developmental command
## line version of RAMAS Metapop. This version is an 
## order of magnitude faster than the traditional GUI
## for the snpl files.

# Write prefix and suffix for the *.mp files. Note that trailing and 
# leading double quotes (") are there for proper command syntax
metapop.cmd.prefix <- 'START /WAIT "title" "C:\\Users\\Matthew\\Documents\\My Dropbox\\RAMAS\\MetapopCmd.exe" "'
metapop.cmd.suffix <- '" /verbose /RUN=YES'

# Get all of the *.mp files - this should result in 160k mp files
snpl.mp.files <- Sys.glob(paths="PartNum*/*/*/snpl*mp")
foreach( snpl = 1:length(snpl.mp.files) ) %dopar% {
  # Concatenate metapop command
  metapop.cmd <- paste( metapop.cmd.prefix, snpl.mp.files[snpl], metapop.cmd.suffix, sep="" )
  # Call Metatpop.exe
  shell( metapop.cmd )
}

# Stop the cluster, closing the extra R sessions
stopCluster(cl)

## Search for unrun *.mp files
## There is a problem in the above call to run the
## various *.mp files, in that if two *.mps using the
## same set of KCH files are called at almost the same 
## time, which ever is called first LOCKS the KCH
## files temporarily, such that the second *.mp does 
## not run. 

# Every *.mp file that successfully ran should have
# an associated scl file. So first, get all the 
# scl files
snpl.scl.files <- Sys.glob(paths="PartNum*/*/*/snpl*SCL")
# String replace the SCL with mp
snpl.mp.files.Done <- sub("SCL","mp",snpl.scl.files)
# Use Set Operation setdiff to get a list of the
# mp files that did not get run
snpl.mp.files.ToRun <- setdiff(snpl.mp.files,snpl.mp.files.Done)
# Run this list of *.mp files
for ( snpl in snpl.mp.files.ToRun ){
  # Concatenate metapop command
  metapop.cmd <- paste( metapop.cmd.prefix, snpl, metapop.cmd.suffix, sep="" )
  # Call Metatpop.exe
  shell( metapop.cmd )  
}

## **************************************************************##
## Section 5: Extraction of results from *.mp files
## Here we use a snpl specific modified version of the
## mp.mult.results.r script (part of the SA Code)
## The modifications allow for extraction of information
## about the PartNum, RandType, Repetition, and SLR Scenario
##
## BUG TO FIX: In the current versions of the SA scripts
## Generation time is calculated by an external, compiled
## program (GenTime.exe). In order to run this program
## from within R, I have to be in the SACode directory.
## To FIX this in the future, I plan to implement the 
## GenTime algorithm in an R script, but to get this 
## project done, I'm using the work around of switching
## directories for now.
setwd( sacode.snpl )

# Get vector lists of the snpl mp files seperated by PartNum
# Make paths to look at
GlobPaths <- paste(snpl.project.dir, new.PartNum.dirs, "/*/*/snpl*mp",sep="")
# lapply glob call
snpl.mp.byPartNum <- lapply(GlobPaths,Sys.glob)
# Create a list of names of resout.csv files, to be
# used in the foreach loop below
res.out.files <- paste(snpl.SAres.dir,'snpl.',
                       part.num.toRun,'.rep.SA2.csv',sep='')

## Create one csv file for each of the PartNum_ cases run. 
## Do this in Parallel, using length(part.num.toRun) processing cores
# Setup cluster
require(doSNOW)
require(foreach)
cl <- makeCluster(length(part.num.toRun),"SOCK") # For group computer
registerDoSNOW(cl)
foreach( GP = 1:length(GlobPaths) ) %dopar% {
  # Start by getting SA analysis scripts
  source( 'sensitivity.setup.r' )
  sensitivity.setup( sens.base.dir="./" )
  # Call mp.mult.results.snpl.r
  mp.mult.results.snpl(mp.file.list=snpl.mp.byPartNum[[GP]], out.csv=res.out.files[GP],
                       spatial=TRUE)
  
}
# Stop the cluster, closing the extra R sessions
stopCluster(cl)

## Several functions have been written else where to 
## partition a snpl dataset by Sampling Type, SLR Scenario
## and Absolute versus Relative End-points and then 
## perform a brt analyses on these data.  Therefore, 
## the data extracted above needs to be reordered to create the 
## format required by 'snpl.df.partition'. This format is
## as follows: 
##   -data.frame 40,000 rows x 68 columns (read in as csv file)
##   -rows 1:10k = lhs.nocc, 10001:20k = lhs.2m, 
##         20001:30k = unif.nocc, 30001:40k = unif.2m

## Read in and re-order the csv files to match the format
## described in the intro to this section
require(plyr)

# Make a function that does the reordering on one csv file, 
# then lapply to res.out.files
reorder.resOut <- function( resout.file ){
  # Read the csv file into a data.frame
  resout.csv <- read.csv( resout.file )
  # Move the csv file to a backup file
  resout.file.BKUP <- sub("csv","BKUP.csv",resout.file)
  file.rename(resout.file,resout.file.BKUP)
  # Reorder the data.frame
  slrOrder <- c("NoCC","2M")
  resout.csv$SLR <- factor(resout.csv$SLR,levels=slrOrder,ordered=TRUE)
  rand.type.Order <- c("LHS","Unif")
  resout.csv$RandType <- factor(resout.csv$RandType,levels=rand.type.Order,ordered=TRUE)
  # Use plyr function 'arrange'
  resout.csv <- arrange( resout.csv,RandType,SLR)
  # Write the csv file
  write.csv(resout.csv, file=resout.file, quote=FALSE, row.names=FALSE) 
}
# Apply function to vector of resout.csv files
sapply(res.out.files,reorder.resOut)

## **************************************************************##
## Section 6: Carry out BRT analysis
## Use boosted regression tree analysis to investigate
## the order of importance of different variables of 
## interest in the SNPL Sensitivity Analysis
## I have previously run these analyses of SA1, but am
## now running them for SA2. The main differences between
## SA1 and SA2 is that the bounds for a select number of
## parameters is tighter in SA2 than in SA1, to decrease
## the importance of those parameters compared to all 
## others.

## Set new working directory - set to the Sensitivity Analysis
## directory the resides on DropBox. This is where all the analysis
## really gets done
setwd( sa.sens.dir )

## Source snpl_sa_functions.r
## This script has functions used to partition the data
## collected and implement the brt analysis. The file
## is kept in sacode.snpl
source( paste(sacode.snpl,'snpl_sa_functions.r',sep="") )

## BRT Analysis Parameter Specifications 
## -------------------------------------
## Set predictor variables:
## The predictor variables we are examining correspond to those values
## examined in the 'one-at-a-time' sensitivity analysis carried out
## in Aiello-Lammens et al. 2012
gbm.predictors <- c(2,10,11,63,64,62,48,61)

## Set response variable:
## We are focusing on two general response variables, Expected
## Minimum Abundance (EMA) and Probability of Population Decline
## to 50 Individuals (Prob.50).  There are also two corresponding
## 'relative end-point metrics'.
## It is possible to determine which data.frame corresponds to 
## absolute or relative end-points based on the 'length' of the 
## data.frame - legth( absolute ) = 68 and length( relative ) = 70
## For Relative end-point metrics
## 69 = p50.delta
## 70 = ema.delta
## For Absolute end-point metrics
## 25 = Prob.50
## 29 = EMA
## Here we define which absolute and relative response values
## we are using in the brt analysis that follows
abs.resp.p50 <- 25
rel.resp.p50 <- 69
abs.resp.ema <- 29
rel.resp.ema <- 70
## -------------------------------------

## First do analysis for end-point = *Prob.50*
abs.resp <- abs.resp.p50
rel.resp <- rel.resp.p50

## Perform analysis on for each res.out.files element
## Below is how to do this in one loop, using one core
# snpl.brt.results.p50 <- vector( mode='list', length=0)
# for ( res.file in 1:length(res.out.files)){
#   # Make brt.file.base.name
#   brt.file.base.name <- unname(basename(res.out.files[res.file]))
#   brt.file.base.name <- sub('rep.','',brt.file.base.name)
#   brt.file.base.name <- sub('csv','p50.',brt.file.base.name)
#   # Call function snpl.run.mult.brt
#   snpl.brt.results.p50[[res.file]] <- snpl.run.mult.brt(snpl.csv=res.out.files[res.file],
#                                         brt.file.base.name=brt.file.base.name,
#                                         learning.rate=0.001)
#   
# }

## Perform analysis on for each res.out.files element
## Below is how to do this in parallel. 
## WARNING: If length(res.out.files) > 8, this may crash the
## computer (there are only 8 cores)
require(doSNOW)
require(foreach)
cl <- makeCluster(length(res.out.files),"SOCK") # For group computer
registerDoSNOW(cl)
# By defualt, the resutls from each run of 
# snpl.run.mult.brt will be combined as a list
snpl.brt.results.p50 <- foreach( RF = 1:length(res.out.files) ) %dopar% {
  source( paste(sacode.snpl,'snpl_sa_functions.r',sep="") )
  # Make brt.file.base.name
  brt.file.base.name <- unname(basename(res.out.files[RF]))
  brt.file.base.name <- sub('rep.','',brt.file.base.name)
  brt.file.base.name <- sub('csv','p50.',brt.file.base.name)
  # Call function snpl.run.mult.brt
  snpl.run.mult.brt(snpl.csv=res.out.files[RF],
                    brt.file.base.name=brt.file.base.name,
                    learning.rate=0.001)
}
stopCluster(cl)

## Also run BRT Analysis for Part.Num. = 10k - Modified sensitivity analysis
snpl.10k.SA2.p50.brt.results <- snpl.run.mult.brt(snpl.csv='Results/snpl.10000.rep.SA2.csv',
                  brt.file.base.name='snpl.10k.SA2.ema.',learning.rate=0.001)

# Assign names to snpl.10k.SA2.brt.results
save( snpl.10k.SA2.p50.brt.results, file="Results/snpl.10k.SA2.p50.brt.results.RData" )

## Next do analysis for end-point = *EMA*
abs.resp <- abs.resp.ema
rel.resp <- rel.resp.ema

cl <- makeCluster(length(res.out.files),"SOCK") # For group computer
registerDoSNOW(cl)
# By defualt, the resutls from each run of 
# snpl.run.mult.brt will be combined as a list
snpl.brt.results.ema <- foreach( RF = 1:length(res.out.files) ) %dopar% {
  source( paste(sacode.snpl,'snpl_sa_functions.r',sep="") )
  # Make brt.file.base.name
  brt.file.base.name <- unname(basename(res.out.files[RF]))
  brt.file.base.name <- sub('rep.','',brt.file.base.name)
  brt.file.base.name <- sub('csv','ema.',brt.file.base.name)
  # Call function snpl.run.mult.brt
  snpl.run.mult.brt.ema(snpl.csv=res.out.files[RF],
                    brt.file.base.name=brt.file.base.name,
                    learning.rate=0.001)
}
stopCluster(cl)

# Make names vector to assign to snpl.brt.results
brt.res.names <- sub('.*/','',res.out.files)
brt.res.names <- sub('.csv','',brt.res.names)
brt.res.names <- sub('rep.','',brt.res.names)

# Name list elements for snpl.brt.results.p50
names(snpl.brt.results.ema) <- brt.res.names
# Save these results to be used later
save(snpl.brt.results.ema, 
     file=paste(snpl.SAres.dir,'snpl.SA2.ema.brt.results.RData',sep='') )

## Also run BRT Analysis for Part.Num. = 10k - Modified sensitivity analysis
snpl.10k.SA2.brt.results.ema <- snpl.run.mult.brt.ema(snpl.csv='Results/snpl.10000.rep.SA2.csv',
                  brt.file.base.name='snpl.10k.SA2.ema.',learning.rate=0.01)

# Assign names to snpl.10k.SA2.brt.results
save( snpl.10k.SA2.brt.results.ema, file="Results/snpl.10k.SA2.ema.brt.results.RData" )

## If the above has already been run, then just load in the
## results, which should have been saved to disk.
load( 'Results/snpl.10k.SA2.ema.brt.results.RData' )


## **************************************************************##
## Section 7a: Relative Influence (Importance) data.frame construction
## for endpoint = p50

## As with all of the SA1 results, the snpl 10k SA2 results
## have been run already (for Prob. 50).  Therefore, we can 
## load them here, rather than have to re-run them
load( 'Results/snpl.10k.SA2.p50.brt.results.RData')

## Load all SA2 P50 results if needed
load( 'Results/snpl.SA2.p50.brt.results.RData' )

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

# Create a new list object to store all rel.inf data.frames
brt.summ.df.complete <- vector( mode='list', length=0 )
# Loop through list of lists of results
for ( part.num.ind in 1:length(snpl.brt.results.p50)) {
  part.num.result <- snpl.brt.results.p50[[ part.num.ind ]]
  # (Re)create a vector to store this part.num.result data.frames
  brt.part.num.summ.df <- vector(mode="list", length=0)
  # Each of these lists has 6 list elements - loop through these
  for ( scenario.res in 1:length(part.num.result) ) {
    brt.summ.list <- part.num.result[[ scenario.res ]]$brt.summary
    brt.part.num.summ.df[[ scenario.res ]] <- make.brt.summary.df( brt.summ.list, end.point=abs.rel.vector[ scenario.res ],
                                                                   slr=slr.vector[ scenario.res ])
  }
  names( brt.part.num.summ.df ) <- scenario.names
  brt.summ.df.complete[[ part.num.ind ]] <- brt.part.num.summ.df
}
names( brt.summ.df.complete) <- names( snpl.brt.results.p50 )
# Clean-up
rm( brt.part.num.summ.df, brt.summ.list )

## **************************************************************##
## Section 7b: Relative Influence (Importance) data.frame construction
## for endpoint = ema

## As with all of the SA1 results, the snpl 10k SA2 results
## have been run already (for Prob. 50).  Therefore, we can 
## load them here, rather than have to re-run them
load( 'Results/snpl.10k.SA2.ema.brt.results.RData')

## Load all SA2 P50 results if needed
load( 'Results/snpl.SA2.ema.brt.results.RData' )

## Define the importance values that we are considering the 
## references (ie TRUE) values.  In nearly all scenarios, this
## should be one of the Part.Num. = 10000 scenarios. Currently
## they are defined as the 'lhs' results
# Relative end-point metrics
brt.summary.relEnds.ref <- brt.summary.ord( snpl.10k.SA2.brt.results.ema$relEnds.lhs$brt.summary[[1]] )
# Absolute end-point metrics - 2M SLR
brt.summary.absEnds.slr.ref <- brt.summary.ord( snpl.10k.SA2.brt.results.ema$lhs.2m$brt.summary[[1]] )
# Absolute end-point metrics - NoCC 
brt.summary.absEnds.nocc.ref <- brt.summary.ord( snpl.10k.SA2.brt.results.ema$lhs.nocc$brt.summary[[1]] )


# Get the element names for the xxx.results lists - should be the
# same for all Part.Num. cases
scenario.names <- names( snpl.10k.SA2.brt.results.ema )
# Change scenario names to make visualization and anlaysis easier
# later
scenario.names[5:6] <- c("lhs.relEnds","unif.relEnds")
# Make a  vector classifing scenarios as abs or rel end
abs.rel.vector <- ifelse( grepl( pattern="rel", scenario.names ), yes="rel", no="abs" )
# Make a vector for the slr scenario ("nocc" or "2m")
slr.vector <- ifelse( grepl( pattern="nocc", scenario.names ), yes="nocc", no="2m" )

# Create a new list object to store all rel.inf data.frames
brt.ema.summ.df.complete <- vector( mode='list', length=0 )
# Loop through list of lists of results
for ( part.num.ind in 1:length(snpl.brt.results.ema)) {
  part.num.result <- snpl.brt.results.ema[[ part.num.ind ]]
  # (Re)create a vector to store this part.num.result data.frames
  brt.part.num.summ.df <- vector(mode="list", length=0)
  # Each of these lists has 6 list elements - loop through these
  for ( scenario.res in 1:length(part.num.result) ) {
    brt.summ.list <- part.num.result[[ scenario.res ]]$brt.summary
    brt.part.num.summ.df[[ scenario.res ]] <- 
      make.brt.summary.df( brt.summ.list, end.point=abs.rel.vector[ scenario.res ],
                           slr=slr.vector[ scenario.res ])
  }
  names( brt.part.num.summ.df ) <- scenario.names
  brt.ema.summ.df.complete[[ part.num.ind ]] <- brt.part.num.summ.df
}
names( brt.ema.summ.df.complete) <- names( snpl.brt.results.ema )
# Clean-up
rm( brt.part.num.summ.df, brt.summ.list )

## **************************************************************##
## Section 8: Correlation matrix construction
calc.cor.brt.summary <- function( part.num.list, cor.type="pearson" ) {
  return( lapply( part.num.list, cor, method=cor.type ) )
}

## P50
#brt.corr.complete <- lapply( brt.summ.df.complete, calc.cor.brt.summary, cor.type="pearson" )
#brt.corr.complete <- lapply( brt.summ.df.complete, calc.cor.brt.summary, cor.type="spearman" )
#brt.corr.complete <- lapply( brt.summ.df.complete, calc.cor.brt.summary, cor.type="kendall" )

## EMA
brt.corr.complete <- lapply( brt.ema.summ.df.complete, calc.cor.brt.summary, cor.type="pearson" )
#brt.corr.complete <- lapply( brt.ema.summ.df.complete, calc.cor.brt.summary, cor.type="spearman" )
#brt.corr.complete <- lapply( brt.ema.summ.df.complete, calc.cor.brt.summary, cor.type="kendall" )


## Extract values for correlation between brt analyses of
## Part.Num./Sampling/SLR scenarios and their corresponding
## reference scenario. These values are simlpy the last vector
## of the correlation matrices
extract.ref.cor <- function( scenario.cor.mat ) {
  cor.mat.dim <- dim( scenario.cor.mat)
  # Extract last column, dropping the last row (cor value with itself, i.e. 1)
  return( scenario.cor.mat[ 1:(cor.mat.dim[1]-1),cor.mat.dim[2] ] )
}

## A simple function to lapply the extract.ref.cor - just a way
## to get around the nested list strcuture I have going on
apply.extract.ref.cor <- function( cor.mat.list ) {
  return( lapply( cor.mat.list, extract.ref.cor ) )
}

## Create a list of correlation with reference scenario
brt.corr.reference <- lapply( brt.corr.complete, apply.extract.ref.cor )

## Melt brt.corr.reference into a data.frame
brt.corr.reference <- melt( brt.corr.reference )

## Split the second column of the new data.frame to seperate
## the sampling type from the slr scenario
df_split <- strsplit( brt.corr.reference$L2, split="\\." )
brt.corr.reference <- cbind( brt.corr.reference, do.call( rbind, df_split) )
# Assign some column names
names( brt.corr.reference ) <- c( "cor.val", "samp.slr", "part.num",
                                  "samp","slr" )
brt.corr.reference$part.num <- factor( brt.corr.reference$part.num, 
                                       c('snpl.100.SA2','snpl.250.SA2','snpl.500.SA2','snpl.1000.SA2') )

## Use ggplot to have a look at our results in box-plot form
# This call makes plots where all of the y-axis are the same
# ggplot( brt.corr.reference, aes(samp,cor.val)) + geom_boxplot() + facet_grid( slr~part.num )
# This call makes plots where the y-axis vary individually for each **facet**
ggplot( brt.corr.reference, aes(samp,cor.val)) + geom_boxplot() + facet_grid( slr~part.num, scales='free_y' )
# This call makes plots where the y-axis varies individiaully for each **plot**
#ggplot( brt.corr.reference, aes(samp,cor.val)) + geom_boxplot() + facet_wrap( slr~part.num, scales='free_y' )

#ggsave('SA2-pearson-cor-boxplots-ema.pdf')

## Use ggplot to have a look at our results in box-plot form
temp <- brt.corr.reference
temp$samp <- as.character( temp$samp)
temp$samp[ temp$samp=="lhs" ] <- "LHS"
temp$samp[ temp$samp=="unif" ] <- "URS"
temp$slr <- as.character( temp$slr )
temp$slr[ temp$slr=="2m" ] <- "2m SLR"
temp$slr[ temp$slr=="nocc" ] <- "No SLR"
temp$slr[ temp$slr=="relEnds" ] <- "Relative"
temp$part.num <- as.character( temp$part.num )
temp$part.num[ temp$part.num=="snpl.100.SA2" ] <- "100"
temp$part.num[ temp$part.num=="snpl.250.SA2" ] <- "250"
temp$part.num[ temp$part.num=="snpl.500.SA2" ] <- "500"
temp$part.num[ temp$part.num=="snpl.1000.SA2" ] <- "1000"
temp$part.num <- 
  factor( temp$part.num, c('100','250','500','1000') )

ggplot( temp, aes(samp,cor.val)) + 
  geom_boxplot() + 
  facet_grid( slr~part.num, scales='free_y' ) +
  ylab("Pearson Correlation Coefficient Value") +
  xlab("Sampling Type") +
  theme_bw() +
  theme( text=element_text( size=12, family="Times") )

## Save the P50 results to this file
#ggsave( file="figures/Diss_Fig_2_10.pdf", width=6.5, height=6.5, units="in" )

## Save the EMA results to this file
ggsave( file="figures/Diss_Fig_2_11.pdf", width=6.5, height=6.5, units="in" )
