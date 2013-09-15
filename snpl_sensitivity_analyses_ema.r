## **************************************************************##
## File: snpl_sensitivity_analyses_ema.r
## Author: Matthew Aiello-Lammens
## Data: 2 November 2012
## Modified: 10 November 2012 - to run EMA analysis on Windows machine
##
## Purpose: This file includes R code for the analysis of the data
## generated and collected to investigate methods of performing
## sensitivity analysis on the Florida Snowy Plover population
## models.
##
## Much of the work that is presented here was originally
## developed and documented in snpl_sensitivity_worklog.R
##
## **************************************************************##
## Nomenclature and naming concerns:
## I have had some troubles with assigning names to some of the
## factors examined in this study.  Here I attempt to define them.
##
## Sampling Method: 
## This is the method in which random scaling multipliers are 
## selected. The scaling multiplier is used to generate a new
## value for specific parameters in the *.mp file.  There are two
## methods implemented in this analysis:
##   lhs - Latin hypercube sampling
##   unif - Uniform Random Variable sampling
##
## Partition Number:
## I am least consistant with the use of this term. Partition 
## number comes from the 'randomLHS' function and is defined
## as 'the number of simulations or design points'.  Alternatively
## it can also be thought of as the number of 'dimensions' 
## sampled.  I have previously referred to this number as 
## 'sample size', but this nomenclature should be eliminated.
## However, note that this value is labeled as 'SampSize' in the 
## data.frames of collected data, and I do not plan to change this.
## Currently, five different values for partition number  
## have been utilized in creation of simulation sets:
## 10
## 100
## 500
## 1000
## 10000
## Two important points about these partition numbers:
## 1) We are considering the simulations with partition numbers
## of 10000 to be the "True" representation of sensitivity.
## 2) We are unable to use the simulations with partition numbers
## of 10 in our analysis in a meaningful way, because our analysis
## method (boosted regression trees) cannot analyze a dataset
## with 10 samples and 8 predictor variables.
##
## Absolute versus Relative End-point Metrics:
## Results from RAMAS simulations include population end-point
## metrics such as Probability of Extinction, Expected Minimum
## Abundance, Prob. decline to a threshold, etc.  We are 
## considering these measures to be *Absolute* end-point
## metrics.
## In contrast, we also calculated *Relative* end-point metrics.
## We carried out matched RAMAS Metapop simulations 
## for a No Sea-level Rise scenario and a 2m sea-level
## rise scenario.  These scenarios correspond with the simulations
## used in Aiello-Lammens et al. 2012 (Glob. Change. Biol.).
## Relative end-point metrics were calculated by determining
## the differences (delta) in the absolute end-point metrics
## between paired No-slr and 2m-slr simulations.

## **************************************************************##
## Section 0: Require Packages
require(gbm)
require(dismo)
require(ggplot2)
require(reshape)

## **************************************************************##
## Section 1: Definition of Functions

snpl.df.partition <- function( snpl.curr.rep ) {
  ## Partition a Snowy Plover data.frame into several data.frames
  ## based on Sampling Type, SLR scenario, Absolute
  ## and Relative End-point Metrics
  ##
  ## Args:
  ##  snpl.currr.rep: A data.frame of SNPL simulation results,
  ##  should be 40,000 rows x 68 columns
  ##
  ## Returns:
  ##  snpl.dfs.list: A list element containing five data.frames
  ##  which are partitions of the larger data.frame
  
  # Re-order the kch.type factor prior to partitioning the data.frame
  snpl.curr.rep$kch.type <- factor( snpl.curr.rep$kch.type, c('LO.KCH','ME.KCH','HI.KCH'))

  # Set values for partitions
  lhs.nocc <- 1:10000
  lhs.2m <- 10001:20000
  unif.nocc <- 20001:30000
  unif.2m <- 30001:40000
  
  # Create a new list element
  snpl.dfs.list <- vector(mode="list", length=0)
  
  # Seperate out lhs and unif into different data sets
  snpl.dfs.list$lhs.nocc <- snpl.curr.rep[lhs.nocc,]
  snpl.dfs.list$lhs.2m <- snpl.curr.rep[lhs.2m,]
  snpl.dfs.list$unif.nocc <- snpl.curr.rep[unif.nocc,]
  snpl.dfs.list$unif.2m <- snpl.curr.rep[unif.2m,]
  
  # Make relative end-point (matched) data set
  p50.delta <- snpl.curr.rep$prob.50[c(lhs.2m,unif.2m)] - snpl.curr.rep$prob.50[c(lhs.nocc,unif.nocc)]
  ema.delta <- snpl.curr.rep$exp.min.n[c(lhs.2m,unif.2m)] - snpl.curr.rep$exp.min.n[c(lhs.nocc,unif.nocc)]
  snpl.relEnds <- cbind(snpl.curr.rep[c(lhs.nocc,unif.nocc),], p50.delta, ema.delta)
  
  # Clean up a little
  rm(p50.delta)
  rm(ema.delta)
  
  # Seperate out lhs and unif into different relEnds (matched) data sets
  snpl.dfs.list$relEnds.lhs <- snpl.relEnds[1:10000,]
  snpl.dfs.list$relEnds.unif <- snpl.relEnds[10001:20000,]
  
  # Return snpl.dfs.list
  return(snpl.dfs.list)
}

snpl.run.brt <- function( snpl.brt.run, gbm.predictors, gbm.response, save.brt = FALSE,
                          brt.file.basename = 'brt.file', learning.rate=0.001 ) {
  ## Run a Boosted Regression Tree analysis on a SNPL
  ## data.frame.
  ##
  ## Args:
  ##  snpl.brt.run: The data.frame to be used in this run of the 
  ##  brt analysis.
  ##
  ##  gbm.predictors: A vector of the predictor value indices.
  ##
  ##  gbm.response: A value for the response value indice.
  ##
  ## Returns:
  ##  list object containing two lists: 1) all summary data.frames
  ##  and 2) results of a gbm.interactions call on the gbm objects.
  
  # Get a list of unique replication numbers
  RepNum <- unique( snpl.brt.run$RepNumber )
  
  # Create two empty lists
  snpl.brt.summ.list <- vector(mode="list",length=0)
  snpl.brt.int.list <- vector(mode="list",length=0)
  
  # Loop through all the reps
  for ( Rep in 1:length(RepNum) ) {
    print('### REP ###');print(Rep)
    # Subset the data.frame, taking only "Rep" results
    # I define this as a global variable so that the gbm.interactions
    # function works.
    snpl.sub <<- subset( snpl.brt.run, subset=RepNumber==RepNum[Rep] )
    snpl.sub.brt <- gbm.step( snpl.sub, gbm.x=gbm.predictors, gbm.y=gbm.response,
                              family="gaussian", tree.complexity=4, learning.rate=learning.rate,
                              bag.fraction=0.5, tolerance.method="fixed",tolerance=0.001)
    # Put a while loop here that checks for whether the brt actually fit the
    # data. If it did not, then the snpl.sub.brt variable will be NULL
    while( is.null( snpl.sub.brt ) ){
      # Print a warning message
      print('### Warning: no model fit - retrying now ###')
      snpl.sub.brt <- gbm.step( snpl.sub, gbm.x=gbm.predictors, gbm.y=gbm.response,
                                family="gaussian", tree.complexity=4, learning.rate=learning.rate,
                                bag.fraction=0.5, tolerance.method="fixed",tolerance=0.001)
    }
    
    # Add the brt results to snpl.brt.list
    snpl.brt.summ.list[[ Rep ]] <- summary( snpl.sub.brt )
    snpl.brt.int.list[[ Rep ]] <- gbm.interactions( gbm.object=snpl.sub.brt )
    
    # Save brt model to file - these models can be investigated later
    if ( save.brt ) {
      brt.file.name <- paste( brt.file.basename, Rep, 'RData', sep="." )
      save( snpl.sub.brt, file=brt.file.name )
    }
  }
  # Clean-up
  rm( snpl.sub, envir = .GlobalEnv ) 
  return( list(brt.summary=snpl.brt.summ.list, brt.interactions=snpl.brt.int.list) )
}

brt.summary.ord <- function( brt.summary.df ) {
  ## Order a brt.summary data frame alphebetically by variable name
  ## 
  ## Args:
  ##    brt.summary.df: A data.frame resultings from a 'summary()' 
  ##    call on a gbm object
  ##
  ## Returns:
  ##    brt.summary.df.ord: The summary data.frame, ordered  
  ##    alphebetically by varialbe name
  brt.summary.df.ord <- brt.summary.df[ order(brt.summary.df$var), ]
  return( brt.summary.df.ord)
}

make.brt.summary.df <- function( brt.summary.list, end.point, slr="NA" ) { 
  ## Construct the brt relative influecne (rel.inf) 
  ## data.frame.  Relative influence can also be interpreted as order
  ## of importance of variables.
  ##
  ## Args:
  ##    brt.summary.list: A list of the various brt.summary data.frames
  ##    produced as part of the snpl.run.brt function
  ##
  ##    end.point: Either "rel" or "abs" for relative or absolute
  ##    end-point metrics.
  ##
  ##    slr: A boolean only used if end.point=="abs".  If this is the case
  ##    then slr = either "2m" or "nocc", indicating which reference rel.inf
  ##    to use.
  ##
  ## Returns:
  ##    brt.summary.df: A data.frame of all sorted and concatanated 
  ##    relative influence values as returned from summary( [gbm object] )
  
  # Apply the brt.summary.ord function on the list of brt.summary results
  brt.summary.list.ord <- lapply( brt.summary.list, brt.summary.ord )
  
  # Loop through the list, extracting the rel.inf vals for each
  brt.summary.df <- vector()
  for ( summ in brt.summary.list.ord ){
    brt.summary.df <- cbind( brt.summary.df, summ$rel.inf )
  }
  brt.summary.df <- as.data.frame( brt.summary.df )
  # Give the new data.frame row.names
  row.names( brt.summary.df ) <- as.character(brt.summary.list.ord[[1]]$var)
  
  # Add the 'reference' relative influence data.frame
  if ( end.point == "rel" ) {
    ref.rel.inf <- brt.summary.relEnds.ref$rel.inf
  } else if ( end.point == "abs" ) {
    ifelse( slr=="nocc", ref.rel.inf <- brt.summary.absEnds.nocc.ref$rel.inf,
            ref.rel.inf <- brt.summary.absEnds.slr.ref$rel.inf)
  } else {
    stop( 'Incorrect end.point selected. Must be either "rel" or "abs"' )
  }
  brt.summary.df <- cbind( brt.summary.df, reference=ref.rel.inf )
  
  # Retrun the final data.frame
  return( brt.summary.df )
}

## **************************************************************##
## Section 2: BRT Analysis Parameter Specifications 

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
abs.resp <- 29
rel.resp <- 70

## **************************************************************##
## Section 3: BRT Analysis for Part.Num. = 10000 scenario

# Lines commented out since analysis was already run
# 
# # Run brt analysis on Part.Num. = 10000 scenario
# #snpl.10k.rep <- read.csv('/Users/mlammens/Dropbox/RA-FL-Shore-Birds/SensitivityAnalysis/Results/snpl.10000.rep.csv')
# snpl.10k.rep <- read.csv('Results/snpl.10000.rep.csv')
# snpl.10k.part <- snpl.df.partition( snpl.10k.rep )
# # Clean-up
# rm( snpl.10k.rep )
# # Loop through the snpl.10k.part list, and execute the brt analysis
# snpl.10k.brt.results <- vector(mode="list",length=0)
# for ( df.num in 1:length(snpl.10k.part) ){
#   snpl <- snpl.10k.part[[ df.num ]]
#   # Determine which gbm.response variable to use
#   ifelse ( length(snpl) == 68, gbm.response <- abs.resp, gbm.response <- rel.resp )
#   # Call brt analysis function
#   brt.file.base.name <- paste( '/Users/Matthew/Projects/FL-Shore-Birds/BRT_Models/snpl.10k.ema.',
#                                df.num, sep="")
#   snpl.10k.brt.results[[ df.num ]] <- snpl.run.brt( snpl, gbm.predictors, gbm.response, 
#                                                     save.brt=TRUE, brt.file.basename=brt.file.base.name,
#                                                     learning.rate=0.01)
# }
# # Assign names to snpl.10k.brt.results
# names(snpl.10k.brt.results) <- names(snpl.10k.part)
# save( snpl.10k.brt.results, file="Results/snpl.10k.ema.brt.results.RData")

## If the above has already been run, then just load in the
## results, which should have been saved to disk.
load( 'Results/snpl.10k.ema.brt.results.RData' )

## Define the importance values that we are considering the 
## references (ie TRUE) values.  In nearly all scenarios, this
## should be one of the Part.Num. = 10000 scenarios. Currently
## they are defined as the 'lhs' results
# Relative end-point metrics
brt.summary.relEnds.ref <- brt.summary.ord( snpl.10k.brt.results$relEnds.lhs$brt.summary[[1]] )
# Absolute end-point metrics - 2M SLR
brt.summary.absEnds.slr.ref <- brt.summary.ord( snpl.10k.brt.results$lhs.2m$brt.summary[[1]] )
# Absolute end-point metrics - NoCC 
brt.summary.absEnds.nocc.ref <- brt.summary.ord( snpl.10k.brt.results$lhs.nocc$brt.summary[[1]] )

## **************************************************************##
## Section 4: BRT Analysis for Part.Num. = 100

# # Run brt analysis on Part.Num. = 100 scenario
# snpl.100.rep <- read.csv('Results/snpl.100.rep.csv')
# snpl.100.part <- snpl.df.partition( snpl.100.rep )
# # Clean-up
# rm( snpl.100.rep )
# # Point to BRT_Model storage Location
# brt.model.stor <- '/Users/Matthew/Projects/FL-Shore-Birds/BRT_Models/'
# ###brt.model.stor <- '/Users/mlammens/Documents/StonyBrook-BEE/FL-shorebird/Sensitivity-Analysis-Data-Local/BRT_Models/'
# # Loop through the snpl.100.part list, and execute the brt analysis
# snpl.100.brt.results <- vector(mode="list",length=0)
# for ( df.num in 1:length(snpl.100.part) ){
#   snpl <- snpl.100.part[[ df.num ]]
#   # Determine which gbm.response variable to use
#   ifelse ( length(snpl) == 68, gbm.response <- abs.resp, gbm.response <- rel.resp )
#   # Call brt analysis function
#   brt.file.base.name <- paste( brt.model.stor,'snpl.100.ema.',
#                                df.num, sep="")
#   snpl.100.brt.results[[ df.num ]] <- snpl.run.brt( snpl, gbm.predictors, gbm.response, 
#                                                     save.brt=TRUE, brt.file.basename=brt.file.base.name,
#                                                     learning.rate=0.01)
# }
# # Assign names to snpl.100.brt.results
# names(snpl.100.brt.results) <- names(snpl.100.part)
# save( snpl.100.brt.results, file="Results/snpl.100.ema.brt.results.RData")

## If the above has already been run, then just load in the
## results, which should have been saved to disk.
load( 'Results/snpl.100.ema.brt.results.RData' )
  
## **************************************************************##
## Section 5: BRT Analysis for Part.Num. = 500

# # Run brt analysis on Part.Num. = 500 scenario
# snpl.500.rep <- read.csv('Results/snpl.500.rep.csv')
# snpl.500.part <- snpl.df.partition( snpl.500.rep )
# # Clean-up
# rm( snpl.500.rep )
# # Point to BRT_Model storage Location
# brt.model.stor <- '/Users/Matthew/Projects/FL-Shore-Birds/BRT_Models/'
# ###brt.model.stor <- '/Users/mlammens/Documents/StonyBrook-BEE/FL-shorebird/Sensitivity-Analysis-Data-Local/BRT_Models/'
# # Loop through the snpl.500.part list, and execute the brt analysis
# snpl.500.brt.results <- vector(mode="list",length=0)
# for ( df.num in 1:length(snpl.500.part) ){
#   snpl <- snpl.500.part[[ df.num ]]
#   # Determine which gbm.response variable to use
#   ifelse ( length(snpl) == 68, gbm.response <- abs.resp, gbm.response <- rel.resp )
#   # Call brt analysis function
#   brt.file.base.name <- paste( brt.model.stor,'snpl.500.ema.',
#                                df.num, sep="")
#   snpl.500.brt.results[[ df.num ]] <- snpl.run.brt( snpl, gbm.predictors, gbm.response, 
#                                                     save.brt=TRUE, brt.file.basename=brt.file.base.name,
#                                                     learning.rate=0.001)
# }
# # Assign names to snpl.500.brt.results
# names(snpl.500.brt.results) <- names(snpl.500.part)
# save( snpl.500.brt.results, file="Results/snpl.500.ema.brt.results.RData")

## If the above has already been run, then just load in the
## results, which should have been saved to disk.
load( 'Results/snpl.500.ema.brt.results.RData' )
  
## **************************************************************##
## Section 6: BRT Analysis for Part.Num. = 1000

# # Run brt analysis on Part.Num. = 1000 scenario
# snpl.1000.rep <- read.csv('Results/snpl.1000.rep.csv')
# snpl.1000.part <- snpl.df.partition( snpl.1000.rep )
# # Clean-up
# rm( snpl.1000.rep )
# # Point to BRT_Model storage Location
# brt.model.stor <- '/Users/Matthew/Projects/FL-Shore-Birds/BRT_Models/'
# ###brt.model.stor <- '/Users/mlammens/Documents/StonyBrook-BEE/FL-shorebird/Sensitivity-Analysis-Data-Local/BRT_Models/'
# # Loop through the snpl.1000.part list, and execute the brt analysis
# snpl.1000.brt.results <- vector(mode="list",length=0)
# for ( df.num in 1:length(snpl.1000.part) ){
#   snpl <- snpl.1000.part[[ df.num ]]
#   # Determine which gbm.response variable to use
#   ifelse ( length(snpl) == 68, gbm.response <- abs.resp, gbm.response <- rel.resp )
#   # Call brt analysis function
#   brt.file.base.name <- paste( brt.model.stor,'snpl.1000.ema.',
#                                df.num, sep="")
#   snpl.1000.brt.results[[ df.num ]] <- snpl.run.brt( snpl, gbm.predictors, gbm.response, 
#                                                     save.brt=TRUE, brt.file.basename=brt.file.base.name,
#                                                     learning.rate=0.001)
# }
# # Assign names to snpl.1000.brt.results
# names(snpl.1000.brt.results) <- names(snpl.1000.part)
# save( snpl.1000.brt.results, file="Results/snpl.1000.ema.brt.results.RData")

## If the above has already been run, then just load in the
## results, which should have been saved to disk.
load( 'Results/snpl.1000.ema.brt.results.RData' )
  
## **************************************************************##
## Section 7: BRT Analysis for Part.Num. = 250

# # Run brt analysis on Part.Num. = 250 scenario
# snpl.250.rep <- read.csv('Results/snpl.250.rep.csv')
# snpl.250.part <- snpl.df.partition( snpl.250.rep )
# # Clean-up
# rm( snpl.250.rep )
# # Point to BRT_Model storage Location
# brt.model.stor <- '/Users/Matthew/Projects/FL-Shore-Birds/BRT_Models/'
# ###brt.model.stor <- '/Users/mlammens/Documents/StonyBrook-BEE/FL-shorebird/Sensitivity-Analysis-Data-Local/BRT_Models/'
# # Loop through the snpl.250.part list, and execute the brt analysis
# snpl.250.brt.results <- vector(mode="list",length=0)
# for ( df.num in 1:length(snpl.250.part) ){
#   snpl <- snpl.250.part[[ df.num ]]
#   # Determine which gbm.response variable to use
#   ifelse ( length(snpl) == 68, gbm.response <- abs.resp, gbm.response <- rel.resp )
#   # Call brt analysis function
#   brt.file.base.name <- paste( brt.model.stor,'snpl.250.ema.',
#                                df.num, sep="")
#   snpl.250.brt.results[[ df.num ]] <- snpl.run.brt( snpl, gbm.predictors, gbm.response, 
#                                                     save.brt=TRUE, brt.file.basename=brt.file.base.name,
#                                                     learning.rate=0.001)
# }
# # Assign names to snpl.250.brt.results
# names(snpl.250.brt.results) <- names(snpl.250.part)
# save( snpl.250.brt.results, file="Results/snpl.250.ema.brt.results.RData" )

## If the above has already been run, then just load in the
## results, which should have been saved to disk.
load( 'Results/snpl.250.ema.brt.results.RData' )
  
## **************************************************************##
## Section 8: Relative Influence (Importance) data.frame construction

# Get the element names for the xxx.results lists - should be the
# same for all Part.Num. cases
scenario.names <- names( snpl.10k.brt.results )
# Change scenario names to make visualization and anlaysis easier
# later
scenario.names[5:6] <- c("lhs.relEnds","unif.relEnds")
# Make a  vector classifing scenarios as abs or rel end
abs.rel.vector <- ifelse( grepl( pattern="rel", scenario.names ), yes="rel", no="abs" )
# Make a vector for the slr scenario ("nocc" or "2m")
slr.vector <- ifelse( grepl( pattern="nocc", scenario.names ), yes="nocc", no="2m" )
# Make a list of the different scenarios - note that though the
# .RData files have the ema tag in them, the actual saved list
# structure does not.
snpl.brt.results <- list( snpl.100.brt.results, snpl.250.brt.results, 
                          snpl.500.brt.results, snpl.1000.brt.results )

# Create a new list object to store all rel.inf data.frames
brt.summ.df.complete <- vector( mode='list', length=0 )
# Loop through list of lists of results
for ( part.num.ind in 1:length(snpl.brt.results)) {
  part.num.result <- snpl.brt.results[[ part.num.ind ]]
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
names( brt.summ.df.complete) <- c( 'snpl.100', 'snpl.250', 'snpl.500', 'snpl.1000' )
# Clean-up
rm( brt.part.num.summ.df, brt.summ.list )

## **************************************************************##
## Section 9: Correlation matrix construction
calc.cor.brt.summary <- function( part.num.list, cor.type="pearson" ) {
  return( lapply( part.num.list, cor, method=cor.type ) )
}

brt.corr.complete <- lapply( brt.summ.df.complete, calc.cor.brt.summary, cor.type="pearson" )
#brt.corr.complete <- lapply( brt.summ.df.complete, calc.cor.brt.summary, cor.type="spearman" )
#brt.corr.complete <- lapply( brt.summ.df.complete, calc.cor.brt.summary, cor.type="kendall" )


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
brt.corr.reference$part.num <- factor( brt.corr.reference$part.num, c('snpl.100','snpl.250',
                                                                      'snpl.500','snpl.1000') )

## Use ggplot to have a look at our results in box-plot form
ggplot( brt.corr.reference, aes(samp,cor.val)) + geom_boxplot() + facet_grid( slr~part.num, scales='free_y' )

ggsave(file='pearson-cor-boxplots-ema.pdf')