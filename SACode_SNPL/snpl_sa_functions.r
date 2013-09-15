## **************************************************************##
## File: snpl_sa_functions.r
## Author: Matthew Aiello-Lammens
## Data: 12 January 2013
##
## Purpose: This file includes R code for the functions used to
## analyze the data
## generated and collected to investigate methods of performing
## sensitivity analysis on the Florida Snowy Plover population
## models.
##
## This file is a split from the snpl_sensitivity_analysis.R 
## script, which it self largely consisted of code that was 
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
  ##  snpl.dfs.list: A list element containing six data.frames
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
                          brt.file.name = 'brt.file', learning.rate=0.001 ) {
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
                              bag.fraction=0.5)#, tolerance.method="fixed",tolerance=0.001)
    # Put a while loop here that checks for whether the brt actually fit the
    # data. If it did not, then the snpl.sub.brt variable will be NULL
    while( is.null( snpl.sub.brt ) ){
      # Print a warning message
      print('### Warning: no model fit - retrying now ###')
      snpl.sub.brt <- gbm.step( snpl.sub, gbm.x=gbm.predictors, gbm.y=gbm.response,
                                family="gaussian", tree.complexity=4, learning.rate=learning.rate,
                                bag.fraction=0.5)#, tolerance.method="fixed",tolerance=0.001)
    }
    
    # Add the brt results to snpl.brt.list
    snpl.brt.summ.list[[ Rep ]] <- summary( snpl.sub.brt )
    snpl.brt.int.list[[ Rep ]] <- gbm.interactions( gbm.object=snpl.sub.brt )
    
    # Save brt model to file - these models can be investigated later
    if ( save.brt ) {
      brt.file <- paste( brt.file.name, Rep, 'RData', sep="." )
      save( snpl.sub.brt, file=brt.file )
    }
  }
  # Clean-up
  rm( snpl.sub, envir = .GlobalEnv ) 
  return( list(brt.summary=snpl.brt.summ.list, brt.interactions=snpl.brt.int.list) )
}

snpl.run.brt.ema <- function( snpl.brt.run, gbm.predictors, gbm.response, save.brt = FALSE,
                          brt.file.name = 'brt.file', learning.rate=0.001 ) {
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
      brt.file <- paste( brt.file.name, Rep, 'RData', sep="." )
      save( snpl.sub.brt, file=brt.file )
    }
  }
  # Clean-up
  rm( snpl.sub, envir = .GlobalEnv ) 
  return( list(brt.summary=snpl.brt.summ.list, brt.interactions=snpl.brt.int.list) )
}

snpl.run.mult.brt <- function( snpl.csv, brt.model.stor='/Projects/SnowyPlover/BRT_Models/',
                               brt.file.base.name, learning.rate=0.001 ){
  ## Call snpl.run.brt on a given 'Replicate Number' dataset (40k *.mp files) 
  ##
  ## Args:
  ##   snpl.csv: A *.csv file that is the result of mp.mult.results.r.
  ##     For the snpl analyses, these csv files are 40k rows by 68 col
  ##
  ##   brt.model.stor: A location on disk where individual brt models
  ##     are stored. Default is location of Akcakaya Group computer 
  ##
  ##   brt.file.base.name: Base name to be given to the stored brt model files,
  ##     for example, 'snpl.10k.SA2' or 'snpl.10k'. This name is given
  ##     a suffix corresponding to the data.frame number within the part.list.
  ##     Each df.num corresponds to a different Sampling Method - SLR scenario
  ##     (1=lhs.nocc, 2=lhs.2m, 3=unif.nocc, 4=unif.2m, 5=relEnds.lhs, 6=relEnds.unif)
  ##
  ##   learning.rate: Learning rate value to be passed to brt analysis
  ##
  ## Returns:
  ##   snpl.brt.results: A list structure of brt summary and interactions
  ##   results. Should be 6 elements, each which has 2 additional elements
  
  # Print file being analyzed
  print( paste( 'Begin BRT Analysis on:', snpl.csv ) )
  # Read csv file and partition
  snpl.rep <- read.csv(snpl.csv)
  snpl.part <- snpl.df.partition( snpl.rep )
  # Clean-up
  rm( snpl.rep )
  # Loop through the snpl.part list, and execute the brt analysis
  snpl.brt.results <- vector(mode="list",length=0)
  for ( df.num in 1:length(snpl.part) ){
    snpl <- snpl.part[[ df.num ]]
    # Determine which gbm.response variable to use
    ifelse ( length(snpl) == 68, gbm.response <- abs.resp, gbm.response <- rel.resp )
    # Call brt analysis function
    brt.file.name <- paste( brt.model.stor,brt.file.base.name,
                                 df.num, sep="")
    snpl.brt.results[[ df.num ]] <- snpl.run.brt( snpl, gbm.predictors, gbm.response, 
                                                      save.brt=TRUE, brt.file.name=brt.file.name,
                                                      learning.rate=learning.rate)
  }
  # Assign names to snpl.brt.results
  names(snpl.brt.results) <- names(snpl.part)
  return( snpl.brt.results )
}

snpl.run.mult.brt.ema <- function( snpl.csv, brt.model.stor='/Projects/SnowyPlover/BRT_Models/',
                               brt.file.base.name, learning.rate=0.001 ){
  ## Call snpl.run.brt.ema on a given 'Replicate Number' dataset (40k *.mp files) 
  ## **NOTE** This function differs from snpl.run.mult.brt in that it contains
  ## settings specific to the brt analysis of the endpoint EMA
  ## Specifically, this requires setting `tolerance.method=fixed` and
  ## `tolerance=0.001`, as is done in the function snpl.run.brt.ema
  ##
  ## Args:
  ##   snpl.csv: A *.csv file that is the result of mp.mult.results.r.
  ##     For the snpl analyses, these csv files are 40k rows by 68 col
  ##
  ##   brt.model.stor: A location on disk where individual brt models
  ##     are stored. Default is location of Akcakaya Group computer 
  ##
  ##   brt.file.base.name: Base name to be given to the stored brt model files,
  ##     for example, 'snpl.10k.SA2' or 'snpl.10k'. This name is given
  ##     a suffix corresponding to the data.frame number within the part.list.
  ##     Each df.num corresponds to a different Sampling Method - SLR scenario
  ##     (1=lhs.nocc, 2=lhs.2m, 3=unif.nocc, 4=unif.2m, 5=relEnds.lhs, 6=relEnds.unif)
  ##
  ##   learning.rate: Learning rate value to be passed to brt analysis
  ##
  ## Returns:
  ##   snpl.brt.results: A list structure of brt summary and interactions
  ##   results. Should be 6 elements, each which has 2 additional elements
  
  # Print file being analyzed
  print( paste( 'Begin BRT Analysis on:', snpl.csv ) )
  # Read csv file and partition
  snpl.rep <- read.csv(snpl.csv)
  snpl.part <- snpl.df.partition( snpl.rep )
  # Clean-up
  rm( snpl.rep )
  # Loop through the snpl.part list, and execute the brt analysis
  snpl.brt.results <- vector(mode="list",length=0)
  for ( df.num in 1:length(snpl.part) ){
    snpl <- snpl.part[[ df.num ]]
    # Determine which gbm.response variable to use
    ifelse ( length(snpl) == 68, gbm.response <- abs.resp, gbm.response <- rel.resp )
    # Call brt analysis function
    brt.file.name <- paste( brt.model.stor,brt.file.base.name,
                            df.num, sep="")
    snpl.brt.results[[ df.num ]] <- snpl.run.brt.ema( snpl, gbm.predictors, gbm.response, 
                                                  save.brt=TRUE, brt.file.name=brt.file.name,
                                                  learning.rate=learning.rate)
  }
  # Assign names to snpl.brt.results
  names(snpl.brt.results) <- names(snpl.part)
  return( snpl.brt.results )
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
