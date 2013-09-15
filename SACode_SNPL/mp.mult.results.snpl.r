mp.mult.results.snpl <- function( mp.file.list, out.csv, spatial=FALSE, mac=FALSE ) {
# Call mp.results for a list of files and export results as a csv file
#
# Author: Matthew Aiello-Lammens
# Created: 23 January 2011
#
# Args:
#	mp.file.list: a text file of the list of *.mp files for which results should be extracted
# 
# Returns:
#	No direct returns. Creates a CSV file of input and output metrics for multiple
#	*.mp files
#
#
###################################################################################################

#mp.list <- readLines( mp.file.list )
# Adjusted to take in a vector of files, rather than a .txt or .csv file
mp.list <- mp.file.list
mp.file.cnt <- length( mp.list )

mp.res.colnames <- c("file","metapop.initab","Gen.Abar","Gen.Mu1","Gen.Tgen","EigenVal",
                     "dd.type","Rmax","GrowthRt","fec.stdev.avg","surv.stdev.avg",
                     "stdev.avg","fec.cv.avg","surv.cv.avg","cv.avg","N.maxVmin.10",
                     "N.plusVmin.SD","N.CV.10","St.First.Rep","St.First.Rep.Name",
                     "ext.risk","threshold","prob.thresh.maxt","med.time.cross","prob.50",
                     "prob.250","prob.1000","prob.Thr.50","exp.min.n","sderr.ema","n.mean",
                     "n.stdev","metapop.chng","occ.maxt","occ.stdev.maxt","quant.05",
                     "quant.25","quant.50","quant.75","quant.95","harv.avg","harv.stdev",
                     "harv.min","harv.max","use.disp.dist.func","avg.disp.dist.b",
                     "max.disp.dist.Dmax","mean.t0.disp.rate","loquart.t0.disp.rate",
                     "hiquart.t0.disp.rate","mean.t0.Ngt0.disp.rate",
                     "loquart.t0.Ngt0.disp.rate","hiquart.t0.Ngt0.disp.rate",
                     "mean.t0.nearest.disp.rate","loquart.t0.nearest.disp.rate",
                     "hiquart.t0.nearest.disp.rate","mean.t0.Ngt0.nearest.disp.rate",
                     "loquart.t0.Ngt0.nearest.disp.rate","hiquart.t0.Ngt0.nearest.disp.rate",
                     "use.corr.dist.func","avg.corr.dist.b","kch.type","ad.surv","fecund",
                     "RandType","RepNumber","SLR","SampSize")
# Write column names to the output file
write.table( t(mp.res.colnames), file=out.csv, col.names=FALSE, row.names=FALSE,
             sep=",",append=TRUE)

###mp.mult.res <- vector()
for ( mp in 1:mp.file.cnt ) {
  print(paste('******** Extracting Data from:',mp.list[mp] ,'********'))
  # SNPL Specific data extraction - assumes a very specific file path format
  run.info <- unlist(strsplit( mp.list[mp], split='/'))
  rl <- length(run.info)
  # Random Sampling Type - either LHS or Unif
  rand.type <- sub("_.*","",run.info[ rl-2 ])
  # Replicate Number - Not applicable for PartNum=10k
  rep.number <- sub(".*_","",run.info[ rl-2 ])
  # Sea-level rise scenario - either 2M or NoCC
  slr <- sub("_Ceiling","",run.info[ rl-1 ])
  slr <- sub(".*_","",slr)
  # SampSize is an old way of saying Partition Number (PartNum)
  SampSize <- sub("PartNum_","",run.info[ rl-3 ])
  # Combine this information into a small data.frame
  run.info.df <- data.frame( RandType=rand.type, RepNumber=rep.number, SLR=slr, SampSize=SampSize )
  
  # Call mp.results.r on this file. This returns 64 columns
  mp.res <- mp.results( mp.list[mp], spatial=spatial, mac=mac )
  # Combine the snpl specific values - 
  mp.res <- cbind( mp.res, run.info.df )
  ###mp.mult.res <- rbind( mp.mult.res, mp.res )
  write.table( mp.res, file=out.csv, append=TRUE, col.names=FALSE,sep=",")
}

###write.csv( mp.mult.res, out.csv )
}
