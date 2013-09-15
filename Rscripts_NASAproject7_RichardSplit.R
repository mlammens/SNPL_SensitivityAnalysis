######################################################
######     SET WORKING DIRECTORY

#setwd("C:/RGP_docs/Results_analysis_(Kevin)")
setwd("C:/Users/Kevin/Desktop/NASA Analysis")
#setwd("C:\\Users\\Kevin\\Desktop\\NASA_meeting_0181412")

##############################
#######  LOAD PACKAGES

library(gbm)
library(dismo)
library(scales)
library(ggplot2)
library(lattice)
library(ipred)
library(party)
library(RColorBrewer)
library(tree)
library(languageR)
library(rms)
library(ROCR)
library(rpart)


################################################################
#####################################  LOAD NEW FUNCTIONS


#################################################
######################  NEW FUNCTION FOR PLOTTING SUBTREES FROM BOOSTED REGRESSION MODELS  ...

## plot_subtree: function to visualize the subtrees for BRT analyses...
##    KTS 7/1/2012  

plot_subtree = function(brt_model, data, pred_cols, lr, scenario_name, response_name, tree_ndx = 1000){
    #  which component tree to visualize??
	sepdist <- 5   
	frame <- pretty.gbm.tree(brt_model,i.tree=tree_ndx)
	pred_names_ndx <- names(data)[pred_cols]
	splitVars <- pred_names_ndx[frame$SplitVar[which(frame$SplitVar!=-1)]+1]
	nsplits <- length(splitVars)                    
	yoffset <- sum(frame$ErrorReduction*1.3)*.02
	graphics.off()
	par(mai=c(.1,.1,.5,.1))
      	# set plot area
	plot(1,1,type="n",xlim=c(-10,20),ylim=c(0,sum(frame$ErrorReduction*1.3)),bty="n",
            xaxt="n",yaxt="n",xlab="",ylab="",main=paste(response_name,scenario_name,"tree",tree_ndx,sep=" "))
	nodecoords <- list(left=list(x=numeric(nsplits),y=numeric(nsplits)),right=list(x=numeric(nsplits),y=numeric(nsplits)))
	nodecoords_base <- c(5,sum(frame$ErrorReduction)*1.2)  ### list(x=c(5,4),y=c(sum(frame$ErrorReduction),sum(frame$ErrorReduction[-1])))  #base...
	nodecoords$left$x[1]=nodecoords_base[1]-sepdist; nodecoords$left$y[1]=nodecoords_base[2]-frame$ErrorReduction[1]
	nodecoords$right$x[1]=nodecoords_base[1]+sepdist; nodecoords$right$y[1]=nodecoords_base[2]-frame$ErrorReduction[1]
	text(nodecoords_base[1],nodecoords_base[2]+yoffset,paste(splitVars[1],"<",round(frame$SplitCodePred[1],2),sep=""))
	points(c(nodecoords_base[1],nodecoords$left$x[1]),c(nodecoords_base[2],nodecoords$left$y[1]),type="l")
	points(c(nodecoords_base[1],nodecoords$right$x[1]),c(nodecoords_base[2],nodecoords$right$y[1]),type="l")
	counter1=1
	splitndx <- numeric(nsplits)
	splitndx[counter1] <- 1      # loop variable at each split...
	for(i in 2:nrow(frame)){
	  if(frame$SplitVar[i]!=-1){   # if not a terminal leaf...
	    counter1=counter1 + 1
	    splitndx[counter1] <- i   # what is the value of the loop variable at this split?
	    sepdist = sepdist*0.8
	    origin_node = which((frame$LeftNode+1)==i|(frame$RightNode+1)==i) 
	    ndx <- which(origin_node==splitndx)  
	    left <- ifelse((frame$LeftNode[origin_node]+1)==i,1,2)
	    text(nodecoords[[left]]$x[ndx],nodecoords[[left]]$y[ndx]+yoffset,paste(splitVars[counter1],"<",round(frame$SplitCodePred[i],2),sep=""))   # split variable...
	    nodecoords$left$x[counter1]=nodecoords[[left]]$x[ndx]-sepdist
	    nodecoords$left$y[counter1]=nodecoords[[left]]$y[ndx]-frame$ErrorReduction[i]
	    nodecoords$right$x[counter1]=nodecoords[[left]]$x[ndx]+sepdist
	    nodecoords$right$y[counter1]=nodecoords[[left]]$y[ndx]-frame$ErrorReduction[i]
	    points(c(nodecoords[[left]]$x[ndx],nodecoords$left$x[counter1]),c(nodecoords[[left]]$y[ndx],nodecoords$left$y[counter1]),type="l")
	    points(c(nodecoords[[left]]$x[ndx],nodecoords$right$x[counter1]),c(nodecoords[[left]]$y[ndx],nodecoords$right$y[counter1]),type="l")   
	  }
	  if(frame$SplitVar[i]==-1){   # if a terminal leaf...
	    origin_node = which((frame$LeftNode+1)==i|(frame$RightNode+1)==i)
	    if(length(origin_node)>0){ 
	      ndx <- which(origin_node==splitndx)  
	      left <- ifelse((frame$LeftNode[origin_node]+1)==i,1,2)
	      text(nodecoords[[left]]$x[ndx],nodecoords[[left]]$y[ndx]-yoffset,paste(round(eval(parse(text=workingModel))$initF+(frame$Prediction[i]/lr),4),sep=""))   # split variable...   
	    }
	  }
	}
}


#############################################
#################### NEW FUNCTION: "find_fraction" for determining the fraction
  #  of the total observations to sample without replacement for each tree in the random forest
  #  or for each term of the boosted model 


find_fraction <- function(dataframe){
  len <- nrow(dataframe)
  nspec <- length(unique(dataframe$spec))  # number of geographic configurations...
  target <- nspec-1
  testarray <- rep(c(1:nspec),each=floor(len/nspec))  
  obj <- optimize(subsample_fn,c(0.005,0.2),target=target,array=testarray)   #(par,target,testarray)
  return(obj$minimum)
}

subsample_fn <- function(par,target,array){
  dif <- numeric(200)
  for(i in 1:200){
    dif[i] <- abs(target-length(unique(sample(array,length(array)*par,replace=F))))
  }
  a <- mean(dif)
  return(a)
}

#################################################
######################  NEW GBM.PERSPEC FUNCTION...

# rewrite the perspective plotting function

gbm.perspec <- function (gbm.object, x = 1, y = 2, pred.means = NULL, x.label = NULL, 
    x.range = NULL, y.label = NULL, z.label = "fitted value", 
    y.range = NULL, z.range = NULL, leg.coords = NULL, ticktype = "detailed", 
    theta = 55, phi = 40, smooth = "none", mask = FALSE, perspective = TRUE, 
    ...) 
{
    if (!require(gbm)) {
        stop("you need to install the gbm package to use this function")
    }
    if (!require(splines)) {
        stop("you need to install the splines package to use this function")
    }
    gbm.call <- gbm.object$gbm.call
    gbm.x <- gbm.call$gbm.x
    n.preds <- length(gbm.x)
    gbm.y <- gbm.call$gbm.y
    pred.names <- gbm.call$predictor.names
    family = gbm.call$family
    have.factor <- FALSE
    x.name <- gbm.call$predictor.names[x]
    if (is.null(x.label)) {
        x.label <- gbm.call$predictor.names[x]
    }
    y.name <- gbm.call$predictor.names[y]
    if (is.null(y.label)) {
        y.label <- gbm.call$predictor.names[y]
    }
    data <- eval(parse(text = gbm.call$dataframe))[, gbm.x]
    n.trees <- gbm.call$best.trees
    if (is.vector(data[, x])) {
        if (is.null(x.range)) {
            x.var <- seq(min(data[, x], na.rm = T), max(data[, 
                x], na.rm = T), length = 50)
        } else {
            x.var <- seq(x.range[1], x.range[2], length = 50)
        }
    } else {
        x.var <- names(table(data[, x]))
        have.factor <- TRUE
    }
    if (is.vector(data[, y])) {
        if (is.null(y.range)) {
            y.var <- seq(min(data[, y], na.rm = T), max(data[, 
                y], na.rm = T), length = 50)
        } else {
            y.var <- seq(y.range[1], y.range[2], length = 50)
        }
    } else {
        y.var <- names(table(data[, y]))
        #if (have.factor) {
        #    stop("at least one marginal predictor must be a vector!")
        #}
        #else {
            have.factor <- TRUE
        #}
    }
    pred.frame <- expand.grid(list(x.var, y.var))
    names(pred.frame) <- c(x.name, y.name)
    pred.rows <- nrow(pred.frame)
    #if (have.factor) {
    #    if (is.factor(pred.frame[, 2])) {
    #        pred.frame <- pred.frame[, c(2, 1)]
    #        x.var <- y.var
    #    }
    #}
    j <- 3
    for (i in 1:n.preds) {
        if (i != x & i != y) {
            if (is.vector(data[, i])) {
                m <- match(pred.names[i], names(pred.means))
                if (is.na(m)) {
                  pred.frame[, j] <- mean(data[, i], na.rm = T)
                }
                else pred.frame[, j] <- pred.means[m]
            }
            if (is.factor(data[, i])) {
                m <- match(pred.names[i], names(pred.means))
                temp.table <- table(data[, i])
                if (is.na(m)) {
                  pred.frame[, j] <- rep(names(temp.table)[2], 
                    pred.rows)
                }
                else {
                  pred.frame[, j] <- pred.means[m]
                }
                pred.frame[, j] <- factor(pred.frame[, j], levels = names(temp.table))
            }
            names(pred.frame)[j] <- pred.names[i]
            j <- j + 1
        }
    }
    prediction <- predict.gbm(gbm.object, pred.frame, n.trees = n.trees, 
        type = "response")
    if (smooth == "model") {
        pred.glm <- glm(prediction ~ ns(pred.frame[, 1], df = 8) * 
            ns(pred.frame[, 2], df = 8), data = pred.frame, family = poisson)
        prediction <- fitted(pred.glm)
    }
    max.pred <- max(prediction)
    min.pred <- min(prediction)
    cat("maximum value = ", round(max.pred, 2), "\n")
    if (is.null(z.range)) {
        if (family == "bernoulli") {
            z.range <- c(0, 1)
        } else if (family == "poisson") {
            z.range <- c(0, max.pred * 1.1)
        } else {
            #z.min <- min(data[, y], na.rm = T)
            #z.max <- max(data[, y], na.rm = T)
            #z.delta <- z.max - z.min
            #z.range <- c(z.min - (1.1 * z.delta), z.max + (1.1 * 
            #    z.delta))
            z.range <- c(min.pred,max.pred)
        }
    }
    if (have.factor == FALSE) {
        pred.matrix <- matrix(prediction, ncol = 50, nrow = 50)
        if (smooth == "average") {
            pred.matrix.smooth <- pred.matrix
            for (i in 2:49) {
                for (j in 2:49) {
                  pred.matrix.smooth[i, j] <- mean(pred.matrix[c((i - 
                    1):(i + 1)), c((j - 1):(j + 1))])
                }
            }
            pred.matrix <- pred.matrix.smooth
        }
        if (mask) {
            mask.trees <- gbm.object$gbm.call$best.trees
            point.prob <- predict.gbm(gbm.object[[1]], pred.frame, 
                n.trees = mask.trees, type = "response")
            point.prob <- matrix(point.prob, ncol = 50, nrow = 50)
            pred.matrix[point.prob < 0.5] <- 0
        }
        if (!perspective) {
            image(x = x.var, y = y.var, z = pred.matrix, zlim = z.range)
        }
        else {
            persp(x = x.var, y = y.var, z = pred.matrix, zlim = z.range, 
                xlab = x.label, ylab = y.label, zlab = z.label, 
                theta = theta, phi = phi, r = sqrt(10), d = 3, 
                ticktype = ticktype, mgp = c(4, 1, 0), ...)
        }
    }
    if (have.factor) {
        factor.list <- names(table(pred.frame[, 1]))
        n <- 1
        if (is.null(z.range)) {
            vert.limits <- c(min.pred * 1.1, max.pred * 1.1)
        } else {
            vert.limits <- z.range
        }
        plot(as.numeric(pred.frame[pred.frame[, 1] == factor.list[1], 2]), 
            prediction[pred.frame[, 1] == factor.list[1]], type = "n", pch=NA,
            ylim = vert.limits, xlab = y.label, ylab = z.label,xaxt="n")
        if(is.factor(pred.frame[, 2])) xaxlab <- names(table(pred.frame[, 2]))
        if(is.vector(pred.frame[, 2])) xaxlab <- seq(min(pred.frame[, 2]),max(pred.frame[, 2]),length=5)
        axis(1,at=c(1:length(xaxlab)),labels=xaxlab)
        for (i in 1:length(factor.list)) {
            factor.level <- factor.list[i]
            #if()
            lines(pred.frame[pred.frame[, 1] == factor.level, 
                2], prediction[pred.frame[, 1] == factor.level], 
                lty = i)
        }
        if (is.null(leg.coords)) {
            #x.max <- max(pred.frame[, 2])
            #x.min <- min(pred.frame[, 2])
            #x.range <- x.max - x.min
            #x.pos <- c(x.min + (0.02 * x.range), x.min + (0.3 * 
            #    x.range))
            #y.max <- max(prediction)
            #y.min <- min(prediction)
            #y.range <- y.max - y.min
            #y.pos <- c(y.min + (0.8 * y.range), y.min + (0.95 * 
            #    y.range))
            legend(locator(1), factor.list, lty = c(1:length(factor.list)),    #x = x.pos, y = y.pos, 
                bty = "n")
        }
        else {
            legend(x = leg.coords[1], y = leg.coords[2], factor.list,     #  
                lty = c(1:length(factor.list)), bty = "n")
        }
    }
}

#############################################################
##########  FUNCTION makeResponseMatrix: for use in Random Forest univariate and interaction plots
#####  makes a new data frame of response variables for prediction, holding all
####   variables constant at mean levels except for the variable(s) of interest....

makeResponseMatrix <- function(k,RFObject,pred.data,n.preds,pred.names,data,rf.x){

         # initialize the predictor data frame...
  subset2 <- data.frame(nullcol = rep(NA,times=100)) 

         # loop through predictor variables
  for(v in 1:n.preds){       	
    if(v==k){ 
       subset2 <- cbind(subset2,seq(min(pred.data),max(pred.data),length=100))    # if the variable of interest, use the observed values
       names(subset2)[v+1] <- pred.names[v]
    } else{
       subset2 <- cbind(subset2,rep(mean(data[,rf.x[v]]),times=100))   # if NOT the variable of interest, replicate the mean value
       names(subset2)[v+1] <- pred.names[v]
    }
  }
	
     # compile a data frame (subset2) for prediction. 
     # generate a prediction from random forest model

     # check to make sure the predictors are all of the same class
  for(v in 2:(n.preds+1)){
    if(class(data[,rf.x[v-1]])!=class(subset2[,v])) subset2[,v] <- eval(parse(text=paste("as.",class(data[,rf.x[v-1]]),"(subset2[,v])",sep="")))
  }

  predictions <- predict(RFObject,newdata=subset2)
  df <- data.frame(v1=seq(min(pred.data),max(pred.data),length=100),v2=predictions)
  names(df) <- c(pred.names[k],"y")
  return(df) 
}

#######################################################################
########################################################################
#        NEW FUNCTION: makefoldvec: for cross validation in RF and BRT

   # define a fold vector so that the holdout samples are spatially uncorrelated from the training sets
make_foldvec <- function(n.folds){
  ##n.folds = 12
  nspecies <- max(as.numeric(subset1$spec))
  tempfolds1 <- numeric(nspecies)
  temp2 <- c(1:nspecies)
  reps <- floor(nspecies/n.folds)
  counter1 = 1
  counter2 = reps
  foldlink = numeric(nspecies)
  foldlink[1:(n.folds*counter2)] <- rep(c(1:n.folds),each=counter2)
  foldlink[which(foldlink==0)] <- n.folds
  for(i in 1:(n.folds)){
    temp3 <- sample(temp2,reps,replace=F)
    tempfolds1[counter1:counter2] <- temp3
    temp2 <- temp2[-which(temp2%in%temp3)]
    counter1 = counter1 + reps
    counter2 = counter2 + reps
  }
  if(length(which(tempfolds1==0)>0)) tempfolds1[which(tempfolds1==0)] <- temp2
  foldVector <- foldlink[match(as.numeric(subset1$spec),tempfolds1)]
  if(n.folds == length(unique(subset1$spec))) foldVector <- as.numeric(subset1$spec)
  return(foldVector)
}



############################################
##################  NEW FUNCTION: RF_UnivariatePlots
######  For use with random forest object from cforest
######  displays univariate plots, analogous to "gbm.plot" in the "dismo" package  


RF_UnivariatePlots <- function(object, varimp, data,predictors,plot.layout){

      # gbm.object = eval(parse(text=workingModel))
    graphics.off()
    par(mfrow=plot.layout)
    smooth = FALSE
    rug = FALSE
    common.scale = TRUE
    write.title = FALSE 
    y.label = "fitted function"
    x.label = NULL
    show.contrib = TRUE
    n.plots = min(prod(plot.layout),length(predictors))

    rf.x <- which(names(data)%in%predictors)     
    pred.names = names(data[,rf.x])  #  predictors
    #n.plots = length(pred.names) 
     #variable.no = 0

    original.response <- data.cforest@data@get("response")   # OR...   data.cforest@responses@test_trafo
    response.name <- names(data.cforest@responses@variables)  # gbm.call$response.name
    max.plots <- plot.layout[1] * plot.layout[2]
    plot.count <- 0
    n.pages <- 1
    max.vars <- length(predictors)
    if (n.plots > max.vars) {
        n.plots <- max.vars
        warning("reducing no of plotted predictors to maximum available (", 
            max.vars, ")")
    }
    predictors3 <- list(rep(NA, n.plots))
    responses <- list(rep(NA, n.plots))
    ymin <- numeric(n.plots)
    ymax <- numeric(n.plots)
    RFPredContributions <- names(varimp)[order(varimp,decreasing=T)]

    for (j in c(1:n.plots)) { 
	  k <- match(RFPredContributions[j],pred.names) # variable to plot as predictor
        if (is.null(x.label)){ 
            var.name <- RFPredContributions[j]
        } else var.name <- x.label
        pred.data <- data[,which(names(data)==pred.names[k])]          
        response.matrix <- makeResponseMatrix(k,object,pred.data,length(pred.names),pred.names,data,rf.x)   # prediction, holding all other predictors at mean values...
        predictors3[[j]] <- response.matrix[, 1]
        if (is.factor(data[, rf.x[k]])) {
            predictors3[[j]] <- factor(predictors3[[j]], levels = levels(data[,rf.x[k]]))
        }
        responses[[j]] <- response.matrix[, 2] - mean(response.matrix[,2])
        if (j == 1) {
            ymin[j] = min(responses[[j]])
            ymax[j] = max(responses[[j]])
        }else {
           ymin[j] = min(responses[[j]])
           ymax[j] = max(responses[[j]]) #ymin = min(ymin, min(responses[[j]]))
            					#ymax = max(ymax, max(responses[[j]]))
        }
    }

    for (j in c(1:n.plots)) {
        if (plot.count == max.plots) {
            plot.count = 0
            n.pages <- n.pages + 1
        }
        plot.count <- plot.count + 1
        #if (n.plots == 1) {
        #    k <- match(pred.names[variable.no],RFPredContributions)
        #    if (show.contrib) {
        #        x.label <- paste(var.name, "  (", round(varimp[k], 1), ")", sep = "")
        #    }
        #}else {
        
        k <- match(RFPredContributions[j],pred.names)
        var.name <- pred.names[k]
            #if (show.contrib) {
        x.label <- paste(var.name, "  (", round(varimp[order(varimp,decreasing=T)][j], 2), ")", sep = "")
            #}else x.label <- var.name
        #}
        #if (common.scale) {
                 #inflationFactor <- abs(max(data.cforest.varimp[order(data.cforest.varimp,decreasing=T)])/data.cforest.varimp[order(data.cforest.varimp,decreasing=T)][j])
            plot(predictors3[[j]], responses[[j]], ylim = c(ymin[j],
                ymax[j]), type = "l", xlab = x.label, ylab = y.label)
        #}else {
        #    plot(predictors3[[j]], responses[[j]], type = "l", 
        #        xlab = x.label, ylab = y.label)
        #}

        #if (smooth & is.vector(predictors3[[j]])) {
        #    temp.lo <- loess(responses[[j]] ~ predictors3[[j]], 
        #        span = 0.3)
        #    lines(predictors3[[j]], fitted(temp.lo), lty = 2, 
        #        col = 2)
        #}

        if (plot.count == 1) {
            if (write.title) {
                title(paste(response.name, " - page ", n.pages, 
                  sep = ""))
            }
            #if (rug & is.vector(data[, rf.x[variable.no]])) {
            #    rug(quantile(data[, rf.x[variable.no]], 
            #      probs = seq(0, 1, 0.1), na.rm = TRUE))
            #}
        }else {
            if (write.title & j == 1) {
                title(response.name)
            }
            #if (rug & is.vector(data[, rf.x[k]])) {
            #    rug(quantile(data[, rf.x[k]], probs = seq(0, 
            #      1, 0.1), na.rm = TRUE))
            #}
        }
    }
     # par(op)
  
}


#################

#######################################################
###   NEW FUNCTION: "RF_FindInteractions" 
#                            Find interactions from Random forest algorithm


RF_FindInteractions <- function(object,data,predictors){

         # gbm.object <- eval(parse(text=workingModel))
         # RFObject <- data.cforest
         # n.trees <- gbm.call$best.trees
         # depth <- gbm.call$interaction.depth
    rf.x <- which(names(data)%in%predictors)     
    pred.names = names(data[,rf.x])  #  predictors
    original.response <- data.cforest@data@get("response")
    n.preds <- length(pred.names)
    cross.tab <- matrix(0, ncol = n.preds, nrow = n.preds)
    dimnames(cross.tab) <- list(pred.names, pred.names)

      #data <- eval(parse(text = gbm.call$dataframe))[, gbm.x]
    for (i in 1:(n.preds - 1)) {
        if (is.vector(data[, rf.x[i]])) {
            x.var <- seq(min(data[, rf.x[i]], na.rm = T), max(data[,rf.x[i]], na.rm = T), length = 20)
        }else {
            x.var <- factor(names(table(data[, rf.x[i]])), levels = levels(data[,rf.x[i]]))
        }
        x.length <- length(x.var)
        cat(i, " ")
        for (j in (i + 1):n.preds) {
            if (is.vector(data[, rf.x[j]])) {
                y.var <- seq(min(data[, rf.x[j]], na.rm = T), max(data[,rf.x[j]], na.rm = T), length = 20)
            }else {
                y.var <- factor(names(table(data[, rf.x[j]])), levels = levels(data[,rf.x[j]]))
            }
            y.length <- length(y.var)
            pred.frame <- expand.grid(list(x.var, y.var))
            names(pred.frame) <- c(pred.names[i], pred.names[j])

            if(class(data[,rf.x[i]])!=class(pred.frame[,1])) pred.frame[,1] <- eval(parse(text=paste("as.",class(data[,rf.x[i]]),"(pred.frame[,1])",sep="")))
            if(class(data[,rf.x[j]])!=class(pred.frame[,2])) pred.frame[,2] <- eval(parse(text=paste("as.",class(data[,rf.x[j]]),"(pred.frame[,2])",sep="")))
            n <- 3
            for (k in 1:n.preds) {
                if (k != i & k != j) {
                  if (is.vector(data[, rf.x[k]])) {
                    pred.frame[, n] <- mean(data[, rf.x[k]], na.rm = T)
                  }else {
                    temp.table <- sort(table(data[, rf.x[k]]), decreasing = TRUE)
                    pred.frame[, n] <- rep(names(temp.table)[1], x.length * y.length)
                    pred.frame[, n] <- as.factor(pred.frame[, n])
                  }
                  names(pred.frame)[n] <- pred.names[k]
                  if(class(data[,rf.x[k]])!=class(pred.frame[,n])) pred.frame[,n] <- eval(parse(text=paste("as.",class(data[,rf.x[k]]),"(pred.frame[,n])",sep="")))

                  n <- n + 1
                }
            }
            prediction <- predict(object,newdata=pred.frame)      #predict.gbm(gbm.object, pred.frame, 
                               #n.trees = n.trees, type = "link")
            interaction.test.model <- lm(prediction ~ as.factor(pred.frame[,1]) + as.factor(pred.frame[, 2]))
              #}
            interaction.flag <- round(mean(resid(interaction.test.model)^2) * 5000, 3)
            cross.tab[i, j] <- interaction.flag
        }
    }

            # rank in terms of importance
    search.index <- ((n.preds^2) + 1) - rank(cross.tab, ties.method = "first")
    n.important <- max(2, round(0.1 * ((n.preds^2)/2), 0))
    var1.names <- rep(" ", n.important)
    var1.index <- rep(0, n.important)
    var2.names <- rep(" ", n.important)
    var2.index <- rep(0, n.important)
    int.size <- rep(0, n.important)
    for (i in 1:n.important) {
        index.match <- match(i, search.index)
        j <- trunc(index.match/n.preds) + 1
        var1.index[i] <- j
        var1.names[i] <- pred.names[j]
        k <- index.match%%n.preds
        if (k > 0) {
            var2.index[i] <- k
            var2.names[i] <- pred.names[k]
            int.size[i] <- cross.tab[k, j]
        }
    }
    rank.list <- data.frame(var1.index, var1.names, var2.index, 
        var2.names, int.size)
      #cat("\n")
      #return(list(rank.list = rank.list, interactions = cross.tab, gbm.call = gbm.object$gbm.call))

    int_object <- list(rank.list=rank.list, interactions = cross.tab)
    return(int_object)

}

##############################
##############################


#################################
##################### NEW FUNCTION: PLOT IMPORTANCE FOR GLM

summary.gbm <- function (object, cBars = length(object$var.names), n.trees = object$n.trees, 
    plotit = TRUE, order = TRUE, method = relative.influence, 
    normalize = TRUE, ...) 
{
    if (n.trees < 1) {
        stop("n.trees must be greater than 0.")
    }
    if (n.trees > object$n.trees) {
        warning("Exceeded total number of GBM terms. Results use n.trees=", 
            object$n.trees, " terms.\n")
        n.trees <- object$n.trees
    }
    rel.inf <- method(object, n.trees)
    rel.inf[rel.inf < 0] <- 0
    if (order) {
        i <- order(-rel.inf)
    } else {
        i <- 1:length(rel.inf)
    }
    if (cBars == 0) 
        cBars <- min(10, length(object$var.names))
    if (cBars > length(object$var.names)) 
        cBars <- length(object$var.names)
    if (normalize) 
        rel.inf <- 100 * rel.inf/sum(rel.inf)
    if (plotit) {
        barplot(rel.inf[i[cBars:1]], horiz = TRUE, col = rainbow(cBars, 
            start = 3/6, end = 4/6), names = predictorNames[match(object$var.names,predictors)][i[cBars:1]], 
            xlab = "Relative influence", ...)
    }
    return(data.frame(var = object$var.names[i], rel.inf = rel.inf[i]))
}

#################################################
######################  NEW FUNCTION... PLOT INTERACTIONS FOR RANDOM FOREST....

# rewrite the perspective plotting function

RF_InteractionPlots <- function(x=1,y=6,object,data,predictors,family){

    pred.means = NULL
    x.label = NULL 
    x.range = NULL 
    y.label = NULL
    z.label = "fitted value" 
    y.range = NULL
    z.range = NULL
    leg.coords = NULL
    ticktype = "detailed" 
    theta = 55
    phi = 40
    smooth = "none"
    mask = FALSE
    perspective = TRUE 

    rf.x <- which(names(data)%in%predictors)     
    pred.names = names(data[,rf.x])  #  predictors
    original.response <- data.cforest@data@get("response")
    n.preds <- length(pred.names)

    rf.y <- which(names(data)==names(original.response))
    have.factor <- FALSE
    x.name <- pred.names[x]
    if (is.null(x.label)) {
        x.label <- x.name
    }
    y.name <- pred.names[y]
    if (is.null(y.label)) {
        y.label <- y.name
    }
    data2 <- data[,rf.x]    #subset out just the predictor variables...
      #n.trees <- gbm.call$best.trees
    if (is.vector(data2[, x])) {
        if (is.null(x.range)) {
            x.var <- seq(min(data2[,x], na.rm = T), max(data2[,x], na.rm = T), length = 50)
        } else {
            x.var <- seq(x.range[1], x.range[2], length = 50)
        }
    } else {
        x.var <- names(table(data[, x]))
        have.factor <- TRUE
    }
    if (is.vector(data2[, y])) {
        if (is.null(y.range)) {
            y.var <- seq(min(data2[,y], na.rm = T), max(data2[,y], na.rm = T), length = 50)
        } else {
            y.var <- seq(y.range[1], y.range[2], length = 50)
        }
    } else {
        y.var <- names(table(data[, y]))
        #if (have.factor) {
        #    stop("at least one marginal predictor must be a vector!")
        #}
        #else {
            have.factor <- TRUE
        #}
    }
    pred.frame <- expand.grid(list(x.var, y.var))
    names(pred.frame) <- c(x.name, y.name)
    pred.rows <- nrow(pred.frame)

    #if (have.factor) {
    #    if (is.factor(pred.frame[, 2])) {
    #        pred.frame <- pred.frame[, c(2, 1)]
    #        x.var <- y.var
    #    }
    #}
    j <- 3
    for (i in 1:n.preds) {
        if (i != x & i != y) {
            if (is.vector(data2[, i])) {
                m <- match(pred.names[i], names(pred.means))
                if (is.na(m)) {
                  pred.frame[, j] <- mean(data2[, i], na.rm = T)
                } else pred.frame[, j] <- pred.means[m]
            }
            if (is.factor(data2[, i])) {
                m <- match(pred.names[i], names(pred.means))
                temp.table <- table(data2[, i])
                if (is.na(m)) {
                  pred.frame[, j] <- rep(names(temp.table)[2],pred.rows)
                }else {
                  pred.frame[, j] <- pred.means[m]
                }
                pred.frame[, j] <- factor(pred.frame[, j], levels = names(temp.table))
            }
            names(pred.frame)[j] <- pred.names[i]
            if(class(data2[, i])!=class(pred.frame[,j])) pred.frame[,j] <- eval(parse(text=paste("as.",class(data2[, i]),"(pred.frame[,j])",sep="")))
            j <- j + 1
        }
    }
    prediction <- predict(object, newdata=pred.frame)
    #if (smooth == "model") {
    #    pred.glm <- glm(prediction ~ ns(pred.frame[, 1], df = 8) * 
    #        ns(pred.frame[, 2], df = 8), data = pred.frame, family = poisson)
    #    prediction <- fitted(pred.glm)
    #}
    max.pred <- max(prediction)
    min.pred <- min(prediction)
    cat("maximum value = ", round(max.pred, 2), "\n")
    if (is.null(z.range)) {
        if (family == "bernoulli") {
            z.range <- c(0, 1)
        } else if (family == "poisson") {
            z.range <- c(0, max.pred * 1.1)
        } else {
            #z.min <- min(data[, y], na.rm = T)
            #z.max <- max(data[, y], na.rm = T)
            #z.delta <- z.max - z.min
            #z.range <- c(z.min - (1.1 * z.delta), z.max + (1.1 * 
            #    z.delta))
            z.range <- c(min.pred,max.pred)
        }
    }
    if (have.factor == FALSE) {
        pred.matrix <- matrix(prediction, ncol = 50, nrow = 50)
        #if (smooth == "average") {
        #    pred.matrix.smooth <- pred.matrix
        #    for (i in 2:49) {
        #        for (j in 2:49) {
        #          pred.matrix.smooth[i, j] <- mean(pred.matrix[c((i - 1):(i + 1)), c((j - 1):(j + 1))])
        #        }
        #    }
        #    pred.matrix <- pred.matrix.smooth
        #}
        #if (mask) {
        #    mask.trees <- gbm.object$gbm.call$best.trees
        #    point.prob <- predict.gbm(gbm.object[[1]], pred.frame, 
        #        n.trees = mask.trees, type = "response")
        #    point.prob <- matrix(point.prob, ncol = 50, nrow = 50)
        #    pred.matrix[point.prob < 0.5] <- 0
        #}
        if (!perspective) {
            image(x = x.var, y = y.var, z = pred.matrix, zlim = z.range)
        } else {
            persp(x = x.var, y = y.var, z = pred.matrix, zlim = z.range, 
                xlab = x.label, ylab = y.label, zlab = z.label, 
                theta = theta, phi = phi, r = sqrt(10), d = 3, 
                ticktype = ticktype, mgp = c(4, 1, 0))
        }
    }
    if (have.factor) {
        factor.list <- names(table(pred.frame[, 1]))
        n <- 1
        if (is.null(z.range)) {
            vert.limits <- c(min.pred * 1.1, max.pred * 1.1)
        } else {
            vert.limits <- z.range
        }
        plot(as.numeric(pred.frame[pred.frame[, 1] == factor.list[1], 2]), 
            prediction[pred.frame[, 1] == factor.list[1]], type = "n", pch=NA,
            ylim = vert.limits, xlab = y.label, ylab = z.label,xaxt="n")
        if(is.factor(pred.frame[, 2])) xaxlab <- names(table(pred.frame[, 2]))
        if(is.vector(pred.frame[, 2])) xaxlab <- seq(min(pred.frame[, 2]),max(pred.frame[, 2]),length=5)
        axis(1,at=c(1:length(xaxlab)),labels=xaxlab)
        for (i in 1:length(factor.list)) {
            factor.level <- factor.list[i]
            #if()
            lines(pred.frame[pred.frame[, 1] == factor.level, 
                2], prediction[pred.frame[, 1] == factor.level], 
                lty = i)
        }
        if (is.null(leg.coords)) {
            #x.max <- max(pred.frame[, 2])
            #x.min <- min(pred.frame[, 2])
            #x.range <- x.max - x.min
            #x.pos <- c(x.min + (0.02 * x.range), x.min + (0.3 * 
            #    x.range))
            #y.max <- max(prediction)
            #y.min <- min(prediction)
            #y.range <- y.max - y.min
            #y.pos <- c(y.min + (0.8 * y.range), y.min + (0.95 * 
            #    y.range))
            legend(locator(1), factor.list, lty = c(1:length(factor.list)),    #x = x.pos, y = y.pos, 
                bty = "n")
        }
        else {
            legend(x = leg.coords[1], y = leg.coords[2], factor.list,     #  
                lty = c(1:length(factor.list)), bty = "n")
        }
    }
}

#############################################################




#####################################################
#####################################################
###### READ IN DATA FOR NASA PROJECT

## NOTE: takes a long time- especially for the df_rel dataset (relative risk from climate change). 
#         So best way to do this is probably to save the R workspace or 
#         save the data frame so you can just re-load the data frame
#         rather than re-loading from the CSV files each time.
#    The only time when you should need to re-read the CSV files is 
#    when you change the set of predictors or modify the predictors.. 


temp1 <- read.csv("nocc_mp_res.csv",header=T)      #  "nasa_lg.csv"  "nocc_results.csv"
temp2 <- read.csv("lev_mp_res.csv",header=T)
temp3 <- read.csv("wre_mp_res.csv",header=T)

dfmain <- rbind(temp1,temp2,temp3)
names(dfmain)  

       ########## Generate two versions: one without examples where n.populations goes to zero
           # before the end of the burn-in, and one with the full data set...

dfmain_full <- dfmain
if(length(which(dfmain$patch.n.10==0))>0)  dfmain <- dfmain[-which(dfmain$patch.n.10==0),] 


#################################################################
#################################################################
      ############  Convert variables and compute new variables  


#########################
####  RESPONSE VARIABLES

dfmain$metapop.logfracinit <- log((dfmain$metapop.abundance.111+0.01)/(dfmain$metapop.abundance.10+0.01))

dfmain$EMA_fracinit <- dfmain$exp.min.n/(dfmain$metapop.abundance.10+0.01)
dfmain$EMA_fracinit_log <- log(dfmain$EMA_fracinit+0.001)

dfmain$logit.pdecline1000 <- ifelse(dfmain$prob.1000<0.0001,0.0001,dfmain$prob.1000)
dfmain$logit.pdecline1000 <- ifelse((1-dfmain$logit.pdecline1000)<0.0001,0.9999,dfmain$logit.pdecline1000)
dfmain$logit.pdecline1000 <- qlogis(dfmain$logit.pdecline1000)

dfmain$logit.pdecline250 <- ifelse(dfmain$prob.250<0.0001,0.0001,dfmain$prob.250)
dfmain$logit.pdecline250 <- ifelse((1-dfmain$logit.pdecline250)<0.000001,0.9999,dfmain$logit.pdecline250)
dfmain$logit.pdecline250 <- qlogis(dfmain$logit.pdecline250)

dfmain$logit.pdecline50 <- ifelse(dfmain$prob.50<0.000001,0.000001,dfmain$prob.50)
dfmain$logit.pdecline50 <- ifelse((1-dfmain$logit.pdecline50)<0.000001,0.9999,dfmain$logit.pdecline50)
dfmain$logit.pdecline50 <- qlogis(dfmain$logit.pdecline50)

dfmain$fracchange <- dfmain$metapop.abundance.111/(dfmain$metapop.abundance.10+0.01)

dfmain$logit.extrisk <- ifelse(dfmain$ext.risk<0.000001,0.000001,dfmain$ext.risk)
dfmain$logit.extrisk <- ifelse((1-dfmain$logit.extrisk)<0.000001,0.99999,dfmain$logit.extrisk)
dfmain$logit.extrisk <- qlogis(dfmain$logit.extrisk)

dfmain$log.n.mean <- log(dfmain$n.mean+0.01)
dfmain$log.quant.50 <- log(dfmain$quant.50+0.01)
dfmain$log.ema <- log(dfmain$exp.min.n+1)

       ### fraction increase or decrease
dfmain$fracdecrease <- ifelse(dfmain$fracchange<1,dfmain$fracchange,1)
 #dfmain$fracincrease <- ifelse(dfmain$metapop.chng<1,1,dfmain$metapop.chng)
 

#########################
####  PREDICTOR VARIABLES
   
        ### largest patch as a percentage of total patch area
dfmain$LargestPatchFrac <- dfmain$lg.patch.area.10 / dfmain$tot.patch.area.10
dfmain$LargestPatchFrac.21 <- dfmain$lg.patch.area.21 / dfmain$tot.patch.area.21 #  NaN if no populations left by this time...

    
dfmain$log.tot.patch.area.10 <- log(dfmain$tot.patch.area.10+0.01)
dfmain$log.tot.patch.area.21 <- log(dfmain$tot.patch.area.21+0.01)

         ## distance to nearest neighbor as a fraction of dispersal distance b (not using for now...)
#dfmain$avg.dist.near.neighbor.10[which(dfmain$patch.n.10==0)] <- 999
#dfmain$avg.dist.near.neighbor.21[which(dfmain$patch.n.21==0)] <- 999
#dfmain$avg.dist.near.neighbor.10[which(dfmain$avg.dist.near.neighbor.10==9999)] <- 999
#dfmain$avg.dist.near.neighbor.21[which(dfmain$avg.dist.near.neighbor.21==9999)] <- 999
#dfmain$IsolationIndex <- log(dfmain$avg.dist.near.neighbor.10/dfmain$avg.disp.dist.b)

dfmain$log.patch.n.10 <- log(dfmain$patch.n.10+0.01)

dfmain$log.metapop.initab <- log(dfmain$metapop.abundance.10)  # was metpop.initab

  #dfmain$log.N.CV.10 <- log(dfmain$N.CV.10)   # Not using for now...

  #dfmain$log.mean.t0.disp.rate <- log(dfmain$mean.t0.disp.rate+0.0001)

  #dfmain$log.cv.avg <- log(dfmain$cv.avg)

#####################
##########    NEW VARIABLES: 29 SEP 2012

       # remove NAs from fractal dimension 
#dfmain$overall.frac.dim.10[which(dfmain$patch.n.10==0)] <- 0
#dfmain$overall.frac.dim.21[which(dfmain$patch.n.21==0)] <- 0

#dfmain$LargestPatchFrac[which(dfmain$patch.n.10==0)] <- 1
#dfmain$LargestPatchFrac.21[which(dfmain$patch.n.21==0)] <- 1

      # remove NAs from fragmentation index
#dfmain$frag.index.hra.10[which(dfmain$patch.n.10==0)] <- 1
#dfmain$frag.index.hra.21[which(dfmain$patch.n.21==0)] <- 1

       # new correlation index
#dfmain$avg.dist.occupied.pops.10[which(dfmain$avg.dist.occupied.pops.10==9999)] <- 5  # KTS: assigned to 5 km, the smallest of the "largest patch area" numbers 
#dfmain$avg.dist.occupied.pops.10[which(dfmain$patch.n.10==0)] <- 1
#dfmain$avg.dist.occupied.pops.21[which(dfmain$patch.n.21==0)] <- 1
dfmain$NewCorrelationIndex <- dfmain$avg.corr.dist.b/dfmain$avg.dist.occupied.pops.10  # lower numbers signify metpopulations with lower overall correlation of environmental variability
dfmain$NewCorrelationIndex[which(dfmain$patch.n.10==1)] <- sqrt(dfmain$tot.patch.area.10[which(dfmain$patch.n.10==1)])
dfmain$NewCorrelationIndex <- log(dfmain$NewCorrelationIndex)

      # New isolation  metric: dispersal rate at maximum distance to nearest neighbor: low values indicate low connectivity
#dfmain$avg.dist.near.neighbor.10[which(dfmain$avg.dist.near.neighbor.10==9999)] <- 999  
dfmain$ConnectivityIndex <- ifelse(dfmain$avg.dist.near.neighbor.10<dfmain$max.disp.dist.Dmax,(dfmain$disp.dist.func.a * exp((-1*dfmain$avg.dist.near.neighbor.10)/dfmain$avg.disp.dist.b)),0)    
dfmain$ConnectivityIndex[which(dfmain$patch.n.10==1)] <- 0

dfmain$ConnectivityIndex21 <- ifelse(dfmain$avg.dist.near.neighbor.21<dfmain$max.disp.dist.Dmax,(dfmain$disp.dist.func.a * exp((-1*dfmain$avg.dist.near.neighbor.21)/dfmain$avg.disp.dist.b)),0)    
dfmain$ConnectivityIndex21[which(dfmain$patch.n.21==1)] <- 0


      # NOTE: this index looked strange using maximum distance - most populations were deemed not connected -- so I chose to use average nearest neighbor distance

      # Resit's fragmentation index (percent of population in small and isolated populations)
dfmain$FragIndex <- dfmain$frag.index.hra.10
dfmain$FragIndex21 <- dfmain$frag.index.hra.21
dfmain$FragIndex[which(dfmain$patch.n.10==1)] <- ifelse(dfmain$metapop.abundance.10[which(dfmain$patch.n.10==1)]>100,1,0)
dfmain$FragIndex21[which(dfmain$patch.n.21==1)] <- ifelse(dfmain$metapop.abundance.21[which(dfmain$patch.n.21==1)]>100,1,0)

      # distance above which only 1% is expected to disperse (dispersability...)
dfmain$Dispersability <- -1*dfmain$avg.disp.dist.b * log(0.01/dfmain$disp.dist.func.a)

     ##############
      # Add “niche breadth” metrics: variability in key climate variables (mean annual temp and mean annual precip?) among occupied sites at start of simulation  
temp <- read.csv(file="Niche_Breadth.csv",header=T)
temp1 <- temp$Species.Code
temp2 <- temp$BIO.1.RANGE
temp3 <- temp$BIO.12.RANGE

dfmain$NicheBreadthT <- 0
dfmain$NicheBreadthP <- 0
for(i in 1:length(temp1)){
  dfmain$NicheBreadthT[which(dfmain$spec==temp1[i])] <- temp2[i] 
  dfmain$NicheBreadthP[which(dfmain$spec==temp1[i])] <- temp3[i]
}

    #############
     # Change variables... recent change (10 year period...)

         # recent change in occupied area in last 10 years (AOO trend, proportional)
dfmain$RecentAreaChange <- (dfmain$tot.patch.area.21-dfmain$tot.patch.area.10)/dfmain$tot.patch.area.10   #log((dfmain$tot.patch.area.21/dfmain$tot.patch.area.10)+0.01)     # dfmain$tot.patch.area.10 - dfmain$tot.patch.area.1  

         # recent change in total abundance (proportional)
dfmain$RecentNChange <- (dfmain$metapop.abundance.21-dfmain$metapop.abundance.10)/dfmain$metapop.abundance.10

         # recent change in number of subpopulations (proportional)
dfmain$RecentSubpopChange <- (dfmain$patch.n.21 - dfmain$patch.n.10)/ dfmain$patch.n.10  

         # recent change in overall fractal dimension
dfmain$RecentFracDimChange <- dfmain$overall.frac.dim.21 - dfmain$overall.frac.dim.10 
dfmain$RecentFracDimChange[which(dfmain$patch.n.21==0)] <- 0    # set to zero change if population goes extinct.

         # recent change in fragmentation index
                 ### SET TO NO CHANGE WHEN NUMBER OF POPULATIONS IS EQUAL TO ZERO
dfmain$RecentFragmentation <- dfmain$FragIndex21 - dfmain$FragIndex 
dfmain$RecentFragmentation[which(dfmain$patch.n.21==0)] <- 0   # set to zero change if mp goes extinct

         # recent change in isolation index  ## change to connectivity index
#dfmain$RecentIsolation <- log(dfmain$avg.dist.near.neighbor.21/dfmain$avg.disp.dist.b) - dfmain$IsolationIndex

dfmain$RecentConnectivity <- dfmain$ConnectivityIndex21 - dfmain$ConnectivityIndex   # REDO
dfmain$RecentConnectivity[which(dfmain$patch.n.21==0)]  <- 0      # set to zero change if population goes extinct.

         # recent change in the largest patch fraction
dfmain$RecentLPFChange <- dfmain$LargestPatchFrac.21 - dfmain$LargestPatchFrac
dfmain$RecentLPFChange[which(dfmain$patch.n.21==0)]  <- 0      # set to zero change if population goes extinct.
 

####################More conversions to create new variables
              #BINARY RESPONSE VARIABLES...

       # general decline
dfmain$decline <- ifelse(dfmain$quant.50<(dfmain$metapop.initab*0.75),1,0)


      # greater than 50% chance of extinction
dfmain$extinct5 <- ifelse(dfmain$ext.risk>0.5,1,0)


        # reorder climate change factor levels...

dfmain$clim.chg <- factor(dfmain$clim.chg,levels=c("NoCC","LEV","WRE"))


###################################################
 ######            DEFINE RESPONSE AND PREDICTOR VARIABLES

###################################################
###CRITICAL####
###SELECTION OF VARIABLES FOR RUNNING ANALYSES#####
###############
predictors <- c("log.metapop.initab","Gen.Tgen","GrowthRt","cv.avg","NewCorrelationIndex","log.patch.n.10","log.tot.patch.area.10","overall.frac.dim.10","LargestPatchFrac","ConnectivityIndex","FragIndex","Dispersability","NicheBreadthT","NicheBreadthP","RecentAreaChange","RecentNChange","RecentSubpopChange","RecentFracDimChange","RecentConnectivity","RecentFragmentation","RecentLPFChange")   # "log.mean.t0.disp.rate",  
predictorNames <- c("Initial Abundance","Generation Length","Maximum Growth Rate","Variability in Vital Rates","Spatial Correlation of Variability","Number of patches","Total Occupied Area","Overall Fractal Dimension","Largest Patch Fraction",
                                  "Connectivity Index","Fragmentation Index","Innate Dispersal Ability","Climate Breadth at t0, temp","Climate Breadth at t0, precip","Recent change in occupied area","Recent change in abundance","Recent change in number of subpops","Recent change in fractal dimension","Recent Change in Connectivity","Recent fragmentation", "Recent Change in Patch Dominance")  # "Mean Dispersal Rate",
cbind(predictors,predictorNames)
###############

predictor_cols <- which(names(dfmain)%in%predictors)

ema_responses <- c("exp.min.n","log.ema","EMA_fracinit_log","metapop.chng","metapop.logfracinit","log.n.mean","decline")
ema_cols <- which(names(dfmain)%in%ema_responses)

risk_responses <- c("ext.risk","prob.1000","logit.extrisk","logit.pdecline1000","logit.pdecline250","logit.pdecline50","extinct5")
riskresponse_cols <- which(names(dfmain)%in%risk_responses)

responses <- c(ema_responses,risk_responses)
response_cols <- which(names(dfmain)%in%responses)

  #responseNames<-c("Expected Minimum Abundance", "EMA (log scale)", "EMA", etc. etc.)

other_columns <- c("clim.chg","spec","model")
other_cols <- which(names(dfmain)%in%other_columns)

allcols <- c(predictor_cols,riskresponse_cols,ema_cols,other_cols)


#####################################################
#####################################################
     #  remove all rows with NA or NaNs     # NOTE: NaNs in "avg.dist.near.neighbor.10"

   # ADD CHECK FOR 9999s.  

temp <- array(0,dim=c(ncol(dfmain),4))
for (i in predictor_cols){    # loop through the variables of interest and see if any NAs or Infs etc..
  if (sum(is.nan(dfmain[,i]))>0){
    temp[i,1] = temp[i,1] + sum(is.nan(dfmain[,i]))
    #dfmain <- dfmain[-which(is.nan(dfmain[,i])),]
  }
  if( sum(is.na(dfmain[,i]))>0){
    temp[i,2] = temp[i,2] + sum(is.na(dfmain[,i]))
    #dfmain <- dfmain[-which(is.na(dfmain[,i])),]
  }
  if( sum(is.infinite(dfmain[,i]))>0){
    temp[i,3] = temp[i,3] + sum(is.infinite(dfmain[,i]))
    #dfmain <- dfmain[-which(is.infinite(dfmain[,i])),]
  }
  if( sum(dfmain[,i]==9999,na.rm=T)>0){
    temp[i,4] = temp[i,4] + sum(is.infinite(dfmain[,i]))
    #dfmain <- dfmain[-which(is.infinite(dfmain[,i])),]
  }
}
temp

                              # identify where the NAs and NANs are
#which(apply(temp,1,sum)>0)
#head(dfmain[,which(apply(temp,1,sum)>0)])
#names(dfmain[,which(apply(temp,1,sum)>0)])
 
names(dfmain)


#################################
##########  MANUALLY REMOVE SOME SIMULATION RUNS (only one match and mismatch per species)

#  (from Resit) remove the following:

# BGTU - Snake
# BMTU - Small_Salamander
# BMTU - Snake
# BMTU - Tortoise
# GOTO - Large_Salamander
# OBTU - Small_Salamander

ss <- list()
ss[[1]] <- which(dfmain$spec=="BGTU"&dfmain$model=="Snake")
ss[[2]] <- which(dfmain$spec=="BMTU"&dfmain$model=="Small_Salamander")
ss[[3]] <- which(dfmain$spec=="BMTU"&dfmain$model=="Snake")
ss[[4]] <- which(dfmain$spec=="BMTU"&dfmain$model=="Tortoise")
ss[[5]] <- which(dfmain$spec=="GOTO"&dfmain$model=="Large_Salamander")
ss[[6]] <- which(dfmain$spec=="OBTU"&dfmain$model=="Small_Salamander")
ssndx <- c(ss[[1]],ss[[2]],ss[[3]],ss[[4]],ss[[5]],ss[[6]])
dfmain <- dfmain[-ssndx,]



#################################
########################
#######   CORRELATION ANALYSES: POTENTIAL PREDICTOR VARIABLES

############################################
### correlation of generation time variables: exploration to help in selection of variables
gentimevars <-  c("Gen.Abar","Gen.Mu1","Gen.Tgen")        ##names(dfmain)[c(8:10)]    
formula = paste("~",paste(gentimevars,collapse="+"),sep="")
pairs(eval(parse(text=formula)),data=dfmain, 
   main="Scatterplot Matrix of gen time Variables",pch=20,cex=.2)

round(cor(dfmain[,which(names(dfmain)%in%gentimevars)]),2)

### correlation of growth rate variables
growthratevars <- c("EigenVal","Rmax","GrowthRt")   # names(dfmain)[c(11,13,14)]    
formula = paste("~",paste(growthratevars,collapse="+"),sep="")
pairs(eval(parse(text=formula)),data=dfmain, 
   main="Scatterplot Matrix of growth rate Variables",pch=20,cex=.2)

round(cor(dfmain[,which(names(dfmain)%in%growthratevars)]),2)

### correlation of variability variables
varvars <- c("cv.avg","N.maxVmin.10","N.CV.10")   #names(dfmain)[c(20,21,23)]    
formula = paste("~",paste(varvars,collapse="+"),sep="")
pairs(eval(parse(text=formula)),data=dfmain, 
   main="Scatterplot Matrix of variability Variables",pch=20,cex=.2)

round(cor(dfmain[,which(names(dfmain)%in%varvars)]),2)

### correlation of dispersal variables
dispersalvars <- c("Dispersability","ConnectivityIndex","FragIndex","overall.frac.dim.10","log.patch.n.10") # names(dfmain)[c(52,175,167,173)]  "avg.disp.dist.b", "IsolationIndex",   
formula = paste("~",paste(dispersalvars,collapse="+"),sep="")
pairs(eval(parse(text=formula)),data=dfmain, 
   main="Scatterplot Matrix of dispersal Variables",pch=20,cex=.2)

round(cor(dfmain[,which(names(dfmain)%in%dispersalvars)]),2)


### correlation of fragmentation variables
fragvars <- c("FragIndex","ConnectivityIndex","overall.frac.dim.10","log.tot.patch.area.10","log.metapop.initab")   # names(dfmain)[c(174, 173, 104, 165)]      
formula = paste("~",paste(fragvars,collapse="+"),sep="")
pairs(eval(parse(text=formula)),data=dfmain, 
   main="Scatterplot Matrix of fragmentation Variables",pch=20,cex=.2)

round(cor(dfmain[,which(names(dfmain)%in%fragvars)]),2)


### correlation of area and niche breadth variables
areavars <- c("log.patch.n.10","NicheBreadthT","NicheBreadthP","log.tot.patch.area.10")         # names(dfmain)[c(176,177,165)]      
formula = paste("~",paste(areavars,collapse="+"),sep="")
pairs(eval(parse(text=formula)),data=dfmain, 
   main="Scatterplot Matrix of area and breadth variables",pch=20,cex=.2)

round(cor(dfmain[,which(names(dfmain)%in%areavars)]),2)


### correlation of recent change variables
changevars <- c("RecentAreaChange", "RecentNChange","RecentSubpopChange","RecentFracDimChange","RecentConnectivity","RecentFragmentation","RecentLPFChange")     # names(dfmain)[c(178:183)]      
formula = paste("~",paste(changevars,collapse="+"),sep="")
pairs(eval(parse(text=formula)),data=dfmain, 
   main="Scatterplot Matrix of recent change variables",pch=20,cex=.2)

round(cor(dfmain[,which(names(dfmain)%in%changevars)]),2)


###################################
############      PCA and factor analysis  (KTS: greyed out for now)

####   Run Principal Component Analysis and factor analysis on spatial/ fragmentation/isolation 
  #        variables to look for natural groupings??

				# retrieve spatial variables once per "species" (spatial configuration...)????
#configvars <- fragvars   
#formula <- paste("~",paste(configvars,collapse="+"),sep="")
#temp <- princomp(eval(parse(text=formula)), data=dfmain, cor=T)
#plot(temp$scores[,1]~temp$scores[,2])


#temp2 <- factanal(eval(parse(text=formula)), data=dfmain, scores = "Bartlett", factors=3,rotation = "varimax",)
#plot(temp2$scores[,1]~temp2$scores[,3])



###########################################################
###########################################################
###########################################################
##################################    RELATIVE RISK     #  now altered to deal with match/mismatch...


###########################
#                      NEW DATA FRAMES: df_lev, df_wre

cchange <- dfmain$clim.chg
mpfilefull <- as.character(dfmain$mp.run)
matchfull <- as.character(dfmain$match.stat)
matchnew <- rep(unique(matchfull),times=length(unique(dfmain$mp.run)))  
mpfile <- rep(as.character(unique(dfmain$mp.run)),each=2)   # account for mismatches...
nfiles <- length(mpfile)
varnames <- names(dfmain)
nvars <- length(allcols)

df_lev <- data.frame(ndx=c(1:nfiles))
df_wre <- data.frame(ndx=c(1:nfiles))
for (i in 1:nvars){
  tempvecname1 <- paste("levvar",i,sep="")
  tempvecname2 <- paste("wrevar",i,sep="")
  temp1 <- numeric(nfiles)
  temp2 <- numeric(nfiles)
  if(allcols[i]%in%other_cols){
    temp1 <- character(nfiles)
    temp2 <- character(nfiles)
  }
  counter <- 1
  for (j in 1:(nfiles)){
    levndx <- which((mpfilefull==mpfile[j])&cchange=="LEV"&matchfull==matchnew[j])
    wrendx <- which((mpfilefull==mpfile[j])&cchange=="WRE"&matchfull==matchnew[j])
    noccndx <- which((mpfilefull==mpfile[j])&cchange=="NoCC"&matchfull==matchnew[j])
    if(allcols[i]%in%riskresponse_cols) temp1[j] <- ifelse(length(levndx)>0,(dfmain[levndx,allcols[i]]-dfmain[noccndx,allcols[i]]),NA)  
    if(allcols[i]%in%riskresponse_cols) temp2[j] <- ifelse(length(wrendx)>0,(dfmain[wrendx,allcols[i]]-dfmain[noccndx,allcols[i]]),NA)
    if(allcols[i]%in%predictor_cols) temp1[j] <- ifelse(length(levndx)>0,dfmain[levndx,allcols[i]],NA)
    if(allcols[i]%in%predictor_cols) temp2[j] <- ifelse(length(wrendx)>0,dfmain[wrendx,allcols[i]],NA)
    if(allcols[i]%in%ema_cols) temp1[j] <- ifelse(length(levndx)>0,(dfmain[levndx,allcols[i]]-dfmain[noccndx,allcols[i]]),NA)  
    if(allcols[i]%in%ema_cols) temp2[j] <- ifelse(length(wrendx)>0,(dfmain[wrendx,allcols[i]]-dfmain[noccndx,allcols[i]]),NA)
    if(allcols[i]%in%other_cols) temp1[j] <- ifelse(length(levndx)>0,as.character(dfmain[levndx,allcols[i]]),NA)  
    if(allcols[i]%in%other_cols) temp2[j] <- ifelse(length(wrendx)>0,as.character(dfmain[wrendx,allcols[i]]),NA)
  }
  tempcolname <- varnames[allcols[i]]
  assign(tempvecname1,temp1)
  assign(tempvecname2,temp2)
  
  eval(parse(text=paste("df_lev$",tempcolname,"<-",tempvecname1,sep="")))
  eval(parse(text=paste("df_wre$",tempcolname,"<-",tempvecname2,sep="")))  
}

df_lev$match <- matchnew
df_lev$mpfile <- mpfile
df_wre$match <- matchnew
df_wre$mpfile <- mpfile

     ##############  bind the two data frames together

names(df_lev)

df_rel <- rbind(df_lev,df_wre)
df_rel$clim.chg <- factor(c(rep("LEV",times=nrow(df_lev)),
               rep("WRE",times=nrow(df_wre))),levels=c("LEV","WRE")) 
names(df_rel)

df_rel$spec <- as.factor(df_rel$spec)
df_rel$model <- as.factor(df_rel$model)

 #df_rel <- df_rel[-which(is.na(df_rel$metapop.chng)),]     # remove NAs

 #hist(df_lev$logit.pdecline1000)
 #hist(df_lev$logit.extrisk)

     #  remove all rows with NA or NaNs     # NOTE: NaNs in "avg.dist.near.neighbor.10"

temp <- array(0,dim=c(ncol(df_rel),4))
for (i in 1:ncol(df_rel)){
  if (sum(is.nan(df_rel[,i]))>0){
    temp[i,1] = temp[i,1] + sum(is.nan(df_rel[,i]))
    #df_rel <- df_rel[-which(is.nan(df_rel[,i])),]
  }
  if( sum(is.na(df_rel[,i]))>0){
    temp[i,2] = temp[i,2] + sum(is.na(df_rel[,i]))
    df_rel <- df_rel[-which(is.na(df_rel[,i])),]
  }
  if( sum(is.infinite(df_rel[,i]))>0){
    temp[i,3] = temp[i,3] + sum(is.infinite(df_rel[,i]))
    #df_rel <- df_rel[-which(is.infinite(df_rel[,i])),]
  }
  if( sum(df_rel[,i]==9999,na.rm=T)>0){
    temp[i,4] = temp[i,4] + sum(is.infinite(df_rel[,i]))
    #df_rel <- dfmain[-which(df_rel[,i]==9999,na.rm=T)>0,]
  }
}

                              # identify where the NAs and NANs are
which(apply(temp,1,sum)>0)
head(df_rel[,which(apply(temp,1,sum)>0)])
names(df_rel[,which(apply(temp,1,sum)>0)])
nrow(df_rel)


################################
################################
######START OF ANALYSES#########
################################

############################
### EXPLORE RESPONSE VARIABLES...

   # explore response variables (Main)...

 #ndx=1
firsthalf = c(1:8)
secondhalf = c(9:length(response_cols))

graphics.off()
par(mfrow=c(3,3))
for(i in firsthalf){
  #ndx=i
  hist(dfmain[,response_cols[i]],main="",xlab=names(dfmain)[response_cols[i]],,freq=F)  #  responseNames[which(responses==names(dfmain)[response_cols[i]])]   
}

graphics.off()
par(mfrow=c(3,3))
for(i in secondhalf){
  #ndx=i
  hist(dfmain[,response_cols[i]],main="",xlab=names(dfmain)[response_cols[i]],freq=F)   
}

graphics.off()
hist(dfmain$ext.risk,xlab="Extinction Risk",freq=F)

table(dfmain$extinct5)

##version of above that prints to a file (high quality); can use the tiff function with dev.off for any plots##

#tiff("figureTEMP.tif", compression = "lzw",width = 2880, height = 2000,
#     units = "px", pointsize = 30,
#     bg = "white", #res = NA,
#     restoreConsole = TRUE)

#par(mfrow=c(3,4))
 #ndx=1
#for(i in 1:length(response_cols)){
#  ndx=i
#  hist(dfmain[,response_cols[ndx]],main="",xlab=names(dfmain)[response_cols[ndx]],freq=F, cex=1.2)   # logit EMA as frac init.
#}

#dev.off()


#################################
   ### Response variables (Relative risk due to climate change)

 #ndx=1
firsthalf = c(1:8)
secondhalf = c(9:length(response_cols))

graphics.off()
par(mfrow=c(3,3))
for(i in firsthalf){
  #ndx=i
  hist(df_rel[,i+length(predictors)],main="",xlab=names(df_rel)[i+length(predictors)],freq=F)   
}

graphics.off()
par(mfrow=c(3,3))
for(i in secondhalf){
  #ndx=i
  hist(df_rel[,i+length(predictors)],main="",xlab=names(df_rel)[i+length(predictors)],freq=F)   
}

##########################################
#
   # explore predictor variables (Main)...

graphics.off()
par(mfrow=c(3,3))
 #ndx=1
firsthalf = c(1:7)
secondhalf = c(8:14)
thirdhalf =  c(15:length(predictor_cols))

 #ndx=1
for(i in firsthalf){
  #ndx=i
  hist(dfmain[,predictor_cols[i]],main="",xlab=predictorNames[which(predictors==names(dfmain)[predictor_cols[i]])],freq=F)   # logit EMA as frac init.
}

graphics.off()
par(mfrow=c(3,3))
for(i in secondhalf){
  #ndx=i
  hist(dfmain[,predictor_cols[i]],main="",xlab=predictorNames[which(predictors==names(dfmain)[predictor_cols[i]])],freq=F)   # logit EMA as frac init.
}

graphics.off()
par(mfrow=c(3,3))
for(i in thirdhalf){
  #ndx=i
  hist(dfmain[,predictor_cols[i]],main="",xlab=predictorNames[which(predictors==names(dfmain)[predictor_cols[i]])],freq=F)   # logit EMA as frac init.
}


   ### Relative risk due to climate change
##just a check, should be identical to above###
#par(mfrow=c(3,4))
#for(i in 1:length(predictor_cols)){
#  ndx=i
#  hist(df_rel[,i+1],main="",xlab=names(df_rel)[i+1],freq=F)   # logit EMA as frac init.
#}

graphics.off()

############################
##### Question: for the relative risk scenarios, how would we compute recent change in occupied area, since that
            #  changes under each of the climate scenarios??   Currently, we would use only the recent change under
            #  no climate change... 

########################################################
#                               DESCRIPTIVE PLOTS... ABSOLUTE METRICS

                   # functions for presenting different types of data

thousands <- function(x){    # for EMA
   a <- round(x/1000,0)
   a <- paste(a,"k",sep="")
   return(a)
}

fractions <- function(x){   # for percent extinction etc.  ...
  a <- round(x*100,1)
  a <- paste(a,"%",sep="")
  return(a)
}

dec2 <- function(x){   # for percent extinction etc.  ...
  a <- round(x,2)
  a <- paste(a,sep="")
  return(a)
}

#  make separate data frames for species groups- this will hopefully
        # help in presenting some descriptive statistics.
##NEEDS TO AGREE WITH FINAL SET OF SPECIES; must include all species##

turtles1 <- c("ARTU","BGTU","BMTU","MATU")       
turtles2 <- c("BLTU","OBTU","WOTU")
tortoises <- c("ADTO","GOTO","MDTO")
snakes1 <- c("EFSN","EISN","GGSN")            
#snakes2 <- c()
snakes2 <- c("LPSN","CMSN","KTSN","MSSN")
snakes3 <- c("PNSN","RASN","STSN","SHSN")
salamanders1 <- c("CTSA","DNSA","JPSA")     
salamanders2 <- c("OSSA","CMSA","FFSA","KPSA")
salamanders3 <- c("RFSA","RMSA","SESA","SMSA")
salamanders4 <- c("WSSA","WESA")
largesalamanders1 <- c("HELL")
lizards <- c("BLLZ")

temp <- sort(c(turtles1, turtles2, tortoises,
  snakes1, snakes2, snakes3,  salamanders1, 
  salamanders2, salamanders3, 
  salamanders4, largesalamanders1,lizards))

allSpecies <- temp

temp2 <- as.character(sort(unique(dfmain$spec))) 
cbind(temp, temp2)
temp%in%temp2   # just a check to make sure all species are included
temp2%in%temp


#######################################################
                 ###########################  METHOD 1: univariate plots with raw data               

#  possible species groups: turtles1, turtles2, snakes1, snakes2, snakes3, salamanders1 to 4, tortoises, lizards
    # note: when species specific only works for demographic variables: not spatial variables...

response <- "ext.risk"  #  "metapop.logfracinit"  #  "log.ema"  #    "logit.extrisk"  # 
predictor <- "log.metapop.initab"  #  "Gen.Tgen"  #   "GrowthRt"   #  "log.N.CV.10"  #   "cv.avg"   #  "NewCorrelationIndex"  #  "log.patch.n.10"  #  "log.tot.patch.area.10"  #  "overall.frac.dim.10"  #  "LargestPatchFrac"  #  "ConnectivityIndex" # "LogFragIndex" # "Dispersability"  #  "NicheBreadthT" # "NicheBreadthP"  #  "RecentAreaChange"  # "RecentNChange"  #  "RecentSubpopChange"  #  "RecentFracDimChange" #  "RecentFragmentation"
sp.group <- "salamanders1"  #  "largesalamanders1"  #  "turtles1"  #  "snakes3"  #     
sp.name <- "Salamander group 1" #  "Hellbender"  #  "Turtles group 1"  #  #  "Snakes group 3"  "Tortoises"  # "lizards"  #    
xax <- "Initial Abundance (log transformed)"  #  "Lambda/ Rmax"  #  "initial occupied area (log scale)"  #  
yax <- "Extinction risk"  #   "Fraction remaining (log scale)"  #  "Expected minimum abundance"  # 
fmt <- "fractions"  #  "dec2"  #  "thousands"  #  #format of y axis
fmtx <- "dec2"   #format of x axis
dframe <- "dfmain"  #  dfmain_full  #   "df_rel" #     dfmain or df_rel for absolute or relative metrics
 
eval(parse(text=paste("subset1 <- subset(",dframe,",spec%in%",sp.group,"&match.stat==\"Match\")",sep="") )) 

p <- ggplot(subset1, aes(eval(parse(text=predictor)),eval(parse(text=response)))) +  
	geom_point(aes(colour=clim.chg,shape=clim.chg)) + 
	facet_grid(spec ~ clim.chg,scales="free") +
      #opts(legend.position = "none") +
      #stat_smooth(aes(colour=clim.chg), method="lm", size = 1.3, se=FALSE ) +   # 
      scale_colour_manual("Climate scenario",values=rev(brewer.pal(3,"Set1"))) +    # start=0.6, end=0.05   values = c("green","blue", "red")
      scale_shape("Climate scenario") +
      scale_x_continuous(xax, labels=eval(parse(text=fmtx))) + scale_y_continuous(yax, labels=eval(parse(text=fmt))) +  #    # 
      opts(title=paste(sp.name,sep=" "))
p 

           ###############################
                    ########### alternative...  climate scenarios overlaid 

#   response variables: ext.risk, exp.min.n, prob.1000, metapop.chng, fracchange

#  relevant predictor variables: GrowthRt, cv.avg, log.N.CV.10,avg.disp.dist.b,
    #          max.disp.dist.Dmax, log.mean.t0.disp.rate, avg.corr.dist.b 

#  possible species groups: turtles, snakes1, snakes2, salamanders


response <- "ext.risk"  #  "metapop.logfracinit"   #   "prob.1000" #   "exp.min.n"  #   "prob.1000" #      
predictor <- "ConnectivityIndex" #  "log.metapop.initab"  #  "Gen.Tgen"  #   "GrowthRt"   #  "log.N.CV.10"  #   "cv.avg"   #  "NewCorrelationIndex"  #  "log.patch.n.10"  #  "log.tot.patch.area.10"  #  "overall.frac.dim.10"  #  "LargestPatchFrac"  #  "LogFragIndex" # "Dispersability"  #  "NicheBreadthT" # "NicheBreadthP"  #  "RecentAreaChange"  # "RecentNChange"  #  "RecentSubpopChange"  #  "RecentFracDimChange" #  "RecentFragmentation"
sp.group <- "allSpecies"   #  "salamanders3" #   "tortoises"  #  "turtles2  #  "snakes1"   #  "lizards"  #  "snakes1"  #  "snakes2"  #  
sp.name <- "All Species"   #  "Salamanders, group 3"  #  "Tortoises"    #  "Turtles, group 2"  #  "Snakes, group 1"    #  "Lizards"  #  
xax <- "Connectivity Index"  #  "Initial Abundance (log)" # "Initial occupied area (log scale)"  #   "Generation time (years)"  #   "Fractal Dimension"  #  "Isolation Index"  #  "Number of patches"  #  "Growth rate (lambda/Rmax)"   #  "Largest population as frac. of total population, t0"  #   "Recent change in occupied area (log scale)"  #  "Current nearest neighbor distance"  #    "Mean environmental variability, CV"  # 
yax <- "Extinction risk"    #  "Fraction remaining at final time step (log scale)"    #   "Fraction decline, threshold=1000"   # "Expected minimum abundance"   #  
fmt <- "dec2"  #  "fractions"  #   "thousands"  #      
fmtx <- "dec2"
dframe <- "dfmain"  #    "df_rel" #    dfmain or df_rel for absolute or relative metrics
rtype <- "absolute"  #  "relative"  #   
genericModel <- list(FALSE,"Turtle")

if(sp.name=="All Species") eval(parse(text=paste("subset1 <- subset(",dframe,",spec%in%",sp.group,")",sep="") ))
if(sp.name!="All Species") eval(parse(text=paste("subset1 <- subset(",dframe,",spec%in%",sp.group,"&match.stat==\"Match\")",sep="") ))
if(genericModel[[1]])      eval(parse(text=paste("subset1 <- subset(",dframe,",model==genericModel[[2]])",sep="") )) 

facet_var <- ifelse(sp.name!="All Species"&!genericModel[[1]],"spec~.","clim.chg~.") 
title1 <- ifelse(genericModel[[1]],paste("Generic model: ", genericModel[[2]],sep=""),paste(sp.name,"",sep=""))  

p <- ggplot(subset1, aes(eval(parse(text=predictor)),eval(parse(text=response)))) +   
      geom_point(aes(colour=clim.chg,shape=clim.chg)) +
	facet_grid(eval(parse(text=facet_var)),scales="free") +   # 
        #if(sp.name == "All Species") facet_grid(clim.chg~.,scales="free_y") +
      scale_colour_manual("Climate scenario",values=rev(brewer.pal(3,"Set1"))) +  #+  # ,start=0.8,end=0.05
      #stat_smooth(aes(colour=clim.chg), method="loess", size = 1.01, se=FALSE) +
      scale_shape("Climate scenario") +
      scale_x_continuous(xax,labels=eval(parse(text=fmtx))) + 
      scale_y_continuous(paste(yax," (",rtype,")",sep=""),labels = eval(parse(text=fmt))) +
      opts(title=title1)
p 


#############################  SUMMARY BAR CHARTS- CLIMATE CHG EFFECTS

#  possible generic models: Snake, Turtle, Small_Salamander, Large_Salamander, Tortoise

#   response variables: ext.risk, exp.min.n, prob.1000, metapop.chng

response <- "ext.risk"  #  "prob.1000"  #  "metapop.chng"  #  "exp.min.n"  #  "metapop.logfracinit"  # "log.ema"  #   
predictor <- "clim.chg"       		
sp.group <- "All species"  #  "Small_Salamander"  #  "Snake"  #  "Lizard"  #  "Tortoise"  #  "Turtle"   #
sp.name <- "All species"  #  "Small Salamanders"  #  "All snakes"  #  
xax <- "Climate Change Scenario"   		
yax <- "Extinction risk"  #   "Risk of decline (threshold=1000)"  #  "Change in abundance"  #    "Expected minimum abundance"  #  
clims <- c("NoCC","LEV","WRE") #    c("LEV","WRE")  #    
fmt <- "fractions"  #  "dec2"  #  "thousands"  #  
dframe <- "dfmain_full"  #  "dfmain"   #  "df_rel"   #      # dfmain or df_rel for absolute or relative metrics
rtype <- "absolute"  #  "relative"  #  

if(sp.group == "All species"){
  subset1 <- dfmain[which(dfmain$match.stat=="Match"),]
} else{
  eval(parse(text=paste("subset1 <- subset(",dframe,",model==\"",sp.group,"\"&dfmain$match.stat==\"Match\")",sep="") ))
}

meanrisk = eval(parse(text=paste("tapply(subset1$",response,",subset1$",predictor,", mean)",sep="")))
serisk = eval(parse(text=paste("tapply(subset1$",response,",subset1$",predictor,", function(t) sd(t)/sqrt(length(t)))",sep="")))
ccnames = eval(parse(text=paste("names(tapply(subset1$",response,",subset1$",predictor,", mean))",sep="")))

df <- data.frame(
  meanrisk=meanrisk,
  serisk=serisk,
  ccnames=factor(ccnames,levels=clims)
)

limits <- aes(ymax = meanrisk + serisk, ymin=meanrisk - serisk)

p <- ggplot(df,aes(ccnames,meanrisk)) + geom_bar(stat="identity",aes(fill=ccnames)) + 
            geom_errorbar(limits,width=0.2) +
            scale_x_discrete(xax) + 
            scale_y_continuous(paste(yax," (",rtype,")",sep=""),labels= eval(parse(text=fmt))) +
            scale_fill_manual("Climate scenario",values=rev(brewer.pal(3,"Set1"))) +
            #facet_grid(eval(parse(text=paste("model ~ ",".",sep=""))) )
            opts(title=paste("Climate risk,",sp.name,sep=" "))
p

             ###################################  TRY A FACETED BARPLOT (works well)
#  possible generic models: Snake, Turtle, Small_Salamander, Large_Salamander, Tortoise

#   response variables: ext.risk, exp.min.n, prob.1000, metapop.chng

#  possible fmts: fractions, thousands

response <- "ext.risk"  #  "exp.min.n" #   "metapop.chng"  #  "prob.1000"  #   
predictor <- "clim.chg"
clims <- c("NoCC","LEV","WRE") #    c("LEV","WRE")  #   		
xax <- "Climate Change Scenario"   		
yax <- "Extinction risk"  #  "Expected minimum abundance"  #  "Change in abundance"  #  "Probability of decline (threshold = 1000)"  #      
fmt <-  "fractions"  # "thousands"  #  "dec2"  #   
sp.groups <- c("Snake","Large_Salamander","Small_Salamander","Turtle","Tortoise","Lizard")  # 
dframe <- "dfmain_full"  #  "dfmain"  #   "df_rel" #  dfmain or df_rel for absolute or relative metrics
rtype <- "absolute" #   "relative" # 

eval(parse(text=paste("subset1 <- ",dframe,"[which(",dframe,"$model%in%sp.groups&",dframe,"$match.stat==\"Match\"),]",sep="")))   ### dfmain[which(dfmain$model%in%sp.groups),]

len <- length(sp.groups)*length(clims)
meanrisk <- numeric(len);serisk <- numeric(len) ;ccnames <- numeric(len);group<-numeric(len)
counter=1
for(i in 1:length(sp.groups)){  # loop through species groups 
  eval(parse(text=paste("subset1 <- subset(",dframe,",model==\"",sp.groups[i],"\"&",dframe,"$match.stat==\"Match\")",sep="") ))
  meanrisk[counter:(counter+(length(clims)-1))] = eval(parse(text=paste("tapply(subset1$",response,",subset1$",predictor,", mean)",sep="")))
  serisk[counter:(counter+(length(clims)-1))] = eval(parse(text=paste("tapply(subset1$",response,",subset1$",predictor,", function(t) sd(t)/sqrt(length(t)))",sep="")))
  ccnames[counter:(counter+(length(clims)-1))] = eval(parse(text=paste("names(tapply(subset1$",response,",subset1$",predictor,", mean))",sep="")))
  group[counter:(counter+(length(clims)-1))] = rep(sp.groups[i],length(clims))
  counter=counter+length(clims)
}
df <- data.frame(
  meanrisk=meanrisk,
  serisk=serisk,
  ccnames=factor(ccnames,levels=clims),
  group=group
)

limits <- aes(ymax = meanrisk + serisk, ymin=meanrisk - serisk)

p <- ggplot(df,aes(ccnames,meanrisk)) + 
            geom_bar(stat="identity",aes(fill=ccnames)) + 
            geom_errorbar(limits,width=.2) +
            scale_x_discrete(xax) + 
            scale_y_continuous(paste(yax," (",rtype,")",sep=""),labels= eval(parse(text=fmt))) +
            scale_fill_manual("Climate scenario",values=rev(brewer.pal(3,"Set1"))) +
            facet_grid(eval(parse(text=paste(". ~ ","group",sep=""))),scales="free" ) +
            opts(title=paste("Climate risk,","by generic species group",sep=" ")) #+
            #coord_flip() 
p



             ###################################  BARPLOT BY SPECIES 
#  possible generic models: Snake, Turtle, Small_Salamander, Large_Salamander, Tortoise

#   response variables: ext.risk, exp.min.n, prob.1000, metapop.chng

#  possible fmts: fractions, thousands

response <- "metapop.chng"  #  "ext.risk"  #  "prob.1000"  # "exp.min.n"
predictor <- "clim.chg"  
clims <- c("NoCC","LEV","WRE") #   c("LEV","WRE")  #        		
xax <- "Species"   		
yax <- "Fraction change in abundance"  # "Extinction risk"  # "Probability of decline (threshold=1000)"  # Expected minimum abundance
fmt <- "fractions"  #  "dec2"  #  "thousands"
sp.groups <- c("Snake","Large_Salamander","Small_Salamander","Turtle","Tortoise","Lizard")
ylimits <- c(-1,15)  #  c(0,1)
dframe <- "dfmain_full"  #   "dfmain"  #  "df_rel"  #     dfmain or df_rel for absolute or relative metrics
rtype <- "absolute"  #  "relative"  #   

eval(parse(text=paste("subset1 <- ",dframe,"[which(",dframe,"$model%in%sp.groups&",dframe,"$match.stat==\"Match\"),]",sep="")))   ### dfmain[which(dfmain$model%in%sp.groups),]

species <- unique(subset1$spec)
nspecies <- length(species)

len <- nspecies*length(clims)
meanrisk <- numeric(len);serisk <- numeric(len) ;ccnames <- numeric(len);speciesname<-character(len)
counter=1
for(i in 1:nspecies){  # loop through species  
  eval(parse(text=paste("subset2 <- subset(subset1,spec==\"",species[i],"\")",sep="") ))
  meanrisk[counter:(counter+(length(clims)-1))] = eval(parse(text=paste("tapply(subset2$",response,",subset2$",predictor,", mean)",sep="")))
  serisk[counter:(counter+(length(clims)-1))] = eval(parse(text=paste("tapply(subset2$",response,",subset2$",predictor,", function(t) sd(t)/sqrt(length(t)))",sep="")))
  ccnames[counter:(counter+(length(clims)-1))] = eval(parse(text=paste("names(tapply(subset2$",response,",subset2$",predictor,", mean))",sep="")))
  speciesname[counter:(counter+(length(clims)-1))] = as.character(rep(species[i],length(clims)))
  counter=counter+length(clims)
}
df <- data.frame(
  meanrisk=meanrisk,
  serisk=serisk,
  ccnames=factor(ccnames,levels=clims),
  speciesname=factor(speciesname,levels=as.character(species[order(meanrisk[which(ccnames=="LEV")])]))
)

 #df <- df[-which(is.na(df$meanrisk)),]    # temporary: remove NAs

limits <- aes(ymax = meanrisk + serisk, ymin=meanrisk - serisk)

  #dodge <- position_dodge(width=0.9)
p <- ggplot(df,aes(speciesname,meanrisk)) + 
		geom_bar(position="dodge", stat="identity",aes(fill=ccnames)) +   #
            #geom_errorbar(limits,position=position_dodge(),width=.1) +            # 
            scale_x_discrete(xax) + 
            coord_cartesian(ylim = ylimits) +
            scale_y_continuous(paste(yax," (",rtype,")",sep=""),labels= eval(parse(text=fmt)),breaks=seq(-1,ylimits[2], 1)) +  # limits=ylimits
            scale_fill_manual("Climate scenario",values=rev(brewer.pal(3,"Set1"))) +
              #facet_grid(eval(parse(text=paste(". ~ ","group",sep=""))),scales="free" ) +
            opts(title=paste("Climate risk,","by species",sep=" "))+
            opts(axis.text.x=theme_text(angle=-90, hjust=0)) #+
            #coord_flip() 
#dodge <- position_dodge(width=0.1)
p


##########################################################
########################   SUMMARY HISTOGRAMS OF CLIMATE CHANGE EFFECTS

#  possible generic models: Snake, Turtle, Small_Salamander, Large_Salamander, Tortoise

#   response variables: ext.risk, exp.min.n, prob.1000, metapop.chng

#  possible fmts: fractions, thousands

response <- "metapop.chng"  #  "exp.min.n"  #
predictor <- "clim.chg"
sp.group <- "All Species"  #  "Small_Salamander"  #  "Snake"  #  "Turtle"  #  "Large_Salamander" # "Tortoise" # "Snake" #  
xax <- "Fraction population change"  #  "Expected minimum abundance"  #
yax <- "Probability density"
fmt <- "fractions" # "thousands"  #
xlimits <- c(-1,4)    # ylimits <- c(-2,4)
dframe <- "dfmain"  #  "df_rel"  #  dfmain or df_rel for absolute or relative metrics
rtype <- "absolute"  #  "relative"   # 

if(sp.group!="All Species"){
  eval(parse(text=paste("subset1 <- subset(",dframe,",model==\"",sp.group,"\")",sep="") ))
} else{
  subset1 <- dfmain
}

p <- 
    ggplot(subset1,aes(x=eval(parse(text=response))) ) +       #,
    #geom_histogram(aes(y=..density..,fill=eval(parse(text=predictor)) )) +  # colour="black",
    coord_cartesian(xlim = xlimits) +
    #if(sp.group!="All Species") eval(parse(text=paste("facet_grid(eval(parse(text=paste(\"spec~.\",sep=\"\"))),scales=\"free\" ) +",sep="")))  # ,predictor    eval(parse(text=predictor))
    geom_density(adjust=3,aes(x=eval(parse(text=response)),fill=eval(parse(text=predictor)))) +   # ,colour=eval(parse(text=predictor))  # aes(x=eval(parse(text=response)))
    opts(title=paste(sp.group,sep=" "),axis.text.y = theme_blank(),axis.ticks = theme_blank()) +
    scale_fill_manual("Climate scenario",values=rev(brewer.pal(3,"Set1"))) + #,start=0.8,end=0.05
    scale_x_continuous(paste(xax," (",rtype,")",sep=""),labels= eval(parse(text=fmt)),breaks=seq(xlimits[1],xlimits[2], 0.5) ) +  # limits = xlimits 
    scale_y_continuous(yax)
p



##############################################################################
#########################################  START MAIN ANALYSES  (BRT and RF)
 



##############################################################################
#############################################################################
#############################################################################
                                             #  BOOSTING MODELS...
  
response <- "ext.risk"  #  "extinct5"  #   "log.ema"  #  "metapop.logfracinit"  #   "decline"  #   "metapop.chng"  #   "logit.fracchange"  #    "EMA_fracinit_log"  #  "logit.pdecline1000"   #  
responsename <- "Extinction Risk"  #  "Extinction Risk (relative)"  #  
binaryresponse <- FALSE  #  TRUE  #   
fam <- "gaussian"  #  "bernoulli"  #  (use bernoulli for binary, 0,1; e.g., extinct5 response) 
climate <- "WRE"  #  "LEV"  #  "NoCC"  #       
dframe <- "df_rel"  #  "dfmain"   #  
lr <- 0.001 # 0.005 #learning rate
tc <- 5 #tree complexity

pred_cols <- eval(parse(text=paste("which(names(",dframe,")%in%predictors)",sep="")))   #  note: predictors set prior to developing the df_rel data frame
resp_col <- eval(parse(text=paste("which(names(",dframe,")==response)",sep="")))    #which(names(dfmain)==response)

eval(parse(text=paste("subset1 <- ",dframe,"[which(",dframe,"$clim.chg==climate),]",sep="")))   ### dfmain[which(dfmain$model%in%sp.groups),]


             ########### try different BRT models here

   # note: code for gbm.step_debug() in separate text file...  uses browser() to stop within the excecution
                # of the function 

foldVector <- make_foldvec(n.folds=10)   # 

gbm.model1 <- gbm.step(data = subset1, n.folds=max(foldVector), gbm.x = pred_cols, fold.vector=foldVector,                      # c(1,3,4,5,6,7,8,9,19,20,21)
     gbm.y = resp_col, family = fam, tree.complexity = tc, learning.rate = lr, n.trees = 25, step.size = 25, 
     bag.fraction = find_fraction(subset1), max.trees = 10000, tolerance.method = "auto", tolerance = 0.01,
     keep.fold.models = TRUE, keep.fold.vector = TRUE, keep.fold.fit = TRUE   # for cross validation...
)  # tolerance.method = "auto"    # bag.fraction = 0.5, #    0.5
     
workingModel <- "gbm.model1"

#note to Richard: for model selection, best to use deviance; metrics that follow are best for reporting overall quality of the selected model

            ##################
                      EXTRACT PERFORMANCE METRIC(S)...
                             
                                
#if(!binaryresponse){           # ... if continuous response with Gaussian error
           # compare to original response variable

CVprediction <- eval(parse(text=paste(workingModel,"$fold.fit",sep="")))
if(binaryresponse) CVprediction <- plogis(CVprediction)   # 

realdata <- eval(parse(text=paste("subset1$",response,sep="")))
realprediction <- eval(parse(text=paste(workingModel,"$fitted",sep="")))
CV_RMSE = sqrt(mean((realdata-CVprediction)^2))       # root mean squared error for holdout samples in 10-fold cross-validation ...
real_RMSE = sqrt(mean((realdata-realprediction)^2))  # root mean squared error for residuals from final model

CV_RMSE                       ## display RMSE
real_RMSE


########################################
################################   PSEUDO R-SQUARED

fit_deviance_CV <- eval(parse(text=paste(workingModel,"$cv.statistics$deviance.mean",sep="")))   #   eval(parse(gbm.logfracinit.tc6.lr01$cv.statistics$deviance.mean
fit_deviance_real <- mean((realdata-realprediction)^2)    # mean deviance for full model.
if(binaryresponse) fit_deviance_real <- mean(-2*(dbinom(realdata,1,realprediction,log=T)-dbinom(realdata,1,realdata,log=T)))
  
null_deviance <- mean((realdata-mean(realdata))^2) 
if(binaryresponse) null_deviance <- mean(-2*(dbinom(realdata,1,mean(realdata),log=T)-dbinom(realdata,1,realdata,log=T)))
deviance_explained_CV <- (null_deviance-fit_deviance_CV)/null_deviance   # based on holdout samples
deviance_explained_real <- (null_deviance-fit_deviance_real)/null_deviance   # based on full model...

                        # print pseudo r-squared statistics
deviance_explained_CV
deviance_explained_real

  ####################################
       ########## Alternative method : 
     ### use cross validation algorithm...

n.folds = length(unique(subset1$spec))   #   10   #   #  to make leave-one-out, choose number of species: n.folds = length(unique(dfmain$spec))
n.trees = eval(parse(text=paste("max(",workingModel,"$trees.fitted)",sep="")))  # 1500  # 1300
foldVector <- make_foldvec(n.folds=n.folds) 
    
counter = 1
CVprediction <- numeric(nrow(subset1))
CVobserved <- numeric(nrow(subset1))
realprediction <- numeric(nrow(subset1))
      
for(i in 1:n.folds){
  model <- gbm.fixed(data = subset1[which(foldVector!=i),], gbm.x = pred_cols,         
          gbm.y = resp_col, family = fam, tree.complexity = tc, learning.rate = lr, n.trees = n.trees,  
          bag.fraction = find_fraction(subset1))  # bag.fraction=0.5)  #   # tolerance.method = "auto"    #  

  CVprediction[counter:(counter+(length(which(foldVector==i))-1))] <- predict.gbm(model, subset1[which(foldVector==i),], n.trees = n.trees, 
      type = "response")
  CVobserved[counter:(counter+(length(which(foldVector==i))-1))] <- eval(parse(text=paste("subset1$",response,"[which(foldVector==i)]",sep="")))    #subset1$ext.risk[which(foldVector==i)]
  realprediction[counter:(counter+(length(which(foldVector==i))-1))] <- predict.gbm(gbm.model1,subset1[which(foldVector==i),], n.trees=n.trees, 
      type="response") 
  counter = counter + length(which(foldVector==i))
}

     #realdata <- eval(parse(text=paste("subset1$",response,sep="")))
     #realprediction <- data.cforest@predict_response()
CV_RMSE = sqrt(mean((CVobserved-CVprediction)^2))       # root mean squared error for holdout samples in 10-fold cross-validation ...
real_RMSE = sqrt(mean((CVobserved-realprediction)^2))  # root mean squared error for residuals from final model

CV_RMSE                       ## display RMSE
real_RMSE

         ## if binary predictor, display ROC and compute binary classification performance measures
if(binaryresponse){
  graphics.off()
  par(mfrow=c(2,1))
  pred <- prediction(CVprediction,CVobserved)     # for holdout samples in cross-validation
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc")
  plot(perf, main="Predictions from holdout sample")
  text(.9,.1,paste("AUC = ",round(auc@y.values[[1]],2),sep=""))

  pred <- prediction(realprediction,CVobserved)     # for final model
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc")
  plot(perf,main="Predictions from full model")
  text(.9,.1,paste("AUC = ",round(auc@y.values[[1]],2),sep=""))
} 

         ## if non-binary predictor, display ROC and force computation of binary classification performance measures
             #
if(!binaryresponse){
  graphics.off()
  par(mfrow=c(2,1))  
  pred <- prediction(CVprediction,ifelse(CVobserved>=0.5,1,0))     # for holdout samples in cross-validation
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc")
  plot(perf, main="Predictions from holdout sample")
  text(.9,.1,paste("AUC = ",round(auc@y.values[[1]],2),sep=""))

  pred <- prediction(realprediction,ifelse(CVobserved>=0.5,1,0))     # for final model
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc")
  plot(perf, main="Predictions from full model")
  text(.9,.1,paste("AUC = ",round(auc@y.values[[1]],2),sep=""))
}


                          # if not binary response, then make artificial thresholds...
if(!binaryresponse){
  graphics.off()
  par(mfrow=c(2,1))
  #plot(0,0,type="n")    # for full model
  thresholds <- seq(0.01,0.99,length=101)   # "artificial" extinction thresholds across which to examine performance
  kappa <- numeric(length(thresholds))
  for(i in 1:length(thresholds)){
    trueLabels <- ifelse(CVobserved>=thresholds[i],1,0)
    predLabels <- ifelse(CVprediction>=thresholds[i],1,0)
     #pred <- prediction(CVprediction,trueLabels)
     #perf <- performance(pred, "err")
     #plot(perf, avg= "vertical", 
      #spread.estimate="boxplot", 
      #show.spread.at= seq(0, 1, by=0.1))
    tot <- length(CVobserved)
    tp <- length(which((trueLabels==1)&(predLabels==1)))  
    tn <- length(which((trueLabels==0)&(predLabels==0)))
    fp <- length(which((trueLabels==0)&(predLabels==1)))
    fn <- length(which((trueLabels==1)&(predLabels==0)))
    pr_agree <- (tp+tn)/tot    # overall agreement, or accuracy
    pr_agree_rand <- ((tp+fn)/tot)*((tp+fp)/tot)+((fn+tn)/tot)*((fp+tn)/tot)
    kappa[i] <- (pr_agree-pr_agree_rand)/(1-pr_agree_rand)
  }
  plot(thresholds,kappa,type="l",xlab="Ext. risk threshold", ylab="Cohen's Kappa", main="Holdout sample performance")

  kappa <- numeric(length(thresholds))
  for(i in 1:length(thresholds)){
    trueLabels <- ifelse(CVobserved>=thresholds[i],1,0)
    predLabels <- ifelse(realprediction>=thresholds[i],1,0)
    tot <- length(CVobserved)
    tp <- length(which((trueLabels==1)&(predLabels==1)))  
    tn <- length(which((trueLabels==0)&(predLabels==0)))
    fp <- length(which((trueLabels==0)&(predLabels==1)))
    fn <- length(which((trueLabels==1)&(predLabels==0)))
    pr_agree <- (tp+tn)/tot    # overall agreement, or accuracy
    pr_agree_rand <- ((tp+fn)/tot)*((tp+fp)/tot)+((fn+tn)/tot)*((fp+tn)/tot)
    kappa[i] <- (pr_agree-pr_agree_rand)/(1-pr_agree_rand)
  }
  plot(thresholds,kappa,type="l",xlab="Ext. risk threshold", ylab="Cohen's Kappa", main="Performance: full model")

      ## confusion matrix for 50% extinction risk
  threshold=0.5
  trueLabels <- ifelse(CVobserved>=threshold,1,0)
  predLabels <- ifelse(CVprediction>=threshold,1,0)    # realprediction OR CVprediction
  tot <- length(CVobserved)
  tp <- length(which((trueLabels==1)&(predLabels==1)))  
  tn <- length(which((trueLabels==0)&(predLabels==0)))
  fp <- length(which((trueLabels==0)&(predLabels==1)))
  fn <- length(which((trueLabels==1)&(predLabels==0)))
  matrix(c(tp,fp,fn,tn),nrow=2,ncol=2,dimnames=list(c("T","F"),c("T","F")))
  pr_agree <- (tp+tn)/tot    # overall agreement, or accuracy
  pr_agree_rand <- ((tp+fn)/tot)*((tp+fp)/tot)+((fn+tn)/tot)*((fp+tn)/tot)
  kappa <- (pr_agree-pr_agree_rand)/(1-pr_agree_rand)
  kappa
}


fit_deviance_CV <- mean((CVobserved-CVprediction)^2)
if(binaryresponse) fit_deviance_CV <- mean(-2*(dbinom(CVobserved,1,CVprediction,log=T)-dbinom(realdata,1,realdata,log=T)))
fit_deviance_real <- mean((CVobserved-realprediction)^2)
if(binaryresponse) fit_deviance_real <- mean(-2*(dbinom(CVobserved,1,realprediction,log=T)-dbinom(realdata,1,realdata,log=T)))

null_deviance <- mean((CVobserved-mean(CVobserved))^2)
if(binaryresponse) null_deviance <- mean(-2*(dbinom(CVobserved,1,mean(realdata),log=T)-dbinom(realdata,1,realdata,log=T)))
deviance_explained_CV <- (null_deviance-fit_deviance_CV)/null_deviance   # based on holdout samples
deviance_explained_real <- (null_deviance-fit_deviance_real)/null_deviance   # based on full model...

deviance_explained_CV
deviance_explained_real


##########################################################
			### RESIDUAL PLOTS 
                   ###########   eval(parse(text=paste(workingModel,"$fitted",sep="")))     # gbm.fracChange.tc4.lr01$fitted

           # compare to original response variable
  #cbind(eval(,eval(parse(text=paste("subset1$",response,sep=""))) )

           # residuals
residuals <- eval(parse(text=paste("subset1$",response,sep=""))) - eval(parse(text=paste(workingModel,"$fitted",sep="")))
hist(residuals,freq=F,breaks=25)
curve(dnorm(x,mean(residuals),sd(residuals)),add=T)

qqnorm(residuals)


                   ############ variable importance
graphics.off()
par(mai=c(0.95,3.1,0.6,0.4))
eval(parse(text=paste("summary(",workingModel,",las=1,main=paste(responsename,climate,sep=\", \"))",sep="")))         #summary(nasatest.tc8.lr05)

graphics.off()  ##run this to reset graphics devise AFTER running the above plot##

                   ############# remove unimportant variables (simplify)
			##########Note: read up on how this works (Elith et al.)

#eval(parse(text=paste("nasasimp <- gbm.simplify(",workingModel,", n.drops = 5)",sep="")))         #nasasimp <- gbm.simplify(nasatest.tc5.lr01, n.drops = 5)

        # from plot: optimal number of variables to drop: 2

#nasatest.simple <- gbm.step(data = subset1, gbm.x = c(1,4,5,6,7,8,19,20),
#gbm.y = 16, family = "gaussian", tree.complexity = 5, learning.rate = 0.01,
#bag.fraction = 0.5)
               
                   ############ visualize univariate response
                   ############# plot out fitted functions/ fitted values

#par(mfrow=c(3,3))
graphics.off()
eval(parse(text=paste("gbm.plot(",workingModel,", n.plots = 12, common.scale=FALSE, write.title = FALSE)",sep="")))   #gbm.plot(nasatest.tc5.lr01, n.plots = 9, write.title = FALSE)

#eval(parse(text=paste("gbm.plot.fits(",workingModel,")",sep="")))   #gbm.plot.fits(nasatest.simple)

graphics.off()  ##run this to reset graphics devise AFTER running the above plot##


############  NOTE: write script for determining "optimal" number of variables.

  
                ############### find interactions

eval(parse(text=paste("find.int <- gbm.interactions(",workingModel,")",sep="")))   # find.int <- gbm.interactions(nasatest.simple)

find.int$interactions

find.int$rank.list

  ### plot interaction strength
graphics.off()
lengthndx <- min(9,nrow(find.int$rank.list))
par(mai=c(0.95,3.1,0.6,0.4))
barplot(height=(find.int$rank.list[c(1:min(9,nrow(find.int$rank.list))),5][c(lengthndx:1)]),
            horiz=T,las=1,main=paste(responsename, ", ",climate, sep=""),
            xlab="Index of interaction strength",col=brewer.pal(lengthndx,"Blues"),           
            names.arg=paste("",predictorNames[match(find.int$rank.list[,2][c(lengthndx:1)],predictors)],"\n",predictorNames[match(find.int$rank.list[,4][c(lengthndx:1)],predictors)],sep="") )

graphics.off()  ##run this to reset graphics devise AFTER running the above plot##


  #### Plot key interactions in 3-D 
ind1 = 6; ind2 = 1  ##SELECT HERE THE 2 VARS TO PLOT
graphics.off()
eval(parse(text=paste("gbm.perspec(",workingModel,",",ind1,",",ind2,")",sep="")))  # gbm.perspec(nasatest.simple,4,6)  # perspective=F



                ############## explore subtrees of the final model...   
tree_ndx = 6; eval(parse(text=paste("pretty.gbm.tree(",workingModel,", i.tree = ",tree_ndx,")",sep="")))    #pretty.gbm.tree(nasatest.tc5.lr1, i.tree = 1)

             ## visualize the subtrees... 
plot_subtree(eval(parse(text=workingModel)),eval(parse(text=dframe)),
        pred_cols,lr,climate,response,tree_ndx)


#######################################
##################  EXPLORATORY REGRESSION TREES: 

#  possible generic models: Snake, Turtle, Small_Salamander, Large_Salamander, Tortoise

response <- "ext.risk"  #   "extinct5"  #  "metapop.logfracinit"   #   "metapop.chng"  #  "log.ema"  #  "decline"  #   "exp.min.n"  # "ext.risk"  #  "prob.250"  #  "prob.1000"  #  "prob.50"  #  "logit.pdecline1000" "n.mean"  #  "quant.50"  #  "metapop.chng"  #  "EMA_fracinit"  #  "EMA_fracinit_log"  #  "logit.fracchange"  #  "exp.min.n")         
resp.name <- "Risk of extinction"  #   "Binary Extinction Risk"  #  "Fraction remaining at end of simulation (log scale)"  #  "Expected minimum abundance (log scale)"  #   "Decline vs. increase"  #  "Change in abundance"  #  "Fraction decline (logit)" #  
binaryresponse <- FALSE  # TRUE  #   
sp.group <-  "All species"  #   "Small_Salamander"  # "Snake"    #  "Turtle" #        "Tortoise"  #  
species <- "all"  #  "FPSN"   #      "BLTU"   #   "AWSN"  #  "BGTU" #    "CTSA"  #  "DNSA" #  "JPSA" #  "OSSA"  #          
dframe <-   "dfmain"  #  "df_rel" #   dfmain or df_rel for absolute or relative metrics
climate <- "WRE"  #  "LEV"  #   "NoCC"  #     
rtype <- "absolute"  #  "relative" #        

predictors2 <- paste(predictors,collapse="+")  #  "IsolationIndex+log.metapop.initab+Gen.Tgen+GrowthRt+log.N.CV.10+avg.corr.dist.b+log.patch.n.10+log.tot.patch.area.10+overall.frac.dim.10+LargestPatchFrac"   #     # spec+   log.mean.t0.disp.rate+

if(sp.group=="All species"){
  sp.group <- c("Snake", "Turtle", "Small_Salamander", "Large_Salamander", "Tortoise", "Lizard")
  eval(parse(text=paste("subset1 <- subset(",dframe,",model%in%","sp.group","&clim.chg==\"",climate,"\")",sep="") ))
} else{
  eval(parse(text=paste("subset1 <- subset(",dframe,",(model==\"",sp.group,"\"&clim.chg==\"",climate,"\")&match.stat==\"Match\")",sep="") ))
}

if(species!="all"){
  eval(parse(text=paste("subset1 <- subset1[which(subset1$spec==\"",species,"\"&match.stat==\"Match\"),]",sep="") ))
}

sp.group2 <- "all species"
if(length(sp.group)==1&species=="all"){
  title <- paste(resp.name,", all ",sp.group," species",sep="")
} else{
 title <- paste(resp.name,", ",rtype,", ",sp.group2,", ",climate,sep="")
}
if(species=="all"&length(sp.group)>1) title <- paste(resp.name,", ",rtype,", ",sp.group2,", ",climate,sep="")

formula <- eval(parse(text=paste(response,"~",predictors2,sep="")))

con <- rpart.control()
treeobj_BRTsplits <- rpart(formula, data=subset1, method="anova", control=con)
graphics.off()
plot(treeobj_BRTsplits)
text(treeobj_BRTsplits)

con <- ctree_control(maxdepth = 4)
treeobj_RFsplits <- ctree(formula, data=subset1,control=con)
graphics.off()
plot(treeobj_RFsplits,main=title,type="simple" , inner_panel=node_inner(treeobj, digits = 2, abbreviate = FALSE, 
  fill = "white", pval = FALSE, id = FALSE),terminal_panel=node_terminal(treeobj, digits = 2, abbreviate = FALSE, 
  fill = c("lightgray", "white"), id = FALSE))    #  #   # type="extended"


######################################
###############  RANDOM FOREST: use conditional inference trees   (should try subsampling etc...)
##note that these RFs use the specifications specified in the EXPLORATORY REGRESSION TREES block, above

  #set.seed(47) 
data.controls <- cforest_unbiased(ntree=1500, mtry=4)   # mtry=3 #SELECT RF PARAMETERS HERE

               # find a fraction that generally yields independent observations
data.controls@fraction <- find_fraction(subset1)

data.cforest <- cforest(formula, data = subset1, controls=data.controls)  # controls=data.controls

   ### NOTE: use "getAnywhere(RandomForest)" to find the source code
      ### type 'party:::RandomForest' to access the source code (but also note that much of the source code
          ### for these analyses is in C++ and R just calls pre-compiled routines.)


data.cforest.varimp <- varimp(data.cforest, conditional = FALSE)   # conditional = TRUE produces memory limitation error

  ### plot importance values...   [[if possible probably best to use conditional- takes intercorrelations into account ]]
graphics.off()
lengthndx <- length(data.cforest.varimp)
par(mai=c(0.95,3.1,0.6,0.4))
col <- rainbow(lengthndx, start = 3/6, end = 4/6)      # rep(brewer.pal(6,"Blues"),each=2)
barplot(height=data.cforest.varimp[order(data.cforest.varimp,decreasing = FALSE)],
            horiz=T,las=1,main=paste(resp.name, ", ",climate, sep=""),
            xlab="Index of overall importance",col=col,           
            names.arg=predictorNames[match(names(data.cforest.varimp),predictors)][order(data.cforest.varimp,decreasing = FALSE)])


graphics.off()  ##run this to reset graphics devise AFTER running the above plot##


###################################
#################### RANDOM FOREST: develop performance metrics
            ##################
                      #  EXTRACT PERFORMANCE METRIC(S)...

   ### use cross validation algorithm...
n.folds = length(unique(df_rel$spec))  #  10  #   #  to make leave-one-out, choose number of species: n.folds = length(unique(df_rel$spec))
foldVector <- make_foldvec(n.folds=n.folds)

counter = 1
CVprediction <- numeric(nrow(subset1))
CVobserved <- numeric(nrow(subset1))
realprediction <- numeric(nrow(subset1))
#test <- numeric(nrow(subset1))
for(i in 1:n.folds){
   #data.controls@fraction <- 0.64  # find_fraction(subset1[which(foldVector!=i),])
  model <- cforest(formula, data = subset1[which(foldVector!=i),], controls=data.controls)
  CVprediction[counter:(counter+(length(which(foldVector==i))-1))] <- predict(model,newdata=subset1[which(foldVector==i),])
  CVobserved[counter:(counter+(length(which(foldVector==i))-1))] <- eval(parse(text=paste("subset1$",response,"[which(foldVector==i)]",sep="")))  #subset1$ext.risk[which(foldVector==i)]
  #test[counter:(counter+(length(which(foldVector==i))-1))] <- i
  realprediction[counter:(counter+(length(which(foldVector==i))-1))] <- predict(data.cforest,newdata=subset1[which(foldVector==i),]) 
  counter = counter + length(which(foldVector==i))
}

   #realdata <- eval(parse(text=paste("subset1$",response,sep="")))
   #realprediction <- data.cforest@predict_response()
CV_RMSE = sqrt(mean((CVobserved-CVprediction)^2))       # root mean squared error for holdout samples in 10-fold cross-validation ...
real_RMSE = sqrt(mean((CVobserved-realprediction)^2))  # root mean squared error for residuals from final model

       # print RMSE statistics
CV_RMSE 
real_RMSE

 
if(binaryresponse){
  graphics.off()
  par(mfrow=c(2,1))
  pred <- prediction(CVprediction,CVobserved)     # for holdout samples in cross-validation
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc")
  plot(perf)
  text(.9,.1,paste("AUC = ",round(auc@y.values[[1]],2),sep=""))

  pred <- prediction(realprediction,CVobserved)     # for final model
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc")
  plot(perf)
  text(.9,.1,paste("AUC = ",round(auc@y.values[[1]],2),sep=""))
}

         ## if non-binary predictor, display ROC and force computation of binary classification performance measures
             #
if(!binaryresponse){
  graphics.off()
  par(mfrow=c(2,1))  
  pred <- prediction(CVprediction,ifelse(CVobserved>=0.5,1,0))     # for holdout samples in cross-validation
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc")
  plot(perf, main="Predictions from holdout sample")
  text(.9,.1,paste("AUC = ",round(auc@y.values[[1]],2),sep=""))

  pred <- prediction(realprediction,ifelse(CVobserved>=0.5,1,0))     # for final model
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc")
  plot(perf, main="Predictions from full model")
  text(.9,.1,paste("AUC = ",round(auc@y.values[[1]],2),sep=""))
}

                          # if not binary response, then make artificial thresholds...
if(!binaryresponse){
  graphics.off()
  par(mfrow=c(2,1))
  #plot(0,0,type="n")    # for full model
  thresholds <- seq(0.01,0.99,length=101)   # "artificial" extinction thresholds across which to examine performance
  kappa <- numeric(length(thresholds))
  for(i in 1:length(thresholds)){
    trueLabels <- ifelse(CVobserved>=thresholds[i],1,0)
    predLabels <- ifelse(CVprediction>=thresholds[i],1,0)
    tot <- length(CVobserved)
    tp <- length(which((trueLabels==1)&(predLabels==1)))  
    tn <- length(which((trueLabels==0)&(predLabels==0)))
    fp <- length(which((trueLabels==0)&(predLabels==1)))
    fn <- length(which((trueLabels==1)&(predLabels==0)))
    pr_agree <- (tp+tn)/tot    # overall agreement, or accuracy
    pr_agree_rand <- ((tp+fn)/tot)*((tp+fp)/tot)+((fn+tn)/tot)*((fp+tn)/tot)
    kappa[i] <- (pr_agree-pr_agree_rand)/(1-pr_agree_rand)
  }
  plot(thresholds,kappa,type="l",xlab="Ext. risk threshold", ylab="Cohen's Kappa", main="Holdout sample performance")

  kappa <- numeric(length(thresholds))
  for(i in 1:length(thresholds)){
    trueLabels <- ifelse(CVobserved>=thresholds[i],1,0)
    predLabels <- ifelse(realprediction>=thresholds[i],1,0)    
    tot <- length(CVobserved)
    tp <- length(which((trueLabels==1)&(predLabels==1)))  
    tn <- length(which((trueLabels==0)&(predLabels==0)))
    fp <- length(which((trueLabels==0)&(predLabels==1)))
    fn <- length(which((trueLabels==1)&(predLabels==0)))
    pr_agree <- (tp+tn)/tot    # overall agreement, or accuracy
    pr_agree_rand <- ((tp+fn)/tot)*((tp+fp)/tot)+((fn+tn)/tot)*((fp+tn)/tot)
    kappa[i] <- (pr_agree-pr_agree_rand)/(1-pr_agree_rand)
  }
  plot(thresholds,kappa,type="l",xlab="Ext. risk threshold", ylab="Cohen's Kappa", main="Performance: full model")

      ## confusion matrix for 50% extinction risk
  threshold=0.5
  trueLabels <- ifelse(CVobserved>=threshold,1,0)         
  predLabels <- ifelse(CVprediction>=threshold,1,0)     # CVprediction or realprediction
  tot <- length(CVobserved)
  tp <- length(which((trueLabels==1)&(predLabels==1)))  
  tn <- length(which((trueLabels==0)&(predLabels==0)))
  fp <- length(which((trueLabels==0)&(predLabels==1)))
  fn <- length(which((trueLabels==1)&(predLabels==0)))
  matrix(c(tp,fp,fn,tn),nrow=2,ncol=2,dimnames=list(c("T","F"),c("T","F")))
  pr_agree <- (tp+tn)/tot    # overall agreement, or accuracy
  pr_agree_rand <- ((tp+fn)/tot)*((tp+fp)/tot)+((fn+tn)/tot)*((fp+tn)/tot)
  kappa <- (pr_agree-pr_agree_rand)/(1-pr_agree_rand)
  kappa
}

fit_deviance_CV <- mean((CVobserved-CVprediction)^2)
if(binaryresponse) fit_deviance_CV <- mean(-2*(dbinom(CVobserved,1,CVprediction,log=T)-dbinom(realdata,1,realdata,log=T)))
fit_deviance_real <- mean((CVobserved-realprediction)^2)
if(binaryresponse) fit_deviance_real <- mean(-2*(dbinom(CVobserved,1,realprediction,log=T)-dbinom(realdata,1,realdata,log=T)))
null_deviance <- mean((CVobserved-mean(CVobserved))^2)
if(binaryresponse) null_deviance <- mean(-2*(dbinom(CVobserved,1,mean(CVobserved),log=T)-dbinom(realdata,1,realdata,log=T)))
deviance_explained_CV <- (null_deviance-fit_deviance_CV)/null_deviance   # based on holdout samples
deviance_explained_real <- (null_deviance-fit_deviance_real)/null_deviance   # based on full model...

deviance_explained_CV
deviance_explained_real

			### RESIDUAL PLOTS 
                   ###########   eval(parse(text=paste(workingModel,"$fitted",sep="")))     # gbm.fracChange.tc4.lr01$fitted

           # compare to original response variable
  #cbind(eval(,eval(parse(text=paste("subset1$",response,sep=""))) )

           # residuals
residuals <- CVobserved - realprediction 
graphics.off()
hist(residuals,freq=F,breaks=25)
curve(dnorm(x,mean(residuals),sd(residuals)),add=T)

qqnorm(residuals)



#############################  RANDOM FOREST
#############################  Display univariate plots
  

   ##  note: for now, must have run the analogous boosted regression model (same response and predictor set) first.
     ## since I am using some data from the GBM object...

graphics.off()
RF_UnivariatePlots(object=data.cforest, varimp=data.cforest.varimp, data=subset1, 
                   predictors=predictors,plot.layout=c(3,4))
  
graphics.off()


####################################
#######################   RANDOM FOREST FIND AND PLOT INTERACTIONS

          # NOTE: this one can take a very long time- maybe up to 10 minutes...
rf_findint <- RF_FindInteractions(object=data.cforest,data=subset1,predictors=predictors)

    # display and plot out interactions...
rf_findint$interactions

rf_findint$rank.list

  ### plot interaction strength
graphics.off()
lengthndx <- min(9,nrow(find.int$rank.list))
par(mai=c(0.95,3.1,0.6,0.4))
barplot(height=(rf_findint$rank.list[c(1:min(9,nrow(find.int$rank.list))),5][c(lengthndx:1)]),
            horiz=T,las=1,main=paste(response, " ",climate, sep=""),
            xlab="Index of interaction strength",col=brewer.pal(lengthndx,"Blues"),           
            names.arg=paste("",predictorNames[match(rf_findint$rank.list[,2][c(lengthndx:1)],predictors)],"\n",predictorNames[match(rf_findint$rank.list[,4][c(lengthndx:1)],predictors)],sep="") )

graphics.off()


rf_findint$rank.list

graphics.off()
RF_InteractionPlots(13,9,object=data.cforest,data=subset1,predictors=predictors,family=fam) 

graphics.off()

                ############## explore subtrees of the final model...   
             #  KTS: I have not been able to figure out a way to view the individual trees as pretty plots. But the information 
                 # in each tree can be summarized nicely with the following code:
tree_ndx = 5
party:::prettytree(data.cforest@ensemble[[tree_ndx]], names(data.cforest@data@get("input")))

          # in addition, the following algorithm should make trees that COULD HAVE BEEN in the forest (same algorithm):
randomndx <- sample(c(1:nrow(subset1)),round(find_fraction(subset1)*nrow(subset1)),replace=F)
subset2 <- subset1[randomndx,]
con <- ctree_control(
            mtry = 4, 
		savesplitstats = TRUE, 
		maxdepth = 0
)
treeobj <- ctree(formula, data=subset2, control=con)

plot(treeobj,main=title,type="simple" , inner_panel=node_inner(treeobj, digits = 2, abbreviate = FALSE, 
  fill = "white", pval = FALSE, id = FALSE),terminal_panel=node_terminal(treeobj, digits = 2, abbreviate = FALSE, 
  fill = c("lightgray", "white"), id = FALSE))    


















  