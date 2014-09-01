#
# SOM Toolbox for R
# A Generic version for exploratory data analysis
#
#
#
#
library(kohonen)
library(colorspace) # Need this for hacking palettes

#
# Train N SOMs return the one with best mean avg.deviation from code vects.
# So that we get the SOM with best mapping, with minimal twists.
#
get_best_som <- function(D, n=10, g=somgrid(10, 10, 'hexagonal'), initmat=NULL, alpha)
{
  smap <- NaN
  #serr <- 1.0
  serr <- .Machine$integer.max
  runc <- NaN
  cat("[INFO] Will train",n,"SOMs and pick the one with the least quantization error.\n")
  for(i in seq(1:n))
  {
    if (is.null(initmat))
    {
      cmap <- som(data=D[,1:dim(D)[2]], grid=g, keep.data=T)
    }
    else
    {
      cat("[INFO] Initializing map with given matrix.\n")
      cmap <- som(data=D[,1:dim(D)[2]], grid=g, keep.data=T, init=initmat, alpha=alpha)  
    }
    cerr <- cmap$changes[dim(cmap$changes)[1]]
    cat(paste("[INFO] SOM",i,": Quantization error =",cerr,"\n"))
    if (cerr <= serr)
    {
      serr <- cerr
      smap <- cmap
      runc <- i
    }
  }
  cat("[INFO] Found the best SOM at run =",runc,"with quantization error =",serr,"\n")
  return(smap)
}


#
# som_read_data() : read data from a csv/tsv
# To DO: data sanity checks
# Inputs:
# filename
# separator
# colsToSkip: a vector of positive column indices. e.g. c(1,2,3) removes the first three columns
som_read_data <- function(filename=file.choose(), sep="\t", colsToSkip=NULL, hasLabels=F, labelCol=NULL)
{   
  D <- read.table(filename, sep=sep, header=T, row.names=NULL)
  # Treat all columns as data for now
  dataCols <- c(1:dim(D)[2])
  #Process the label column if needed
  if (hasLabels == T) 
  {
    # determine the label column, if null, make it the last column
    if (is.null(labelCol))
      labelCol = dim(D)[2]
    # Remove labelCol from dataCols and reorder if labelCols is not last
    dataCols <- dataCols[-labelCol]
    labelColName <- names(D)[labelCol]
    # Reorder only if label is not the last column
    if (labelCol != dim(D)[2])
    {
      newOrder <- c(dataCols, labelCol)
      D <- D[, newOrder]
    }
  }
  # Convert data columns to numeric, in case they are not
  # Otherwise scale() gets agitated. 
  if(hasLabels == T)
    colct <- dim(D)[2]-1 # last column is label
  else
    colct <- dim(D)[2]
  for (i in seq(1,colct))
    D[,i] <- as.numeric( D[,i] )
  # Remove columns if needed
  # This is risky, can change the labelCol and dataCol accuracy!!
  if (!is.null(colsToSkip))
    D <- D[ -rowsToSkip ] # drop the first 3 columns
  # Scale data
  D[, dataCols] <- scale(D[, dataCols])
  # Need to handle NAs since som() won't work with them
  # This is naive but will allow the code to continue
  na_count <- length(D[is.na(D)])
  if ( na_count > 0 )
  {
    cat("[WARN] Dataset contains ", na_count, " NAs. Will replace them with 0s but this can change the dataset characteristics.\n")
    D[is.na(D)] <- 0
  }
  # Remove constant columns
  # While they are fine for training, they interfere with plotting
  constant_cols <- names( D[, sapply(D, function(v) var(v, na.rm=TRUE)==0)] )
  if (length(constant_cols) > 0)
  {
    cat("[WARN] The following columns are constant:", constant_cols, ". They will be removed.\n")
    D <- D[,sapply(D, function(v) var(v, na.rm=TRUE)!=0)]
  }
  return(D)
}

#
# som_train() : train a number of SOMs from data and pick the best one
#
som_train <- function( D, grid=sgrid <- somgrid(10, 10, 'hexagonal'), hasLabels=F, somCount=10 )
{
  if (hasLabels == F)
    dataCols <- 1:dim(D)[2]
  else
    dataCols <- 1:dim(D)[2]-1 # som_read_data makes sure the last one is the label, if there is one.
  return(get_best_som( data.matrix(D[,dataCols]), n=somCount))  # Kohonen SOM expects a data.matrix()
}

#
# som_plot() : Plotting functions
#
som_plot <- function(S, D, toFile=F, plotDir="/tmp/", hasLabels=F)
{
  # The following line makes sure the legend does not 
  # appear outside the plotting area.
  par(xpd=NA,oma=c(3,0,0,0)) 
  # Compute general purpose variables
  ts <- gsub("[ :]","_",Sys.time())
  nrows <- dim(D)[1]
  ncols <- dim(D)[2]
  # arbitrary point size cex in mapping plot, 
  # the more data, the smaller the dot
  cex_val <- 200/nrows
  # create directory under plotDir using current timestamp
  if (toFile == T)
  {
    plotDir <- paste(plotDir,ts,".plots/", sep="")
    cat("[INFO] Creating plot directory = ", plotDir,"\n")
    unlink(plotDir, recursive=T)
    dir.create(plotDir,recursive=T) # mkdir -r
  }
  #
  # Determine a suitable number of clusters
  #
  if (hasLabels == T)
  {
    labels <- unique(D[, ncols])
    lnames <- names(table(labels))
    lcount <- length(lnames)
    dimcount <- ncols-1
    cat("[INFO] From labels, the cluster count is set to ", lcount,"\n")
  }else
  {
    lcount <- 5 # arbitrary choice
    dimcount <- ncols
    cat("[INFO] Without labels, the cluster count is arbitrarily set to ", lcount,"\n")
  }
  #
  # Apply hierarchical clustering to find cluster boundaries
  #
  D.hc <- cutree(hclust(dist(S$codes)), lcount)
  #
  # Plot changes, (error over time)
  #
  cat("[INFO] Plotting training error over iterations.\n")
  if(toFile == T)
  {
    fn <- paste(plotDir,"changes.png",sep="")
    png(fn, )
    plot(S, type="changes")
    dev.off()
  }
  else
  {
    plot(S, type="changes")
  }
  #
  # U-Matrix visualization.
  #
  cat("[INFO] Plotting the u-matrix.\n")
  if(toFile == T)
  {
    fn <- paste(plotDir,"u-matrix.png",sep="")
    png(fn, width=1200, height=700)
    plot(S, type="dist.neighbours", main="U-Matrix")
    add.cluster.boundaries(S, D.hc)
    dev.off()
  }
  else
  {
    plot(S, type="dist.neighbours", main="U-Matrix")
    add.cluster.boundaries(S, D.hc)
  }
  #
  # Component planes
  #
  cat("[INFO] Plotting the component planes: ")
  colname = names(S$codes[1,])
  for(i in seq(1, dim(S$codes)[2]))
  {
    cat(colname[i], " ")
    if (toFile == T)
    {
      fn <- paste(plotDir,colname[i],".png",sep="")
      png(fn, width=1200, height=700)
      plot(S, type="property", 
           property=S$codes[,i], 
           main=colname[i])
      dev.off()
    }
    else
    {
      plot(S, type="property", 
           property=S$codes[,i], 
           main=colname[i])
    }
  }
  cat("\n")
  #
  # Hit histograms
  #
  cat("[INFO] Plotting the hit histogram.\n")
  D.hits <- compute_hit_counts(S, D)
  if(toFile == T)
  {
    fn <- paste(plotDir,"hit-histogram.png",sep="")
    png(fn, width=1200, height=700)
    plot(S, type="property", 
         property=D.hits,  main="Hit histogram for training data", 
         palette.name=coolWhiteHotRed)
    dev.off()
  }
  else
  {
    plot(S, type="property", 
         property=D.hits,  main="Hit histogram for training data", 
         palette.name=coolWhiteHotRed)
  }
  #
  # Mapping plots. Different behavior based on the presence or absence of labels
  #
  if( hasLabels == T  )
  {
    labcol <- dim(D)[2]
    bgcols <- rainbow( lcount )
    # Mapping 1 as scatter plots
    if(toFile == T)
    {
      fn <- paste(plotDir,"mapping1.png",sep="")
      png(fn, width=1200, height=700)
      plot(S, type="mapping", 
           col = bgcols[ D[,labcol] ],
           main = "mapping plot", pch=20, cex=cex_val)
      add.cluster.boundaries(S, D.hc)
      dev.off()
    }
    else 
    {
      plot(S, type="mapping", 
           col = bgcols[ D[,labcol] ],
           main = "mapping plot", pch=20, cex=cex_val)
      add.cluster.boundaries(S, D.hc)
    }
    # Mapping 2: as colored neurons
    if(toFile == T)
    {
      fn <- paste(plotDir,"mapping2.png",sep="")
      png(fn, width=1200, height=700)
      xyfpredictions <- as.integer(predict(
        S, 
        trainY=data.matrix(D[,labcol]), 
        trainX=data.matrix(D[,1:dimcount]))$unit.predictions)
      plot(S, type="mapping", 
           bgcol = bgcols[as.integer(xyfpredictions)],
           main = "another mapping plot")
      add.cluster.boundaries(S, D.hc)
      # pch 15 is a filled square
      legend("right", legend=lnames, pch=15, col=bgcols)
      dev.off()
    }
    else
    {
      xyfpredictions <- as.integer(predict(S, 
        trainY=data.matrix(D)[,labcol], 
        trainX=data.matrix(D)[,1:dimcount])$unit.predictions)
      plot(S, type="mapping", 
           bgcol = bgcols[as.integer(xyfpredictions)], 
           main = "another mapping plot")
      add.cluster.boundaries(S, D.hc)
      # pch 15 is a filled square
      legend("right", legend=lnames, pch=15, col=bgcols)
    }
  }
  else # No labels
  {
    #Print hits without coloring.
    if(toFile == T)
    {
      fn <- paste(plotDir,"mapping1.png",sep="")
      png(fn, width=1200, height=700)
      plot(S, type="mapping", 
           col = "gray",
           main = "mapping plot", pch=20, cex=cex_val)
      add.cluster.boundaries(S, D.hc)
      dev.off()
    }
    else
    {
      plot(S, type="mapping", 
           col = "gray",
           main = "mapping plot", pch=20, cex=cex_val)
      add.cluster.boundaries(S, D.hc)
    }
  }
  
}


#
# Function for computing hit histograms 
# converts (data_row, neuron) to (neuron, hit_count)
#
compute_hit_counts <- function(S, D)
{
  hits <- map(S, data.matrix(D))
  hit_count <- rep(0, dim(S$codes)[1])
  for(i in seq(1,length(hits$unit.classif)))
  {
    hit_count[hits$unit.classif[i]] <- hit_count[hits$unit.classif[i]] + 1
  }
  return(hit_count)
}

#
# Palette function
# Hacked this to show hit histograms. The important
# issue is 0 hits gets mapped to white, producing a
# cleaner hit histogram.
#
coolWhiteHotRed <- function(n, alpha = 1) {
  # rev reverses the color palette
  #rev(heat_hcl(n, c = c(100, 0), l = c(30, 90), power = c(1/5, 2)))
  rev(heat_hcl(n, c = c(250, 0), l = c(30, 100), power = c(1/5, 2)))
}

#
# A viewer function for palette hacking.
# Use: pal(coolWhiteHotRed(10))
#
pal <- function(col, border = "light gray", ...)
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
