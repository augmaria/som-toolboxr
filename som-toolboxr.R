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
# som_read_data: read data from a csv/tsv
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
  return(D)
}

#
#
#
som_train <- function( D, grid=sgrid <- somgrid(10, 10, 'hexagonal'), hasLabels=F )
{
  if (hasLabels == F)
    dataCols <- 1:dim(D)[2]
  else
    dataCols <- 1:dim(D)[2]-1 # som_read_data makes sure the last one is the label, if there is one.
  return(get_best_som( data.matrix(D[,dataCols]), n=10))  # Kohonen SOM expects a data.matrix()
}

#
#
#
som_plot <- function(S)
{
  
}