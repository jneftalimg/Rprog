corr <- function(directory, threshold = 0) {
  
  #Gets and stores the filenames in the specified directory.
  FileNames <- list.files(path=directory)
  
  # Calls "complete" function to get observations per file dataframe.
  NumberOfObs <- complete(directory)
  
  #Creates dataframe with file ID's above threshold value.
  ThresID <- subset(NumberOfObs, nobs > threshold)
  
  #Creates a vector of monitor id's to look up the files.
  FileID <- ThresID$id
  
  #Creates an empty vector CorrVect to store the complied monitor correlations.
  CorrVect <- c()
  
  #If statement checks if there are no files above threshold, otherwise will
  # the code will compile the correlation vector.
  if (length(FileID) == 0) {
    ResultVect <- 0
  } else {
    ResultVect <- c()
    
    for (i in FileID){
      #Reads individual monitor data file
      MonData <- read.csv(file=paste(directory, "/", FileNames[i], sep=""),header=TRUE)
      #Retains the complete cases and purges the NA rows from monitor data.
      MonData <- MonData[complete.cases(MonData),]
      filecorr <- cor(MonData["nitrate"],MonData["sulfate"])
      #Compiles the correlation vector with each loop.
      CorrVect <- c(CorrVect, filecorr)
    }
  }
  #Returns the correlation vector out of the function.
  CorrVect
}