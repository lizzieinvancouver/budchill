# Rename all files to remove whitespace

# Given a species code x, loop within that directory and rename all files

underscores <- function(x){
  filez <- dir(x)
  
  for(j in 1:length(filez)) {
        photo <- filez[j]
        system(paste(
          "mv '", paste(x, photo, sep="/"), "' ", 
          gsub(" ", "_", paste(x, photo, sep="/")), sep=""), 
          ignore.stderr = T)
    }
}