
readSpecdata <- function(directory, id = 1:332) {
  specdata <- data.frame()
  for (i in id) {
    specdata <- rbind(specdata, read.csv(file.path(directory, paste(formatC(i, width = 3, flag = "0"), ".csv", sep = "")), colClasses = c("Date", "numeric", "numeric", "integer")))
  }
  specdata
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  specdata <- readSpecdata(directory, id)
  mean(specdata[, pollutant], na.rm = TRUE)
}

complete <- function(directory, id = 1:332) {
  specdata <- readSpecdata(directory, id)
  com <- as.data.frame(table(specdata[is.finite(specdata[, "sulfate"]) & is.finite(specdata[, "nitrate"]), "ID"]))
  colnames(com) <- c("id", "nobs")
  rownames(com) <- paste("row", com[, "id"], sep = "")
  com <- com[paste("row", id, sep = ""), ]
  rownames(com) <- NULL
  com
}

computeCorrelation <- function(data, threshold = 0) {
  com <- data[is.finite(data[, "sulfate"]) & is.finite(data[, "nitrate"]), ]
  ifelse(nrow(com) > threshold, cor(com[, "sulfate"], com[, "nitrate"]), NA)
}

corr <- function(directory, threshold = 0) {
  specdata <- list()
  for (i in 1:332) {
    specdata[[i]] <- read.csv(file.path(directory, paste(formatC(i, width = 3, flag = "0"), ".csv", sep = "")), colClasses = c("Date", "numeric", "numeric", "integer"))
  }
  cors <- sapply(specdata, computeCorrelation, threshold)
  cors[is.finite(cors)]
}
