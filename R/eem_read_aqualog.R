# *************************************************************************
# Fonction reading Aqualog dat files.
# *************************************************************************
eem_read_aqualog <- function(file) {
  data <- read.opj(file)
  
  # Check if the file has .opj extension
  if (tools::file_ext(file) == "opj") {
    # Read the .opj file using the read.opj function
    eem <- read.opj(file)
    
    # Extract the necessary data from the eem object
    ex <- eem
    em <- eem$x[, 1]
    eem <- as.matrix(eem$x[, -1])
  } else {
    # Continue with the original code for other file formats
    eem <- stringr::str_extract_all(data, "-?\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?")
    ex <- sort(as.numeric(eem[[1]]))
    n_col <- lapply(eem, length)
    n_col <- unlist(n_col)
    expected_col <- as.numeric(names(sort(-table(n_col)))[1])
    eem[n_col != expected_col] <- NULL
    eem <- lapply(eem, as.numeric)
    eem <- do.call(rbind, eem)
    em <- eem[, 1]
    eem <- eem[, -1]
    eem <- as.matrix(eem[, ncol(eem):1])
  }

  l <- list(
    file = file,
    x = eem,
    em = em,
    ex = ex
  )

  return(l)
}



