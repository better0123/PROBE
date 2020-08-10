source("./moddicom_functions/features.R")
library(moddicomV2)

## CHANGE ACCORDINGLY TO THE US MACHINE DATASET

# load("./voluson.RData")
# dataset <- final.result

# load("./samsung.RData")
# dataset <- final.result

# load("./esaote.RData")
# dataset <- final.result

load("./toshiba.RData")
dataset <- final.result


if("matrice_toshiba.RData" %in% list.files() ){
  load("matrice_toshiba.RData")
  pat_no <- sub("\\_.*", "", matrice[,1])
  pat_si <- list.files("./dati_dicom_finali_toshiba/")
  dataset <- dataset[which(names(dataset) %in% pat_si)]
  dataset <- dataset[which(!names(dataset) %in% pat_no)]
  i <- length(which(!is.na(matrice[,1])))
}else {
  i <- 1
  righe <- 1
  for(pat in names(dataset)){
    for(image in names(dataset[[pat]])){
      righe <- righe + 1
    }
  }
  matrice <- matrix(data = NA, nrow = righe, ncol = 233)
}

rigaFeatures <- c()
patientsPath <- c()
for(pat in names(dataset)){
  cat(pat)
  cat("\n")
  for(image in names(dataset[[pat]])){
    cat(image)
    cat("\n")
    if(class(dataset[[pat]][[image]]) == "NULL"){
      rigaFeatures <- c(rep("NA", 232))
      patientsPath <- paste(pat,image,sep = "_")
      matrice[i,] <- c("patID"=patientsPath, rigaFeatures)
    }
    else{
      rigaFeatures <- extract_features(voxelCube = round(dataset[[pat]][[image]]*255), n_grey = 100)
      patientsPath <- paste(pat,image,sep = "_")
      matrice[i,] <- c("patID"=patientsPath, unlist(rigaFeatures))
    }
    i <- i + 1
    save(matrice,file = "matrice_toshiba.RData")
  }
}


toshiba_data <- as.data.frame(matrice)
toshiba_data[,2:dim(toshiba_data)[2]] <- lapply(toshiba_data[,2:dim(toshiba_data)[2]] , function(x) as.numeric(as.character(x)))
toshiba_data[,1] <- as.character(toshiba_data[,1])
save(toshiba_data,file = "toshiba_data.RData")
