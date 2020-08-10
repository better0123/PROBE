source("/home/kbocalc/Dropbox/moddicomV2/R/06_fractalFeatures.R")
source("/home/kbocalc/Dropbox/moddicomV2/R/services.R")

library(moddicomV2)

## CHANGE ACCORDINGLY TO THE US MACHINE DATASET

# load("./voluson.RData")
# dataset <- final.result

# load("./samsung.RData")
# dataset <- final.result

# load("./esaote.RData")
# dataset <- final.result

load("./esaote.RData")
dataset <- final.result

if("matrice_esaote_fractals_50100.RData" %in% list.files() ){
  load("matrice_esaote_fractals_50100.RData")
  pat_no <- sub("\\_.*", "", matrice[,1])
  pat_si <- list.files("./dati_dicom_finali_esaote/")
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
  matrice <- matrix(data = NA, nrow = righe, ncol = 6)
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
      rigaFeatures <- c(rep("NA", 5))
      patientsPath <- paste(pat,image,sep = "_")
      matrice[i,] <- c("patID"=patientsPath, rigaFeatures)
    }
    else{
      rigaFeatures <- fractalFeatures(imgObj = round(dataset[[pat]][[image]]*255),ThDown = 0, ThUp = 50)
      patientsPath <- paste(pat,image,sep = "_")
      matrice[i,] <- c("patID"=patientsPath, rigaFeatures$report[1,])
    }
    i <- i + 1
    save(matrice,file = "matrice_esaote_fractals_50100.RData")
  }
}
