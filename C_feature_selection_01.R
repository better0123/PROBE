library(readr)
## CARICO E PROCESSO I DATI CLINICI

Probe_dati <- read_delim("Probe_dati.csv", ",", escape_double = FALSE, trim_ws = TRUE)
dati_us <- Probe_dati[which(Probe_dati$US != 0),]

dati_us <- dati_us[!duplicated(dati_us),]

dati_us$SURNAME <- gsub("[[:punct:]]", "", dati_us$SURNAME)
dati_us$SURNAME <- gsub(" ", "", dati_us$SURNAME)

### MERGE CON OUTCOME CLINICO VOLUSON

load("voluson_data.RData")
voluson_data <- voluson_data[-dim(voluson_data)[1],]

voluson_data$gBRCA <- NA
i <- 1
voluson_data_CS <- numeric()
for(pat in voluson_data$pat_image){
  cat(i)
  cat("\n")
  #if(i == 409) browser()
  pat_sb <- sub("\\_.*", "", pat)
  voluson_data[i,"gBRCA"] <- dati_us[grep(pattern = pat_sb , dati_us$SURNAME, ignore.case = T), "gBRCA STATUS"]
  #voluson_data[i,"CS"] <- dati_us[grep(pattern = pat_sb , dati_us$SURNAME, ignore.case = T), "CS"]
  voluson_data_CS[i] <- dati_us[grep(pattern = pat_sb , dati_us$SURNAME, ignore.case = T), "CS"]
  i <- i +1
}

voluson_data_CS <- unlist(unique(voluson_data_CS))

voluson_data$gBRCA <- as.numeric(voluson_data$gBRCA)

# le frattali all population
load("matrice_voluson_fractals.RData")
voluson_fractals_data <- as.data.frame(matrice)
names(voluson_fractals_data) <- c("pat_image","mean_0.100", "median_0.100", "min_0.100", "max_0.100", "sd_0.100")
voluson_fractals_data[,2:dim(voluson_fractals_data)[2]] <- lapply(voluson_fractals_data[,2:dim(voluson_fractals_data)[2]] , function(x) as.numeric(as.character(x)))
voluson_fractals_data[,1] <- as.character(voluson_fractals_data[,1])

voluson_data <- merge(voluson_fractals_data, voluson_data, by = "pat_image")

# le frattali 0-50
load("matrice_voluson_fractals_050.RData")
voluson_fractals_data_050 <- as.data.frame(matrice)
names(voluson_fractals_data_050) <- c("pat_image","mean_0.50", "median_0.50", "min_0.50", "max_0.50", "sd_0.50")
voluson_fractals_data_050[,2:dim(voluson_fractals_data_050)[2]] <- lapply(voluson_fractals_data_050[,2:dim(voluson_fractals_data_050)[2]] , function(x) as.numeric(as.character(x)))
voluson_fractals_data_050[,1] <- as.character(voluson_fractals_data_050[,1])

voluson_data <- merge(voluson_fractals_data_050, voluson_data, by = "pat_image")

# le frattali 50-100
load("matrice_voluson_fractals_50100.RData")
voluson_fractals_data_50100 <- as.data.frame(matrice)
names(voluson_fractals_data_50100) <- c("pat_image","mean_50.100", "median_50.100", "min_50.100", "max_50.100", "sd_50.100")
voluson_fractals_data_50100[,2:dim(voluson_fractals_data_50100)[2]] <- lapply(voluson_fractals_data_50100[,2:dim(voluson_fractals_data_50100)[2]] , function(x) as.numeric(as.character(x)))
voluson_fractals_data_50100[,1] <- as.character(voluson_fractals_data_50100[,1])

voluson_data <- merge(voluson_fractals_data_50100, voluson_data, by = "pat_image")

### MERGE CON OUTCOME CLINICO SAMSUNG

load("samsung_data.RData")
samsung_data <- samsung_data[-dim(samsung_data)[1],]

samsung_data$gBRCA <- NA
i <- 1
samsung_data_CS <- numeric()
for(pat in samsung_data$pat_image){
  cat(i)
  cat("\n")
  #if(i == 261) browser()
  pat_sb <- sub("\\_.*", "", pat)
  samsung_data[i,"gBRCA"] <- dati_us[grep(pattern = pat_sb , dati_us$SURNAME, ignore.case = T), "gBRCA STATUS"]
  #samsung_data[i,"CS"] <- dati_us[grep(pattern = pat_sb , dati_us$SURNAME, ignore.case = T), "CS"]
  samsung_data_CS[i] <- dati_us[grep(pattern = pat_sb , dati_us$SURNAME, ignore.case = T), "CS"]
  i <- i +1
}

samsung_data_CS <- unlist(unique(samsung_data_CS))

samsung_data$gBRCA <- as.numeric(samsung_data$gBRCA)

# le frattali all population
load("matrice_samsung_fractals.RData")
samsung_fractals_data <- as.data.frame(matrice)
names(samsung_fractals_data) <- c("pat_image","mean_0.100", "median_0.100", "min_0.100", "max_0.100", "sd_0.100")
samsung_fractals_data[,2:dim(samsung_fractals_data)[2]] <- lapply(samsung_fractals_data[,2:dim(samsung_fractals_data)[2]] , function(x) as.numeric(as.character(x)))
samsung_fractals_data[,1] <- as.character(samsung_fractals_data[,1])

samsung_data <- merge(samsung_fractals_data, samsung_data, by = "pat_image")

# le frattali 0-50
load("matrice_samsung_fractals_050.RData")
samsung_fractals_data_050 <- as.data.frame(matrice)
names(samsung_fractals_data_050) <- c("pat_image","mean_0.50", "median_0.50", "min_0.50", "max_0.50", "sd_0.50")
samsung_fractals_data_050[,2:dim(samsung_fractals_data_050)[2]] <- lapply(samsung_fractals_data_050[,2:dim(samsung_fractals_data_050)[2]] , function(x) as.numeric(as.character(x)))
samsung_fractals_data_050[,1] <- as.character(samsung_fractals_data_050[,1])

samsung_data <- merge(samsung_fractals_data_050, samsung_data, by = "pat_image")

# le frattali 50-100
load("matrice_samsung_fractals_50100.RData")
samsung_fractals_data_50100 <- as.data.frame(matrice)
names(samsung_fractals_data_50100) <- c("pat_image","mean_50.100", "median_50.100", "min_50.100", "max_50.100", "sd_50.100")
samsung_fractals_data_50100[,2:dim(samsung_fractals_data_50100)[2]] <- lapply(samsung_fractals_data_50100[,2:dim(samsung_fractals_data_50100)[2]] , function(x) as.numeric(as.character(x)))
samsung_fractals_data_50100[,1] <- as.character(samsung_fractals_data_50100[,1])

samsung_data <- merge(samsung_fractals_data_50100, samsung_data, by = "pat_image")

### MERGE CON OUTCOME CLINICO ESAOTE

load("esaote_data.RData")
esaote_data <- esaote_data[-dim(esaote_data)[1],]

esaote_data$gBRCA <- NA
i <- 1
esaote_data_CS <- numeric()
for(pat in esaote_data$pat_image){
  cat(i)
  cat("\n")
  #if(i == 261) browser()
  pat_sb <- sub("\\_.*", "", pat)
  esaote_data[i,"gBRCA"] <- dati_us[grep(pattern = pat_sb , dati_us$SURNAME, ignore.case = T), "gBRCA STATUS"]
  #esaote_data[i,"CS"] <- dati_us[grep(pattern = pat_sb , dati_us$SURNAME, ignore.case = T), "CS"]
  esaote_data_CS[i] <- dati_us[grep(pattern = pat_sb , dati_us$SURNAME, ignore.case = T), "CS"]
  i <- i +1
}

esaote_data_CS <- unlist(unique(esaote_data_CS))

esaote_data$gBRCA <- as.numeric(esaote_data$gBRCA)

# le frattali all population
load("matrice_esaote_fractals.RData")
esaote_fractals_data <- as.data.frame(matrice)
names(esaote_fractals_data) <- c("pat_image","mean_0.100", "median_0.100", "min_0.100", "max_0.100", "sd_0.100")
esaote_fractals_data[,2:dim(esaote_fractals_data)[2]] <- lapply(esaote_fractals_data[,2:dim(esaote_fractals_data)[2]] , function(x) as.numeric(as.character(x)))
esaote_fractals_data[,1] <- as.character(esaote_fractals_data[,1])

esaote_data <- merge(esaote_fractals_data, esaote_data, by = "pat_image")

# le frattali 0-50
load("matrice_esaote_fractals_050.RData")
esaote_fractals_data_050 <- as.data.frame(matrice)
names(esaote_fractals_data_050) <- c("pat_image","mean_0.50", "median_0.50", "min_0.50", "max_0.50", "sd_0.50")
esaote_fractals_data_050[,2:dim(esaote_fractals_data_050)[2]] <- lapply(esaote_fractals_data_050[,2:dim(esaote_fractals_data_050)[2]] , function(x) as.numeric(as.character(x)))
esaote_fractals_data_050[,1] <- as.character(esaote_fractals_data_050[,1])

esaote_data <- merge(esaote_fractals_data_050, esaote_data, by = "pat_image")

# le frattali 50-100
load("matrice_esaote_fractals_50100.RData")
esaote_fractals_data_50100 <- as.data.frame(matrice)
names(esaote_fractals_data_50100) <- c("pat_image","mean_50.100", "median_50.100", "min_50.100", "max_50.100", "sd_50.100")
esaote_fractals_data_50100[,2:dim(esaote_fractals_data_50100)[2]] <- lapply(esaote_fractals_data_50100[,2:dim(esaote_fractals_data_50100)[2]] , function(x) as.numeric(as.character(x)))
esaote_fractals_data_50100[,1] <- as.character(esaote_fractals_data_50100[,1])

esaote_data <- merge(esaote_fractals_data_50100, esaote_data, by = "pat_image")

### MERGE CON OUTCOME CLINICO TOSHIBA

load("toshiba_data.RData")
toshiba_data <- toshiba_data[-c(221:237),]

toshiba_data$gBRCA <- NA
i <- 1
toshiba_data_CS <- numeric()
for(pat in toshiba_data$pat_image){
  cat(i)
  cat("\n")
  #if(i == 79) browser()
  pat_sb <- as.numeric(sub("\\_.*", "", pat))
  toshiba_data[i,"gBRCA"] <- dati_us[which(as.numeric(dati_us$CS) == pat_sb), "gBRCA STATUS"]
  #toshiba_data[i,"CS"] <- dati_us[which(as.numeric(dati_us$CS) == pat_sb), "CS"]
  i <- i +1
}

toshiba_data_CS <- unique(sub("\\_.*", "", toshiba_data$pat_image))

toshiba_data$gBRCA <- as.numeric(toshiba_data$gBRCA)

# le frattali all population
load("matrice_toshiba_fractals.RData")
toshiba_fractals_data <- as.data.frame(matrice)
names(toshiba_fractals_data) <- c("pat_image","mean_0.100", "median_0.100", "min_0.100", "max_0.100", "sd_0.100")
toshiba_fractals_data[,2:dim(toshiba_fractals_data)[2]] <- lapply(toshiba_fractals_data[,2:dim(toshiba_fractals_data)[2]] , function(x) as.numeric(as.character(x)))
toshiba_fractals_data[,1] <- as.character(toshiba_fractals_data[,1])

toshiba_data <- merge(toshiba_fractals_data, toshiba_data, by = "pat_image")

# le frattali 0-50
load("matrice_toshiba_fractals_050.RData")
toshiba_fractals_data_050 <- as.data.frame(matrice)
names(toshiba_fractals_data_050) <- c("pat_image","mean_0.50", "median_0.50", "min_0.50", "max_0.50", "sd_0.50")
toshiba_fractals_data_050[,2:dim(toshiba_fractals_data_050)[2]] <- lapply(toshiba_fractals_data_050[,2:dim(toshiba_fractals_data_050)[2]] , function(x) as.numeric(as.character(x)))
toshiba_fractals_data_050[,1] <- as.character(toshiba_fractals_data_050[,1])

toshiba_data <- merge(toshiba_fractals_data_050, toshiba_data, by = "pat_image")

# le frattali 50-100
load("matrice_toshiba_fractals_50100.RData")
toshiba_fractals_data_50100 <- as.data.frame(matrice)
names(toshiba_fractals_data_50100) <- c("pat_image","mean_50.100", "median_50.100", "min_50.100", "max_50.100", "sd_50.100")
toshiba_fractals_data_50100[,2:dim(toshiba_fractals_data_50100)[2]] <- lapply(toshiba_fractals_data_50100[,2:dim(toshiba_fractals_data_50100)[2]] , function(x) as.numeric(as.character(x)))
toshiba_fractals_data_50100[,1] <- as.character(toshiba_fractals_data_50100[,1])

toshiba_data <- merge(toshiba_fractals_data_50100, toshiba_data, by = "pat_image")
