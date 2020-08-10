# from results of "feature_selection_01.R"

#CHANGE ACCORDINGLY TO THE DATASET TO ANALYZE
#dati_completi <- rbind(voluson_data, samsung_data, esaote_data, toshiba_data)
#dati_completi <- voluson_data
#dati_completi <- toshiba_data
#dati_completi <- esaote_data
dati_completi <- samsung_data

dati_completi_stabili <- dati_completi[,-1]

dati_completi_stabili$gBRCA_bin <- 1
dati_completi_stabili$gBRCA_bin[which(dati_completi_stabili$gBRCA == 3)] <- 0

dati_completi_stabili <- dati_completi_stabili[,-which(names(dati_completi_stabili) == "gBRCA")]

# elimino le colonne tutte uguali e equelle tutte NA
to.drop1 <- names(which(apply(dati_completi_stabili, 2, sd, na.rm=T) == 0))
dati_completi_stabili <- dati_completi_stabili[,-which(names(dati_completi_stabili) %in% to.drop1)]
dati_completi_stabili <- dati_completi_stabili[,colSums(is.na(dati_completi_stabili))<nrow(dati_completi_stabili)]

#rimuovo le righe NA
dati_completi_stabili <- dati_completi_stabili[rowSums(is.na(dati_completi_stabili))< ncol(dati_completi_stabili)-1,]

# metto la mediana al posto dei valori NaN
for(feat in names(dati_completi_stabili)){
  dati_completi_stabili[,feat][is.nan(dati_completi_stabili[,feat])]  <- median(dati_completi_stabili[,feat], na.rm=TRUE)
}

# feature selection based on pearson
library(caret)
DB_cor <- dati_completi_stabili[-1]
highCorr <- findCorrelation(cor(DB_cor), .9)
selected_feat <- names(DB_cor[-highCorr])

# feature selection based on wilcoxon
StatisticAnalisis <- dati_completi_stabili[,which(names(dati_completi_stabili) %in% selected_feat)]

Stat<-list()
pvalue<-list()

for (feature in names(StatisticAnalisis)[1:dim(StatisticAnalisis)[2]]) {
  cat(feature,"\n")
  if(sd(StatisticAnalisis[,feature])!=0){
    Stat[[feature]]<-list()  
    #1.Process 
    Stat[[feature]]$shap <- shapiro.test(x = StatisticAnalisis[,feature])
    if (Stat[[feature]]$shap$p.value < 0.05) {
      Stat[[feature]]$pvalue <- wilcox.test(x = StatisticAnalisis[which(StatisticAnalisis$gBRCA_bin == 0),feature], y = StatisticAnalisis[which(StatisticAnalisis$gBRCA_bin == 1), feature])      
    }
    else Stat[[feature]]$pvalue <-t.test(x = StatisticAnalisis[which(StatisticAnalisis$gBRCA_bin == 0),feature], y = StatisticAnalisis[which(StatisticAnalisis$gBRCA_bin == 1), feature])
    #6. Create a pvalue matrix
    pvalue[[feature]]<-list()
    pvalue[[feature]]<-Stat[[feature]]$pvalue
  }
}

tabella <- as.data.frame(matrix(data = NA ,nrow = length(pvalue),ncol = 2))
for(i in 1:length(pvalue)){
  tabella[i,1] <-names(pvalue)[i]
  tabella[i,2] <-pvalue[[i]]$p.value
}

tabella_sort <- tabella[
  order(tabella[,2] ,decreasing = F),
  ]

names(tabella_sort) <- c("var","p")
tabella_sort[
  order(tabella_sort[,2] ,decreasing = F),
  ]

tabella_sign <- tabella_sort[which(tabella_sort$p<=0.05),]