library(readr)
PROBE_PATIENTS_CS <- read_csv("PROBE_PATIENTS_CS.csv")

split_train = 0.75
smp_size <- floor(split_train * nrow(PROBE_PATIENTS_CS))

set.seed(123)
train_ind <- sample(seq_len(nrow(PROBE_PATIENTS_CS)), size = smp_size)

train <- PROBE_PATIENTS_CS[train_ind, ]
test <- PROBE_PATIENTS_CS[-train_ind, ]

#this has to be zero to have a per-patient split between train and test
length(which(train$CS %in% test$CS))
