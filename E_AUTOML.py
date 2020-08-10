m tpot import TPOTClassifier
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd

from os import chdir, getcwd
wd=getcwd()
chdir(wd)

#CHANGE ACORDINGLY TO THE TYPE OF US MACHINE DATASET

general_df = pd.read_csv("train_voluson.csv")
del general_df['Unnamed: 0']

y_train = general_df['gBRCA_bin']
X_train = general_df.iloc[:, :-1]

general_df_test = pd.read_csv("test_voluson.csv")
del general_df_test['Unnamed: 0']

y_test = general_df_test['gBRCA_bin']
X_test = general_df_test.iloc[:, :-1]

tpot = TPOTClassifier(verbosity=2, n_jobs=30, population_size = 100)
tpot.fit(X_train, y_train)
print(tpot.score(X_test, y_test))
tpot.export('voluson_patientsplit.py')

