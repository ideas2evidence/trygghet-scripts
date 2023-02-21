
import pandas as pd

with open("path.txt", "r") as file:
    path = file.read().replace("\n", "")

pd.read_spss(path + "Norwegian Crime Survey NSD v1.sav")

