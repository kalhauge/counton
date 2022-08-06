#!/usr/bin/env python3
import pandas as pd
import numpy as np

df = pd.read_csv("output.csv", index_col="Name", usecols=["Name", "Mean", "Stddev"])
with open("benchmarks.md", "w") as md:
    df.sort_values(by=["Mean"], inplace=True)
    df.loc[:, "Mean"] *= 1000
    df.loc[:, "Stddev"] *= 1000
    df = df.rename(columns=lambda a: a + " [ms]").round(2)
    df.to_markdown(buf=md)
