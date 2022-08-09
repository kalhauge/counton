#!/usr/bin/env python3
import pandas as pd
import numpy as np

df = pd.read_csv("output.csv", index_col="Name", usecols=["Name", "Mean", "Stddev"])
with open("benchmarks.md", "w") as md:
    df.sort_values(by=["Mean"], inplace=True)
    df.loc[:, "Mean"] *= 1000
    df.loc[:, "Stddev"] *= 1000
    df = df.rename(columns=lambda a: a + " [ms]").round(2)

    for x in ["numbers", "kjvbible", "10xkjvbible"]:
        print(f"### {x}\n", file=md)
        df.loc[[i for i in df.index if i.startswith(x)]].to_markdown(buf=md)
        print("\n", file=md)

    print(f"### all\n", file=md)
    df.to_markdown(buf=md)
    print("\n", file=md)
