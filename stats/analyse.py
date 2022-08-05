import pandas as pd

df = pd.read_csv("output.csv", index_col=0)
with open("benchmarks.md", "w") as md:
    df.sort_values(by=["Mean"], inplace=True)
    df.loc[:, "Mean"] *= 1000
    df.loc[:, "Stddev"] *= 1000
    df = df[["Mean", "Stddev"]].rename(columns=lambda a: a + " [ms]").round(2)
    df.to_markdown(buf=md)
