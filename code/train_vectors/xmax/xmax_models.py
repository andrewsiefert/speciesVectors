exec(open("code/train_glove.py").read())


cooccur = pd.read_csv("data/sPlot_cooccur_counts.csv")
params = pd.read_csv("code/train_vectors/xmax/grid.csv")

m = 1

pars = params.loc[params.model==m][['dim', 'xmax']]
print(pars)

out = train_glove(cooccur, **pars, vb=1)

# save history
out['history'].to_csv("vectors/xmax/model"+str(m)+"_history.csv", index=False)

# save embedding
out['embedding'].to_csv("vectors/xmax/model"+str(m)+"_embedding.csv", index=False)
