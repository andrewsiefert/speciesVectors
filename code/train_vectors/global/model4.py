exec(open("projects/speciesVectors/code/train_glove.py").read())


cooccur = pd.read_csv("projects/speciesVectors/data/sPlot_cooccur_counts.csv")

params = pd.read_csv("projects/speciesVectors/code/train_vectors/global/grid.csv")

m = 4
pars = params.loc[params.model==m][['dim', 'xmax']]
out = train_glove(cooccur, **pars)


# save model
out['model'].save("../../gscratch/asiefer1/projects/speciesVectors/global/model"+str(m))

# save history
out['history'].to_csv("../../gscratch/asiefer1/projects/speciesVectors/global/model"+str(m)+"_history.csv", index=False)

# save embedding
out['embedding'].to_csv("../../gscratch/asiefer1/projects/speciesVectors/global/model"+str(m)+"_embedding.csv", index=False)

