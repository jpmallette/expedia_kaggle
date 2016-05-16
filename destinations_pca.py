import pandas as pd
from sklearn.decomposition import PCA

destinations = pd.read_csv('destinations.csv.gz')
pca = PCA(n_components=5)
dest_reduced = pca.fit_transform(destinations[["d{0}".format(i + 1) for i in range(149)]])
dest_reduced = pd.DataFrame(dest_reduced)
dest_reduced["srch_destination_id"] = destinations["srch_destination_id"]
dest_reduced.to_csv('destinations_reduced.csv')
