from sklearn.datasets import fetch_20newsgroups
from umap import UMAP
from bertopic import BERTopic
import pandas as pd

docs = fetch_20newsgroups(subset='all',  remove=('headers', 'footers', 'quotes'))['data']

umap_model_1 = UMAP(n_neighbors=15, n_components=5, min_dist=0.0, metric='cosine', random_state=1)
umap_model_2 = UMAP(n_neighbors=15, n_components=5, min_dist=0.0, metric='cosine', random_state=2)
umap_model_3 = UMAP(n_neighbors=15, n_components=5, min_dist=0.0, metric='cosine', random_state=3)
umap_model_4 = UMAP(n_neighbors=15, n_components=5, min_dist=0.0, metric='cosine', random_state=4)
umap_model_5 = UMAP(n_neighbors=15, n_components=5, min_dist=0.0, metric='cosine', random_state=5)

topic_model_1 = BERTopic(umap_model=umap_model_1)
topic_model_2 = BERTopic(umap_model=umap_model_2)
topic_model_3 = BERTopic(umap_model=umap_model_3)
topic_model_4 = BERTopic(umap_model=umap_model_4)
topic_model_5 = BERTopic(umap_model=umap_model_5)

topics_1, probs_1 = topic_model_1.fit_transform(docs)
topics_2, probs_2 = topic_model_2.fit_transform(docs)
topics_3, probs_3 = topic_model_3.fit_transform(docs)
topics_4, probs_4 = topic_model_4.fit_transform(docs)
topics_5, probs_5 = topic_model_5.fit_transform(docs)

data = {"model1": topic_model_1.get_topic_info().iloc[1:,3],
	"model2": topic_model_2.get_topic_info().iloc[1:,3],
	"model3": topic_model_3.get_topic_info().iloc[1:,3],
	"model4": topic_model_4.get_topic_info().iloc[1:,3],
	"model5": topic_model_5.get_topic_info().iloc[1:,3]}
df = pd.DataFrame(data=data)

df.to_csv("topics-seed.csv", index=False)
