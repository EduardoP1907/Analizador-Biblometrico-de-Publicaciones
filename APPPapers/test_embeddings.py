import numpy as np
import pandas as pd
from sentence_transformers import SentenceTransformer

emb   = np.load("www/BD/paper_embeddings.npy")
df    = pd.read_csv("www/BD/BD_papers.csv", sep="|", low_memory=False)
model = SentenceTransformer("paraphrase-multilingual-MiniLM-L12-v2")

queries = [
    "dame papers sobre matematicas",
    "machine learning en salud",
    "redes neuronales para vision computacional",
    "educacion ingenieria programacion",
    "papers de Garcia",
]

for q in queries:
    qv   = model.encode([q], normalize_embeddings=True)[0]
    sims = emb @ qv
    top3 = sims.argsort()[::-1][:3]
    print(f"\nQuery: {q}")
    for rank, idx in enumerate(top3, 1):
        titulo = str(df.iloc[idx]["TITULO"])[:75]
        autor  = str(df.iloc[idx]["NOMBRE_AUTOR"])[:30]
        print(f"  [{rank}] sim={sims[idx]:.3f} | {titulo}")
        print(f"       autor={autor}")
