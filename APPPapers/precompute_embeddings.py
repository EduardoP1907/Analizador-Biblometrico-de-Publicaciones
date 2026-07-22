#!/usr/bin/env python3
"""
Precompute multilingual embeddings for the bibliometric database.
Model: paraphrase-multilingual-MiniLM-L12-v2 (50+ languages, 384 dims)

Run ONCE (or when CSV data changes):
    python precompute_embeddings.py

Output:
    www/BD/paper_embeddings.npy  - embedding matrix (n_papers x 384)
    www/BD/paper_ids.npy         - row indices aligned with CSV
"""

import sys
import os
import numpy as np
import pandas as pd
from pathlib import Path

BASE_DIR = Path(__file__).parent
CSV_PATH = BASE_DIR / "www" / "BD" / "BD_papers.csv"
EMBEDDINGS_PATH = BASE_DIR / "www" / "BD" / "paper_embeddings.npy"
IDS_PATH = BASE_DIR / "www" / "BD" / "paper_ids.npy"
MODEL_NAME = "paraphrase-multilingual-MiniLM-L12-v2"


def prepare_text(row):
    """Concatenate relevant fields for a paper into a single searchable string."""
    parts = []

    # Title (most discriminative field)
    titulo = str(row.get("TITULO", "") or "").strip()
    if titulo:
        parts.append(titulo)

    # Abstract (truncated to avoid token limits)
    resumen = str(row.get("RESUMEN", "") or "").strip()
    if resumen:
        parts.append(resumen[:600])

    # Author keywords (high signal)
    kw_autor = str(row.get("AUTOR_PALABRAS_CLAVES", "") or "").strip()
    if kw_autor:
        parts.append(kw_autor[:300])

    # Index keywords
    kw_index = str(row.get("INDEX_PALABRAS_CLAVES", "") or "").strip()
    if kw_index:
        parts.append(kw_index[:200])

    # Research area (Spanish label — helps multilingual matching)
    area = str(row.get("AREA_COMPUTACION", "") or "").strip()
    if area:
        parts.append(area)

    return " | ".join(parts) if parts else "unknown paper"


def main():
    print("=" * 60)
    print("APPPapers — Precompute Multilingual Embeddings")
    print("=" * 60)

    # ── 1. Verify CSV ────────────────────────────────────────────
    if not CSV_PATH.exists():
        print(f"ERROR: CSV not found at {CSV_PATH}")
        sys.exit(1)

    print(f"\n[1/4] Reading: {CSV_PATH}")
    df = pd.read_csv(CSV_PATH, sep="|", encoding="utf-8", low_memory=False)
    print(f"      {len(df):,} papers loaded | {len(df.columns)} columns")

    # ── 2. Prepare texts ─────────────────────────────────────────
    print("\n[2/4] Preparing paper texts...")
    texts = [prepare_text(row) for _, row in df.iterrows()]
    avg_len = sum(len(t) for t in texts) / len(texts)
    print(f"      Done. Avg text length: {avg_len:.0f} chars")
    print(f"      Sample: {texts[0][:120]}...")

    # ── 3. Load model ────────────────────────────────────────────
    print(f"\n[3/4] Loading model: {MODEL_NAME}")
    print("      (First run downloads ~90 MB — may take 1-2 min)")
    try:
        from sentence_transformers import SentenceTransformer
    except ImportError:
        print("\nERROR: sentence-transformers not installed.")
        print("Run:  pip install sentence-transformers")
        sys.exit(1)

    model = SentenceTransformer(MODEL_NAME)
    print("      Model loaded OK")

    # ── 4. Compute embeddings ────────────────────────────────────
    print(f"\n[4/4] Computing embeddings for {len(texts):,} papers...")
    print("      Batch size: 64 | Estimated time: 1-3 min on CPU")

    embeddings = model.encode(
        texts,
        batch_size=64,
        show_progress_bar=True,
        convert_to_numpy=True,
        normalize_embeddings=True,   # L2-normalize → cosine = dot product
    )

    print(f"\n      Shape: {embeddings.shape} | dtype: {embeddings.dtype}")
    print(f"      Size:  {embeddings.nbytes / 1e6:.1f} MB")

    # ── 5. Save ──────────────────────────────────────────────────
    np.save(str(EMBEDDINGS_PATH), embeddings)

    # Save title index so R can match by TITULO (not fragile row index)
    titulos = df["TITULO"].fillna("").astype(str).tolist()
    import json
    with open(str(BASE_DIR / "www" / "BD" / "paper_titulo_index.json"), "w", encoding="utf-8") as f:
        json.dump(titulos, f, ensure_ascii=False)

    print(f"\n  Embeddings   -> {EMBEDDINGS_PATH}")
    print(f"  Titulo index -> www/BD/paper_titulo_index.json")

    # Verify round-trip
    loaded = np.load(str(EMBEDDINGS_PATH))
    assert loaded.shape == embeddings.shape, "Save/load mismatch!"
    print("\nVerification: OK")
    print("\n" + "=" * 60)
    print("Done! Start the Shiny app — embedding search is ready.")
    print("=" * 60)


if __name__ == "__main__":
    main()
