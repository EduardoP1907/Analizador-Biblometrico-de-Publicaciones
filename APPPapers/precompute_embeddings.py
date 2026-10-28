#!/usr/bin/env python3
"""
Precompute multilingual embeddings for the bibliometric database.
Model: paraphrase-multilingual-MiniLM-L12-v2 (50+ languages, 384 dims,
input window of 128 tokens: each paper is split into passages, see prepare_chunks)

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


def _field(row, name):
    value = str(row.get(name, "") or "").strip()
    return "" if value.lower() == "nan" else value


def prepare_chunks(row, tokenizer, max_tokens):
    """Split a paper into passages that fit in the model's input window.

    The model truncates its input at max_seq_length tokens (128), so a single
    concatenation of title, abstract and keywords loses everything after the
    first ~128 tokens. Instead, the paper is split into passages:
      1. title + author keywords + index keywords + research area
      2..n. the full abstract, in consecutive windows of at most max_tokens
    Each passage is encoded separately and the paper embedding is the
    re-normalized mean of the passage embeddings (see paper_embeddings).
    """
    head = " | ".join(p for p in (_field(row, "TITULO"), _field(row, "AUTOR_PALABRAS_CLAVES"),
                                  _field(row, "INDEX_PALABRAS_CLAVES"), _field(row, "AREA_COMPUTACION")) if p)
    chunks = _windows(head, tokenizer, max_tokens) if head else []

    resumen = _field(row, "RESUMEN")
    if resumen:
        chunks.extend(_windows(resumen, tokenizer, max_tokens))

    return chunks if chunks else ["unknown paper"]


def _windows(text, tokenizer, max_tokens):
    """Cut text into consecutive pieces of at most max_tokens tokens."""
    ids = tokenizer(text, add_special_tokens=False)["input_ids"]
    if len(ids) <= max_tokens:
        return [text]
    return [tokenizer.decode(ids[i:i + max_tokens]) for i in range(0, len(ids), max_tokens)]


def paper_embeddings(model, chunks_per_paper, batch_size=64):
    """Encode every passage and average them per paper (L2-normalized)."""
    flat = [c for chunks in chunks_per_paper for c in chunks]
    owner = np.repeat(np.arange(len(chunks_per_paper)), [len(c) for c in chunks_per_paper])
    vecs = model.encode(flat, batch_size=batch_size, show_progress_bar=True,
                        convert_to_numpy=True, normalize_embeddings=True)
    out = np.zeros((len(chunks_per_paper), vecs.shape[1]), dtype=np.float32)
    np.add.at(out, owner, vecs)
    out /= np.linalg.norm(out, axis=1, keepdims=True)   # L2-normalize → cosine = dot product
    return out, len(flat)


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

    # ── 2. Load model ────────────────────────────────────────────
    print(f"\n[2/4] Loading model: {MODEL_NAME}")
    print("      (First run downloads ~90 MB — may take 1-2 min)")
    try:
        from sentence_transformers import SentenceTransformer
    except ImportError:
        print("\nERROR: sentence-transformers not installed.")
        print("Run:  pip install sentence-transformers")
        sys.exit(1)

    model = SentenceTransformer(MODEL_NAME)
    # 2 tokens of the window are taken by the special tokens <s> and </s>
    max_tokens = model.max_seq_length - 2
    print(f"      Model loaded OK | max_seq_length = {model.max_seq_length}")

    # ── 3. Prepare passages ──────────────────────────────────────
    print("\n[3/4] Splitting papers into passages...")
    chunks = [prepare_chunks(row, model.tokenizer, max_tokens) for _, row in df.iterrows()]
    n_chunks = [len(c) for c in chunks]
    print(f"      Done. Passages per paper: mean {np.mean(n_chunks):.1f}, max {max(n_chunks)}")
    print(f"      Sample: {chunks[0][0][:120]}...")

    # ── 4. Compute embeddings ────────────────────────────────────
    print(f"\n[4/4] Computing embeddings for {len(chunks):,} papers...")
    embeddings, total = paper_embeddings(model, chunks)
    print(f"      {total:,} passages encoded")

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
