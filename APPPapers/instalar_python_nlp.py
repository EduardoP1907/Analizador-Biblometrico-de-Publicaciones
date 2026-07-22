#!/usr/bin/env python3
"""
Instala las dependencias Python necesarias para el motor de embeddings.
Ejecutar con el Python de Miniconda:
    C:\\Users\\eduar\\miniconda3\\python.exe instalar_python_nlp.py
"""
import subprocess, sys

paquetes = [
    "sentence-transformers",
    "torch",            # requerido por sentence-transformers
]

print("Instalando dependencias para el motor NLP de embeddings...\n")
for pkg in paquetes:
    print(f"  Instalando: {pkg}")
    resultado = subprocess.run(
        [sys.executable, "-m", "pip", "install", pkg, "--quiet"],
        capture_output=True, text=True
    )
    if resultado.returncode == 0:
        print(f"  OK: {pkg}")
    else:
        print(f"  ERROR: {pkg}\n{resultado.stderr}")

print("\nVerificando instalacion...")
try:
    from sentence_transformers import SentenceTransformer
    print("sentence-transformers: OK")
except ImportError as e:
    print(f"sentence-transformers: FALLO — {e}")

print("\nListo. Ahora ejecutar:")
print("  C:\\Users\\eduar\\miniconda3\\python.exe precompute_embeddings.py")
