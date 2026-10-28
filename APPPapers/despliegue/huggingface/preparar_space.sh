#!/usr/bin/env bash
#=======================================
# Copia en la carpeta del Space de Hugging Face los archivos que la
# aplicación necesita para funcionar (sin la memoria, pruebas ni respaldos).
# Uso (desde APPPapers/):  bash despliegue/huggingface/preparar_space.sh <carpeta del Space>
#=======================================
set -euo pipefail

DESTINO="${1:?Indique la carpeta del Space clonado}"
ORIGEN="$(cd "$(dirname "$0")/../.." && pwd)"

mkdir -p "$DESTINO/www/BD"
cp "$ORIGEN/despliegue/huggingface/Dockerfile" "$ORIGEN/despliegue/huggingface/README.md" "$DESTINO/"
cp "$ORIGEN/server.R" "$ORIGEN/ui.R" "$ORIGEN/requirements.txt" "$DESTINO/"
cp "$ORIGEN/www/styles.css" "$DESTINO/www/"
cp -r "$ORIGEN/www/SOURCE" "$ORIGEN/www/IMG" "$DESTINO/www/"
for f in BD_papers.csv paper_embeddings.npy paper_titulo_index.json ground_truth_evaluacion.csv; do
  cp "$ORIGEN/www/BD/$f" "$DESTINO/www/BD/"
done

# Los archivos de más de 10 MB deben subirse con Git LFS
cat > "$DESTINO/.gitattributes" <<'EOF'
*.csv filter=lfs diff=lfs merge=lfs -text
*.npy filter=lfs diff=lfs merge=lfs -text
EOF

echo "Listo: $DESTINO"
