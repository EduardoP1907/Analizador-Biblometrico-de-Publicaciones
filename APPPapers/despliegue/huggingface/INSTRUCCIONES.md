# Publicar APPPapers en Hugging Face Spaces (módulo semántico completo)

El Space construye la imagen del `Dockerfile` (R 4.5.1, Shiny y Python con
sentence-transformers para CPU) y publica la app en
`https://huggingface.co/spaces/<usuario>/APPPapers`. El plan gratuito da 2 CPU y
16 GB de RAM; el Space se suspende tras 48 h sin visitas y tarda unos minutos en
despertar.

## Antes de empezar

- **Datos:** `www/BD/BD_papers.csv` queda accesible para quien abra el Space
  público. Confirmar con el profesor guía que se puede publicar; si no, crear el
  Space como **privado** e invitar a los revisores.
- **Herramientas:** cuenta en huggingface.co, Git y Git LFS (`git lfs install`).

## Pasos

1. En huggingface.co → *New Space*: nombre `APPPapers`, SDK **Docker**
   (plantilla *Blank*), hardware *CPU basic*, visibilidad pública o privada.
2. Crear un token de escritura en *Settings → Access Tokens*.
3. Clonar el Space y copiar la aplicación (desde `APPPapers/`):

   ```bash
   git clone https://huggingface.co/spaces/<usuario>/APPPapers ../space_apppapers
   bash despliegue/huggingface/preparar_space.sh ../space_apppapers
   cd ../space_apppapers
   git lfs install
   git add .gitattributes && git add .
   git commit -m "Publica APPPapers"
   git push            # usuario de Hugging Face y el token como contraseña
   ```

4. En la pestaña *Logs* del Space se ve la construcción (unos 20 a 40 minutos la
   primera vez). Al terminar debe aparecer `✅ [EMBED] Motor activo`.
5. Anotar en el `README.md` del Space la URL del repositorio de GitHub y, en la
   memoria, la URL del Space y la etiqueta de la versión publicada.

## Probar la imagen en el computador (opcional)

Con Docker Desktop abierto, desde la carpeta preparada en el paso 3:

```bash
docker build -t apppapers .
docker run --rm -p 7860:7860 apppapers      # abrir http://localhost:7860
```

## Actualizar

Repetir `preparar_space.sh` sobre la misma carpeta, `git commit` y `git push`.
Si cambian los embeddings, regenerar antes `www/BD/paper_embeddings.npy` con
`precompute_embeddings.py`.
