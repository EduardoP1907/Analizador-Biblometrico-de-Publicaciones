#=======================================
# Imagen de APPPapers con el módulo de recuperación semántica completo
# (R + Shiny + Python con sentence-transformers) para un Space Docker de
# Hugging Face. El Space expone el puerto 7860 (ver README.md).
#=======================================
FROM rocker/r-ver:4.5.1

# Bibliotecas del sistema que necesitan los paquetes de R y Python
RUN apt-get update && apt-get install -y --no-install-recommends \
        python3 python3-venv python3-pip \
        libcurl4-openssl-dev libssl-dev libxml2-dev libglpk-dev libicu-dev \
        libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
        libpng-dev libjpeg-dev libtiff5-dev zlib1g-dev git \
    && rm -rf /var/lib/apt/lists/*

# Paquetes de R (lista de entorno_R.R, sin los de pruebas)
RUN install2.r --error --skipinstalled --ncpus -1 \
        dplyr DT ggforce ggplot2 ggthemes ggwordcloud hrbrthemes htmlwidgets \
        igraph networkD3 plotly pracma randomForest readr RColorBrewer shiny \
        shinyBS shinybusy shinyalert shinyjs shinyWidgets stringr tibble tidyr \
        tidyverse tm threejs visNetwork viridis wordcloud wordcloud2 writexl \
        shinyFeedback fontawesome stringi stringdist textrank markdown \
        reticulate httr jsonlite lexRankr shinycssloaders htmltools remotes \
    && R -q -e 'remotes::install_github("davidsjoberg/ggsankey", upgrade = "never")' \
    && rm -rf /tmp/downloaded_packages

# Python: entorno virtual con torch para CPU (evita las dependencias de CUDA)
COPY requirements.txt /tmp/requirements.txt
RUN python3 -m venv /opt/venv \
    && /opt/venv/bin/pip install --no-cache-dir --upgrade pip \
    && /opt/venv/bin/pip install --no-cache-dir torch==2.12.0 \
        --index-url https://download.pytorch.org/whl/cpu \
    && /opt/venv/bin/pip install --no-cache-dir -r /tmp/requirements.txt

# Modelo de embeddings descargado durante la construcción, para que el
# primer usuario no espere la descarga
ENV APPPAPERS_PYTHON=/opt/venv/bin/python \
    RETICULATE_PYTHON=/opt/venv/bin/python \
    HF_HOME=/opt/hf
RUN /opt/venv/bin/python -c "from sentence_transformers import SentenceTransformer; SentenceTransformer('paraphrase-multilingual-MiniLM-L12-v2')" \
    && chmod -R a+rX /opt/hf

# Aplicación (Hugging Face ejecuta el contenedor con el usuario 1000)
RUN useradd -m -u 1000 user
COPY --chown=user . /app
WORKDIR /app
USER user

EXPOSE 7860
CMD ["R", "-q", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 7860, launch.browser = FALSE)"]
