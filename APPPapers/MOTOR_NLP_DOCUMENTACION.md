# 🤖 Motor de Procesamiento de Lenguaje Natural - Chatbot Bibliométrico

## Descripción General

Se ha implementado un motor PLN avanzado tipo chatbot que permite realizar consultas inteligentes sobre papers académicos. El sistema procesa consultas en lenguaje natural, busca papers relevantes, analiza sus abstracts en inglés y genera resúmenes completamente nuevos en español.

## 🚀 Características Principales

### 1. **Búsqueda Semántica Inteligente**
- Análisis de consultas en múltiples idiomas (español/inglés)
- Extracción automática de términos clave
- Búsqueda ponderada por relevancia (título, palabras clave, resumen)
- Coincidencia exacta, por términos y aproximada
- Ranking por calidad académica (SJR, citas)

### 2. **Procesamiento PLN Avanzado**
- Análisis de abstracts en inglés
- Generación de resúmenes completamente nuevos (no extractivos)
- Integración opcional con OpenAI GPT para mayor precisión
- Procesamiento local como fallback
- Identificación automática de temas y metodologías

### 3. **Traducción Automática**
- Traducción de resúmenes del inglés al español
- Múltiples estrategias: Google Translate API, diccionario técnico
- Traducción especializada en términos académicos y técnicos
- Sistema de fallback robusto

### 4. **Interfaz Chatbot Moderna**
- Diseño conversacional intuitivo
- Historial de conversaciones
- Estadísticas en tiempo real
- Exportación de conversaciones
- Interfaz responsive y atractiva

## 📋 Instalación y Configuración

### Paso 1: Instalar Dependencias
```r
# Ejecutar el script de instalación
source("instalar_dependencias_nlp.R")
```

### Paso 2: Configuración Opcional
```r
# Para mejorar la calidad de los resúmenes, configure una clave OpenAI
# En la interfaz del chatbot, ingrese su clave en el campo correspondiente
# Formato: sk-proj-xxxxxxxxxxxxxxx...
```

### Paso 3: Verificar Instalación
```r
# Cargar bibliotecas principales
source("www/SOURCE/UTILS/cargar_bibliotecas.R")

# Verificar motor PLN
source("www/SOURCE/UTILS/nlp_chatbot_engine.R")
```

## 💡 Ejemplos de Uso

### Consultas Recomendadas

**Área de Computación:**
- `"machine learning"`
- `"redes neuronales artificiales"`
- `"algoritmos genéticos optimización"`
- `"inteligencia artificial medicina"`
- `"procesamiento de imágenes deep learning"`

**Área Biomédica:**
- `"análisis genómico bioinformática"`
- `"machine learning diagnóstico médico"`
- `"redes neuronales imagen médica"`

**Consultas Complejas:**
- `"análisis de sentimientos en redes sociales usando deep learning"`
- `"optimización de algoritmos genéticos para problemas de scheduling"`

### Resultado Esperado

Para la consulta `"machine learning"`, el sistema:

1. **Encuentra papers relevantes** (ej: 8 papers)
2. **Analiza abstracts** en inglés de todos los papers
3. **Genera resumen nuevo** como:
   > "Se encontraron 8 papers relacionados con machine learning, dentro de los cuales se encuentran papers relacionados al aprendizaje automático y las matemáticas, ML aplicado en bioquímica y medicina. Los estudios abarcan metodologías de redes neuronales, algoritmos de clasificación supervisada y técnicas de optimización. Las aplicaciones principales incluyen diagnóstico médico, análisis de imágenes y predicción de patrones."

4. **Muestra lista de papers** con títulos, autores, años y enlaces a Scopus

## 🛠️ Arquitectura Técnica

### Archivos Principales

```
www/SOURCE/UTILS/nlp_chatbot_engine.R     # Motor PLN principal
www/SOURCE/UI/tab_busqueda.R              # Interfaz del chatbot  
www/SOURCE/SERVER/server_tab_busqueda.R   # Lógica del servidor
instalar_dependencias_nlp.R               # Script de instalación
```

### Flujo de Procesamiento

1. **Entrada:** Usuario ingresa consulta
2. **Preprocesamiento:** Limpieza y extracción de términos clave
3. **Búsqueda:** Múltiples estrategias de búsqueda semántica
4. **Ranking:** Ordenamiento por relevancia y calidad académica
5. **Análisis NLP:** Procesamiento de abstracts encontrados
6. **Generación:** Creación de resumen nuevo usando GPT o técnicas locales
7. **Traducción:** Conversión del inglés al español
8. **Presentación:** Formato visual en interfaz de chat

## 🔧 Configuraciones Avanzadas

### Con Clave OpenAI (Recomendado)
- **Ventaja:** Resúmenes más precisos y coherentes
- **Costo:** Aproximadamente $0.002 por consulta
- **Configuración:** Ingrese clave en la interfaz

### Sin Clave OpenAI (Funcional)
- **Ventaja:** Completamente gratuito y offline
- **Limitación:** Resúmenes más básicos
- **Funcionamiento:** Usa técnicas de NLP local

### Optimización de Rendimiento
```r
# Configuraciones recomendadas en .Rprofile
options(timeout = 300)
options(encoding = "UTF-8") 
options(repos = c(CRAN = "https://cloud.r-project.org/"))
```

## 📊 Métricas y Estadísticas

El sistema proporciona:
- **Número de papers encontrados**
- **Términos clave identificados** 
- **Tiempo de procesamiento**
- **Calidad de papers** (SJR, citas)
- **Historial de conversaciones**

## 🚨 Solución de Problemas

### Error: "Paquete no encontrado"
```r
# Reinstalar dependencias
source("instalar_dependencias_nlp.R")
```

### Error: "No se encontraron papers"
- Intente términos más generales
- Verifique la ortografía
- Use términos en inglés si es necesario

### Error: "Traducción fallida"
- El sistema usa múltiples fallbacks
- Resultado mínimo garantizado en español

### Rendimiento Lento
- Limite consultas muy amplias
- Use clave OpenAI para procesamiento más eficiente
- Verifique conexión a internet

## 🔄 Futuras Mejoras

### Versión 2.0 Planificada
- [ ] Soporte para múltiples idiomas de entrada
- [ ] Análisis de tendencias temporales
- [ ] Clustering automático de papers
- [ ] Recomendaciones personalizadas
- [ ] API REST independiente
- [ ] Integración con más bases de datos académicas

### Contribuciones
El código está modularizado para facilitar extensiones:
- Nuevos algoritmos de búsqueda en `buscar_papers_semantico()`
- Mejores traducctores en `traducir_a_espanol()`  
- Análisis más profundos en `procesar_abstracts()`

## 📈 Casos de Uso Avanzados

### Para Investigadores
- Exploración rápida de literatura
- Identificación de gaps de investigación
- Análisis de tendencias en áreas específicas

### Para Estudiantes
- Búsqueda de papers para tesis
- Comprensión de conceptos complejos
- Identificación de metodologías relevantes

### Para Administradores Académicos
- Análisis de producción científica institucional
- Identificación de fortalezas de investigación
- Evaluación de impacto académico

---

*Documentación actualizada: Agosto 2025*
*Versión del Motor NLP: 1.0.0*