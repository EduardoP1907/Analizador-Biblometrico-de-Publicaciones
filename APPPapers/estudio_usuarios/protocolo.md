# Estudio de usabilidad del módulo de búsqueda (Chatbot NLP)

Estudio exploratorio corto para la Sección 6.4 de la memoria. Duración: unos 20 minutos por participante.

## Objetivo

Medir si estudiantes del DIINF pueden encontrar publicaciones del repositorio con el chat de APPPapers
(éxito y tiempo por tarea) y cómo perciben su usabilidad (cuestionario SUS; Brooke, 1996).

## Participantes

- 5 estudiantes de Ingeniería Informática (Ingeniería de Ejecución o Civil) que **no** hayan
  participado en el desarrollo. Con 5 participantes se detecta la mayoría de los problemas de usabilidad
  de una interfaz (Nielsen y Landauer, 1993), pero los resultados no se pueden generalizar.
- Registrar solo un código (P1…P5), la carrera y el año de ingreso. No registrar nombres en los resultados.

## Preparación

1. Iniciar la app en el equipo de pruebas (`shiny::runApp()` desde `APPPapers/`) y comprobar que la
   consola muestre los embeddings activos.
2. Abrir la pestaña **Chatbot NLP** con el chat vacío.
3. Tener a mano un cronómetro, `resultados_tareas.csv` y una copia impresa de `cuestionario_SUS.md`.

## Procedimiento (por participante)

1. Leer el consentimiento (abajo) y pedir conformidad verbal o firmada.
2. Explicar en una frase: «Es un chat para buscar publicaciones del repositorio del departamento;
   escribe como quieras». No mostrar ejemplos de consultas.
3. Entregar las tareas de a una. Iniciar el cronómetro al terminar de leer la tarea y detenerlo cuando la
   persona dé su respuesta o pasen 5 minutos. No ayudar; si pregunta, responder «haz lo que harías solo».
4. Anotar en `resultados_tareas.csv`: participante, tarea, éxito (1/0), tiempo en segundos, número de
   consultas escritas y observaciones (dudas, errores, comentarios en voz alta).
5. Al terminar las tres tareas, pedir que complete el cuestionario SUS y las dos preguntas abiertas.
   Copiar las respuestas en `resultados_sus.csv`.

## Tareas

Las respuestas se comprobaron en la app el 20-oct-2026 sobre las 3.596 publicaciones del corpus.

| Tarea | Enunciado para el participante | Criterio de éxito |
|---|---|---|
| T1 | Encuentra dos publicaciones sobre redes neuronales aplicadas a imágenes médicas y anota sus títulos. | Dos títulos que traten redes neuronales sobre imágenes médicas (p. ej., hemorragia intracraneal en TC, segmentación de lesiones cerebrales, clasificación de glóbulos blancos). |
| T2 | ¿Cuántas publicaciones tiene Pablo Pérez Lantero entre 2018 y 2023? | Responde 24. |
| T3 | Encuentra una publicación sobre la enseñanza de la programación a estudiantes de primer año y anota su título y año. | Un título que trate la enseñanza de la programación en cursos iniciales (CS1 o similar). |

## Consentimiento informado (leer al participante)

«Estás invitado a probar un buscador de publicaciones desarrollado para mi trabajo de titulación. Se
registrará si completas tres tareas, el tiempo que te toman y tus respuestas a un cuestionario; no se
registrará tu nombre. Tu participación es voluntaria y puedes detenerte en cualquier momento sin dar
razones. Lo que se evalúa es el sistema, no a ti. Los resultados se publicarán solo de forma agregada en
la memoria. ¿Aceptas participar?»

## Análisis

```
Rscript estudio_usuarios/analizar_estudio.R estudio_usuarios
```

Calcula la tasa de éxito y la mediana de tiempo por tarea, el puntaje SUS de cada participante y su
promedio, y escribe un borrador del texto de la Sección 6.4 con esos números (`resultados_6_4.txt`).
Guardar el consentimiento y las hojas originales como respaldo: la guía de la biblioteca permite
incluirlos como apéndice.
