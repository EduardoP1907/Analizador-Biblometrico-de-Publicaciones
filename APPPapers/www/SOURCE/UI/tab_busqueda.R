# www/SOURCE/UI/tab_busqueda.R

library(shiny)
library(fontawesome)
library(DT)
library(shinycssloaders)    # Para el spinner de carga

# Pestaña de búsqueda con spinner integrado
tab_busqueda <- tabPanel(
  title = tagList(fa_i("search"), "Búsqueda"),
  fluidRow(
    column(10,
      textInput(
        inputId    = "buscador-query",
        label      = NULL,
        placeholder= "Autor, palabra clave o año…"
      )
    ),
    column(2,
      actionButton(
        inputId = "buscador-go",
        label   = tagList(fa_i("magnifying-glass"), "Buscar"),
        class   = "btn-primary",
        width   = "100%"
      )
    )
  ),
  hr(),

  # Aquí se mostrará el “chat” y un spinner mientras se procesa la llamada
  withSpinner(
    uiOutput("buscador-chat"),
    type = 6
  )
)
