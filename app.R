library(shiny)
library(readr)
source("scrapper.R")
precios <- scrapear_productos()

ui <- fluidPage(
  titlePanel("Relevamiento Precios Claros"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(h4("Descargar Datos:")),
      fluidRow(
        downloadButton("descargarLista", "Datos Crudos")
    ),
      fluidRow(
        downloadButton("descargarTabla", "Tabla Comparativa")
    ),
    br(),
    em("Creado por @pdelboca y Open Data Córdoba."),
    width = 2
    ),
    mainPanel(
      tableOutput("tablaPrecios"), 
      width = 10
      )
  )
)

server <- function(input, output) {
  output$tablaPrecios <- renderTable({
    precios %>%
      mutate(nombre_sucursal = paste(bandera_descripcion, "-", nombre_sucursal)) %>%
      select(fecha,
             producto,
             nombre_producto,
             nombre_sucursal,
             precio_lista) %>%
      mutate(
        precio_lista = ifelse(is.na(precio_lista), "$0.00", scales::dollar(precio_lista)),
        fecha = as.character(fecha)
      ) %>%
      spread(nombre_sucursal, precio_lista, fill = 0)
  }, striped = TRUE,bordered = TRUE, spacing = "xs")
  
  output$descargarLista <- downloadHandler(
    filename = function() {
      paste0(format(Sys.Date(), format = "%Y%m%d"), "-", "precios.csv")
    },
    content = function(file) {
      write_csv(precios, file)
    }
  )
  
  output$descargarTabla <- downloadHandler(
    filename = function() {
      paste0(format(Sys.Date(), format = "%Y%m%d"),
             "-",
             "comparativa.csv")
    },
    content = function(file) {
      precios %>%
        mutate(nombre_sucursal = paste(bandera_descripcion, "-", nombre_sucursal)) %>%
        select(fecha,
               producto,
               nombre_producto,
               nombre_sucursal,
               precio_lista) %>%
        spread(nombre_sucursal, precio_lista, fill = 0) %>%
        write_csv(file)
    }
  )
}

shinyApp(ui = ui, server = server)
