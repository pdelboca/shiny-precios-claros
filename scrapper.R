library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(readr)
source("data/get_data.R")

scrapear_productos <- function() {
  id_sucursales <- get_sucursales()
  id_productos <- get_productos()
  url <- paste0(
    "https://d3e6htiiul5ek9.cloudfront.net/prod/comparativa?array_sucursales=",
    paste(id_sucursales, collapse = ","),
    "&array_productos=",
    paste(id_productos, collapse = ","),
    collapse = ""
  )
  json_response <- content(GET(url))
  
  lista_precios <- expand.grid(
    fecha = Sys.Date(),
    sucursal = id_sucursales,
    producto = id_productos,
    stringsAsFactors = FALSE
  )
  
  sucursales <- json_response$sucursales
  for (sucursal in sucursales) {
    this_sucursal <- sucursal$id
    message(this_sucursal)
    for (producto in sucursal$productos) {
      this_producto <- producto$id
      message(this_producto)
      if (!is.null(producto$precioLista)) {
        lista_precios[lista_precios$sucursal == this_sucursal &
                        lista_precios$producto == this_producto, "precio_lista"] <-
          producto$precioLista
      }
    }
  }
  
  lista_final <- lista_precios %>%
    left_join(read_csv("data/sucursales.csv")) %>%
    left_join(read_csv(
      "data/productos.csv",
      col_types = cols(producto = col_character(),
                       nombre_producto = col_character())
    ))
  
  return(lista_final)
}