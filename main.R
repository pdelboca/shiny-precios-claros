library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(readr)
source("data/get_data.R")

url <- paste0(
  "https://d3e6htiiul5ek9.cloudfront.net/prod/comparativa?array_sucursales=",
  paste(id_sucursales,collapse = ","),
  "&array_productos=",
  paste(id_productos,collapse = ","), 
  collapse = ""
)
a <- content(GET(url))

lista_precios <- expand.grid(fecha = Sys.Date(),
                             sucursal = id_sucursales, 
                             producto = id_productos,
                             stringsAsFactors = FALSE)

sucursales <- a$sucursales
for (sucursal in sucursales) {
  this_sucursal <- sucursal$id
  message(this_sucursal)
  for(producto in sucursal$productos){
    this_producto <- producto$id
    message(this_producto)
    if(!is.null(producto$precioLista)){
      lista_precios[lista_precios$sucursal == this_sucursal & lista_precios$producto == this_producto, "precio_lista"] <- producto$precioLista
    }
  }
}

lista_final <- lista_precios %>%
  left_join(read_csv("data/sucursales.csv")) %>%
  left_join(
    read_csv("data/productos.csv",
    col_types = cols(producto = col_character(),
                     nombre_producto = col_character())))

tabla_comparativa <- lista_final %>%
  mutate(nombre_sucursal = paste(bandera_descripcion, "-", nombre_sucursal)) %>% 
  select(fecha, producto, nombre_producto, nombre_sucursal, precio_lista) %>%
  spread(nombre_sucursal,precio_lista, fill = 0)


write_excel_csv(lista_final, paste0("./data/entregables/", format(Sys.Date(), format="%Y%m%d"),"-","precios.csv"))
write_excel_csv(tabla_comparativa, paste0("./data/entregables/", format(Sys.Date(), format="%Y%m%d"),"-","comparativa.csv"))

