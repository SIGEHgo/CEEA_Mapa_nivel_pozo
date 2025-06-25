datos = readxl::read_xlsx("app/assets/Datos/Dosidicadores CEAA 2020-2023.xlsx")

datos = datos[!is.na(datos$...3),]
colnames(datos) = datos[1,]
datos = datos[-c(1,nrow(datos)), -1]

datos = datos |> dplyr::mutate(`Gasto de agua` = sub(x = `Gasto de agua`,pattern = "l/s", replacement = ""),
                               `Gasto de agua` = as.numeric(`Gasto de agua`))
datos = datos |> 
  dplyr::mutate(longitud = sub(x = coordenadas, pattern = ".*?,", replacement = ""),
                latitud = sub(x = coordenadas, pattern = ",.*", replacement = ""),
                longitud = as.numeric(longitud),
                latitud = as.numeric(latitud)) |>
  dplyr::select(-coordenadas)

datos = datos |> tidyr::fill(Municipio, .direction = "down") |> # Rellena valores NA usando valores no NA anteriores ("down") o posteriores ("up").





mun = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")

datos = sf::st_as_sf(x = datos, coords = c("longitud", "latitud"), crs = sf::st_crs(mun) )

plot(mun$geometry)
plot(datos$geometry, col = "red", add = T)

library(leaflet)

mapa_web = leaflet() |>
  addTiles() |>
  addMarkers(data = datos, label = datos$Locacion,
                   popup = paste("Municipio:", "<b>", datos$Municipio,"</b>",
                                 "<br>", "Locacion:", "<b>", datos$Locacion, "</b>",
                                 "<br>", "Año:", "<b>", datos$Año, "</b>",
                                 "<br>", "Estado:", "<b>", datos$estado, "</b>",
                                 "<br>", "Gasto de agua:", "<b>", datos$`Gasto de agua`, "l/s", "</b>",
                                 "<br>", "Marca:", "<b>", datos$Marca, "</b>",
                                 "<br>", "Modelo:", "<b>", datos$Modelo, "</b>"
                                 ) |> lapply(FUN = function(x) { htmltools::HTML(x)})
                   )
mapa_web


p = sf::read_sf("app/assets/Datos/shp/Dosidicadores.shp")


sf::write_sf(datos,"app/assets/Datos/shp/Dosidicadores.shp")
