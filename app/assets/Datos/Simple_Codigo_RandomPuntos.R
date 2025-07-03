datos_prueba = sf::read_sf("app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_prueba.shp")
datos_faltantes = sf::read_sf("app/assets/Datos/Datos_2012_2023_shp/faltantes.shp")

datos = rbind(datos_prueba, datos_faltantes)
datos = datos |> 
  dplyr::arrange(AÑO, NOM_MUN, NOMGEO_LOC) |> 
  dplyr::select(-LOC)

sf::write_sf(datos, "app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_poligonos.shp")
sf::write_sf(datos,"Js/Datos/Datos_2012_2023_poligonos.shp")


datos = sf::read_sf("app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_poligonos.shp")
datos = datos |>  dplyr::mutate(ID = paste0(CVEGEO_LOC, "_", NOMGEO_LOC, "_", f_abast))

datos_random = sf::read_sf("app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_puntos_aleatorios_unicos.shp")
datos_random = datos_random |> 
  dplyr::mutate(ID = paste0(CVEGEO_LOC, "_", NOMGEO_LOC, "_", f_abast))

datos_mapa = merge(x = datos |>  sf::st_drop_geometry(), y = datos_random |> dplyr::select(ID, geometry), by = "ID", all.x = T)
datos_mapa = datos_mapa |>  dplyr::select(-ID,) |>  sf::st_as_sf(crs = sf::st_crs(datos_random)) 


sf::write_sf(datos_mapa, "app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_RandomPuntos.shp")
sf::write_sf(datos_mapa, "Js/Datos/Datos_2012_2023_RandomPuntos.shp")

as.Date(x = 2024)



##################

datos = sf::read_sf("Js/Datos/Datos_2012_2023_poligonos.shp")


fechas = function(str) {
  if (str %in% 2012:2023) {
    return(as.Date(paste0("01/01/", str), format = "%d/%m/%Y"))
  } else {
    return(NA)
  }
}

datos = datos |>
  dplyr::mutate(Fecha = sapply(AÑO, FUN = fechas, simplify = T, USE.NAMES = F))


