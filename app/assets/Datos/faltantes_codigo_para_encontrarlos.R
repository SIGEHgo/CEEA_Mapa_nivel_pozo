datos = read.csv("app/assets/Datos/Faltantes.csv", fileEncoding = "latin1")

datos = datos |>  dplyr::select(NOM_MUN, Localidad, Fuente.de.Abastecimiento)
mun = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")
mun = mun |>  dplyr::select(CVEGEO, NOM_MUN) |>  sf::st_drop_geometry()

datos= merge(x = datos, y = mun, by = "NOM_MUN", all.x = T)
datos = datos |> 
  dplyr::mutate(ID = paste0(CVEGEO, "_", Localidad))

datos_comparacion = datos |>  dplyr::select(ID)

# Localidades para comparar
loc = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp1/13l.shp")
loc = loc |> 
  dplyr::mutate(NOMGEO_L = NOMGEO,
                NOMGEO_L = stringr::str_to_title(NOMGEO_L),
                NOMGEO_L = iconv(x = NOMGEO_L, from = "UTF-8", to = "ASCII//TRANSLIT"),
                NOMGEO_L = stringr::str_trim(NOMGEO_L)
                ) |> 
  dplyr::mutate(ID = paste0(substr(x = CVEGEO, start = 1, stop = 5), "_", NOMGEO_L)) |> 
  dplyr::select(CVEGEO, NOMGEO, NOMGEO_L, ID)


loc_comparacion = loc |>  dplyr::select(ID, CVEGEO) |>  sf::st_drop_geometry()






# Comparacion
library(fuzzyjoin);
library(dplyr);

comparacion = stringdist_join(datos_comparacion, loc_comparacion, 
                         by = "ID",
                         mode = "left",
                         ignore_case = FALSE, 
                         method = "jw", 
                         max_dist = 99, 
                         distance_col = "dist")  |> 
  group_by(ID.x)  |> 
  slice_min(order_by = dist, n = 1) |> 
  dplyr::filter(dist > 0) |>  
  dplyr::arrange(dist)

comparacion

# Mal izquierda

comparacion = comparacion |> 
  dplyr::rename(datos_original_mal = ID.x,
                datos_localidad_bien = ID.y)


comparacion = merge(x = comparacion |>  dplyr::mutate(CVEGEO_MUN_ORIGINAL_MAL = substr(x = datos_original_mal, start = 1, stop = 5)), 
                    y = mun, by.x = "CVEGEO_MUN_ORIGINAL_MAL", by.y = "CVEGEO", all.x = T )            

comparacion = comparacion |> 
  dplyr::rename(NOM_MUN_ORIGINAL_MAL = NOM_MUN) |>  
  dplyr::select(-CVEGEO_MUN_ORIGINAL_MAL)

comparacion = merge(x = comparacion |>  dplyr::mutate(CVEGEO_MUN_LOCALIDAD_BIEN = substr(x = datos_localidad_bien, start = 1, stop = 5)), 
                    y = mun, by.x = "CVEGEO_MUN_LOCALIDAD_BIEN", by.y = "CVEGEO", all.x = T )            

comparacion = comparacion |> 
  dplyr::rename(NOM_MUN_LOCALIDAD_BIEN = NOM_MUN) |>  
  dplyr::select(-CVEGEO_MUN_LOCALIDAD_BIEN)


comparacion = comparacion |> 
  dplyr::select(NOM_MUN_ORIGINAL_MAL, datos_original_mal, datos_localidad_bien, NOM_MUN_LOCALIDAD_BIEN, CVEGEO, dist) |> 
  dplyr::arrange(dist)















### Localidad 2

loc = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp2/13lpr.shp", options = "ENCODING=LATIN1")
loc = loc |> 
  dplyr::mutate(NOMGEO_L = NOMGEO,
                NOMGEO_L = stringr::str_to_title(NOMGEO_L),
                NOMGEO_L = iconv(x = NOMGEO_L, from = "UTF-8", to = "ASCII//TRANSLIT"),
                NOMGEO_L = stringr::str_trim(NOMGEO_L)
  ) |> 
  dplyr::mutate(ID = paste0(substr(x = CVEGEO, start = 1, stop = 5), "_", NOMGEO_L)) |> 
  dplyr::select(CVEGEO, NOMGEO, NOMGEO_L, ID)


loc_comparacion = loc |>  dplyr::select(ID, CVEGEO) |>  sf::st_drop_geometry()



comparacion = stringdist_join(datos_comparacion, loc_comparacion, 
                              by = "ID",
                              mode = "left",
                              ignore_case = FALSE, 
                              method = "jw", 
                              max_dist = 99, 
                              distance_col = "dist")  |> 
  group_by(ID.x)  |> 
  slice_min(order_by = dist, n = 1) |> 
  dplyr::filter(dist > 0) |>  
  dplyr::arrange(dist)

comparacion

# Mal izquierda

comparacion = comparacion |> 
  dplyr::rename(datos_original_mal = ID.x,
                datos_localidad_bien = ID.y)


comparacion = merge(x = comparacion |>  dplyr::mutate(CVEGEO_MUN_ORIGINAL_MAL = substr(x = datos_original_mal, start = 1, stop = 5)), 
                    y = mun, by.x = "CVEGEO_MUN_ORIGINAL_MAL", by.y = "CVEGEO", all.x = T )            

comparacion = comparacion |> 
  dplyr::rename(NOM_MUN_ORIGINAL_MAL = NOM_MUN) |>  
  dplyr::select(-CVEGEO_MUN_ORIGINAL_MAL)

comparacion = merge(x = comparacion |>  dplyr::mutate(CVEGEO_MUN_LOCALIDAD_BIEN = substr(x = datos_localidad_bien, start = 1, stop = 5)), 
                    y = mun, by.x = "CVEGEO_MUN_LOCALIDAD_BIEN", by.y = "CVEGEO", all.x = T )            

comparacion = comparacion |> 
  dplyr::rename(NOM_MUN_LOCALIDAD_BIEN = NOM_MUN) |>  
  dplyr::select(-CVEGEO_MUN_LOCALIDAD_BIEN)


comparacion = comparacion |> 
  dplyr::select(NOM_MUN_ORIGINAL_MAL, datos_original_mal, datos_localidad_bien, NOM_MUN_LOCALIDAD_BIEN, CVEGEO, dist) |> 
  dplyr::arrange(dist)


