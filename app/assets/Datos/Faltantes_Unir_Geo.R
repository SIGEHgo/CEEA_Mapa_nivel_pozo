datos = readxl::read_excel("app/assets/Datos/Faltantes_Corregida.xlsx")

datos = datos |>  dplyr::select(-ID, -NOM_MUN)

loc = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp1/13l.shp")
loc = loc |>  
  dplyr::select(CVEGEO, NOMGEO) |> 
  dplyr::mutate(Localidad = NOMGEO,
                Localidad = stringr::str_to_title(Localidad),
                Localidad = iconv(x = Localidad, from = "UTF-8", to = "ASCII//TRANSLIT"),
                Localidad = stringr::str_trim(Localidad),
                Id = paste0(substr(x = CVEGEO, start = 1, stop = 5), "_", Localidad)
  ) 


mun = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")
mun = mun |>  dplyr::select(CVEGEO, NOM_MUN) |>  sf::st_drop_geometry()

datos = merge(x = datos, y = mun, by.x = "Municipio revisado", by.y = "NOM_MUN", all.x = T)


datos = datos |>  
  dplyr::mutate(Localidad = `Localidad revisada`,
                Localidad = stringr::str_to_title(Localidad),
                Localidad = iconv(x = Localidad, from = "UTF-8", to = "ASCII//TRANSLIT"),
                Localidad = stringr::str_trim(Localidad),
                )

datos = datos |>  
  dplyr::mutate(Id = paste0(CVEGEO, "_", `Localidad`))

loc = loc |> 
  dplyr::select(CVEGEO,NOMGEO, Id, geometry)

names(loc)[1:2] = c("CVEGEO_Correcto", "NOMGEO_Correcto")



datos = merge(x = datos, y = loc, by = "Id", all.x = T)
datos = datos |> 
  dplyr::select(CVEGEO_Correcto, Año, `Municipio revisado`, `Localidad revisada`, Localidad, Fuente.de.Abastecimiento:Temperatura...C, geometry)

geometrias_multipoligono = datos
geometrias_faltantes = datos |>  dplyr::filter(is.na(CVEGEO_Correcto))


#### 
loc = loc |>  dplyr::select(CVEGEO_Correcto, NOMGEO_Correcto) |>  sf::st_drop_geometry()
loc = loc |>  dplyr::mutate(CVE_MUN = substr(x = CVEGEO_Correcto, start = 1, stop = 5))
loc = merge(x = loc, y = mun, by.x = "CVE_MUN",by.y = "CVEGEO", all.x = T)
loc = loc |>  dplyr::mutate(ID = paste0(NOM_MUN, "_", NOMGEO_Correcto)) |> 
  dplyr::select(CVEGEO_Correcto, ID)


geometrias_faltantes = geometrias_faltantes |> dplyr::mutate(ID = paste0(`Municipio revisado`,"_",`Localidad revisada`)) |> 
  dplyr::select(`Municipio revisado`,`Localidad revisada`,ID)



library(fuzzyjoin);
library(dplyr);

comparacion = stringdist_join(geometrias_faltantes, loc, 
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











geometrias_punto = datos |>  dplyr::filter(is.na(CVEGEO_Correcto))
geometrias_punto = geometrias_punto |> dplyr::select(Año:Temperatura...C)


originales = sf::read_sf("app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_prueba.shp")












loc = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp2/13lpr.shp")
loc = loc |>  
  dplyr::select(CVEGEO, NOMGEO) |> 
  dplyr::mutate(Localidad = NOMGEO,
                Localidad = stringr::str_to_title(Localidad),
                Localidad = iconv(x = Localidad, from = "UTF-8", to = "ASCII//TRANSLIT"),
                Localidad = stringr::str_trim(Localidad),
                Id = paste0(substr(x = CVEGEO, start = 1, stop = 5), "_", Localidad)
  ) |> 
  dplyr::select(-Localidad)

loc = loc |>  dplyr::select(Id,CVEGEO:geometry)

mun = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")
mun = mun |>  sf::st_drop_geometry() |> 
  dplyr::select(CVEGEO,NOM_MUN)

datos = geometrias_punto

datos = merge(x = datos, y = mun, by.x = "Municipio revisado", by.y = "NOM_MUN", all.x = T)
datos = datos |> 
  dplyr::mutate(Localidad = `Localidad revisada`,
                Localidad = stringr::str_to_title(Localidad),
                Localidad = iconv(x = Localidad, from = "UTF-8", to = "ASCII//TRANSLIT"),
                Localidad = stringr::str_trim(Localidad),
                Id = paste0(CVEGEO, "_", Localidad)
                )

datos = merge(x = datos, y = loc, by = "Id", all.x = T)
