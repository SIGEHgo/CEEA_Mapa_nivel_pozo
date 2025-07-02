datos = readxl::read_excel("app/assets/Datos/Faltantes_Corregida.xlsx")

datos = datos |>  dplyr::select(-ID, -NOM_MUN, -Localidad)

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
  dplyr::filter()

datos = datos |>  
  dplyr::mutate(Localidad = `Localidad revisada`,
                Localidad = stringr::str_to_title(Localidad),
                Localidad = iconv(x = Localidad, from = "UTF-8", to = "ASCII//TRANSLIT"),
                Localidad = stringr::str_trim(Localidad),
                )