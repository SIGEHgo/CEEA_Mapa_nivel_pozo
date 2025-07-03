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

geometrias_multipoligono = geometrias_multipoligono |> 
  dplyr::filter(!is.na(CVEGEO_Correcto))

geometrias_multipoligono = geometrias_multipoligono |> 
  dplyr::rename(CVEGEO_LOC = CVEGEO_Correcto,
                NOM_MUN = `Municipio revisado`,
                NOMGEO_LOC = `Localidad revisada`,
                LOC = Localidad)

geometrias_multipoligono = sf::st_as_sf(x = geometrias_multipoligono, crs = sf::st_crs(loc))

malos = geometrias_multipoligono |> 
  dplyr::group_by(Año,NOM_MUN,NOMGEO_LOC,Fuente.de.Abastecimiento) |> 
  dplyr::summarise(contar = dplyr::n()) |> 
  dplyr::filter(contar > 1) |> 
  dplyr::ungroup()

malos$NOMGEO_LOC
# Quitar los duplicados de Metepec

which(geometrias_multipoligono$NOMGEO_LOC == "Metepec")
which(geometrias_multipoligono$NOMGEO_LOC == "La Cruz de Piedra")

geometrias_multipoligono = geometrias_multipoligono[-c(62,64, 111),]

geometrias_faltantes = datos |>  dplyr::filter(is.na(CVEGEO_Correcto))


#### 
loc = loc |>  dplyr::select(CVEGEO_Correcto, NOMGEO_Correcto) |>  sf::st_drop_geometry()
loc = loc |>  dplyr::mutate(CVE_MUN = substr(x = CVEGEO_Correcto, start = 1, stop = 5))
loc = merge(x = loc, y = mun, by.x = "CVE_MUN",by.y = "CVEGEO", all.x = T)
loc = loc |>  dplyr::mutate(ID = paste0(NOM_MUN, "_", NOMGEO_Correcto)) |> 
  dplyr::select(CVEGEO_Correcto, ID)


geometrias_faltantes = geometrias_faltantes |> dplyr::mutate(ID = paste0(`Municipio revisado`,"_",`Localidad revisada`)) |> 
  dplyr::select(`Municipio revisado`,`Localidad revisada`,ID)


###################
### Comparacion ###
###################

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









########### Con el otro shp


loc = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp2/13lpr.shp", options = "ENCODING=Latin1")
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
                ) |> 
  dplyr::select(-CVEGEO)

datos = merge(x = datos, y = loc, by = "Id", all.x = T)


# Geometrias punto

geometrias_punto = datos
geometrias_punto = geometrias_punto |> 
  dplyr::filter(!is.na(NOMGEO)) |> 
  dplyr::select(CVEGEO, Año, `Municipio revisado`, NOMGEO, Localidad, Fuente.de.Abastecimiento:Temperatura...C, geometry )

geometrias_punto = geometrias_punto |> 
  dplyr::rename(CVEGEO_LOC = CVEGEO,
                NOM_MUN = `Municipio revisado`,
                NOMGEO_LOC = NOMGEO,
                LOC = Localidad)


geometrias_punto = sf::st_as_sf(x = geometrias_punto, crs = sf::st_crs(loc))

# Buffer
puntos_buffer = sf::st_transform(x = geometrias_punto, crs = sf::st_crs("EPSG:32614"))
puntos_buffer = sf::st_buffer(puntos_buffer, dist = 50)

geometrias_punto = sf::st_transform(x = puntos_buffer, crs = sf::st_crs(loc))



mun = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")

geometrias_multipoligono = sf::st_transform(x = geometrias_multipoligono, crs = sf::st_crs(mun))
geometrias_punto = sf::st_transform(x = geometrias_punto, crs = sf::st_crs(mun))


unir = rbind(geometrias_multipoligono, geometrias_punto)
unir = unir |> 
  dplyr::arrange(Año,NOM_MUN,NOMGEO_LOC,Fuente.de.Abastecimiento)


datos = sf::read_sf("app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_prueba.shp")
names(datos)
unir = unir |> 
  dplyr::rename(f_abast = Fuente.de.Abastecimiento,
                coli_total = Coliformes.Totales..Ausencia.o.Presencia.100mL,
                e_coli = E..Coli.Ausencia.o.Presencia.100mL,
                as = Arsenico.mg.L,
                ba = Bario.mg.L,
                cd = Cadmio..mg.L,
                cu = Cobre.mg.L,
                fe = Hierro..mg.L,
                mn = Manganeso.mg.L,
                pb = Plomo.mg.L,
                zn = Zinc.mg.L,
                cn = Cianuros..mg.L,
                cl_res = Cloro.residual..mg.L,
                cl = Cloruros..mg.L,
                dur = Dureza.Total.mg.L,
                flu = Fluoruros..mg.L,
                no3 = Nitratos.mg.L,
                no2 = Nitritos..mg.L,
                ph = pH,
                sdt = SDT.mg.L,
                so4 = Sulfatos..mg.L,
                cl_tot = Cloro.Total..mg.L,
                cond = Conductividad..µS.cm,
                temp = Temperatura...C,
                AÑO = Año)
                


sf::write_sf(unir ,"app/assets/Datos/Datos_2012_2023_shp/faltantes.shp")

###################
### Comparacion ###
###################



faltantes = datos |>  dplyr::filter(is.na(CVEGEO))
faltantes = faltantes |>  dplyr::select(Año,`Municipio revisado`,`Localidad revisada`:Temperatura...C)

geometrias_faltantes = faltantes |> dplyr::mutate(ID = paste0(`Municipio revisado`,"_",`Localidad revisada`)) |> 
  dplyr::select(`Municipio revisado`,`Localidad revisada`,ID)


loc = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp2/13lpr.shp", options = "ENCODING=Latin1")
loc = loc |>  sf::st_drop_geometry() |> dplyr::select(CVEGEO, NOMGEO) |> 
  dplyr::mutate(CVE_MUN = substr(x = CVEGEO, start = 1, stop = 5))

loc = merge(x = loc, y = mun, by.x = "CVE_MUN", by.y = "CVEGEO", all.x = T)

loc = loc |> dplyr::mutate(ID = paste0(NOM_MUN, "_", NOMGEO)) |> 
  dplyr::select(ID,CVEGEO,NOMGEO)

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




localidad1 = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp1/13l.shp")
localidad2 = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp2/13lpr.shp")












