# setwd("C:/Users/eagel/OneDrive/Escritorio/Lalo/Escuela/Gob/Proyectos/")

setwd("C:/Users/SIGEH/Desktop/Lalo/Gob/Proyectos/")

anios = c(2020:2024)

datos = readxl::read_excel("Comision_Estatal_del_Agua(CEEA)_Mapa/assets/Datos/Historicos Acciones de Desinfección.xlsx", sheet = 1, col_names = T, skip = 1)
datos = datos |> dplyr::select(MUNICIPIOS, `MONITOREO DE CLORO LIBRE RESIDUAL`) |>
  dplyr::mutate(`MONITOREO DE CLORO LIBRE RESIDUAL` = ifelse(`MONITOREO DE CLORO LIBRE RESIDUAL` |> is.na(), yes = -1, no = `MONITOREO DE CLORO LIBRE RESIDUAL`)) |>
  dplyr::mutate(`MONITOREO DE CLORO LIBRE RESIDUAL` = ifelse(`MONITOREO DE CLORO LIBRE RESIDUAL` == "n/a", yes = -1, no = `MONITOREO DE CLORO LIBRE RESIDUAL`)) |>
  dplyr::mutate(`MONITOREO DE CLORO LIBRE RESIDUAL` = as.numeric(`MONITOREO DE CLORO LIBRE RESIDUAL`))

colnames(datos)[2] = gsub(pattern = "MONITOREO DE ", replacement = "", x = colnames(datos)[2])
colnames(datos)[2] = gsub(pattern = " LIBRE RESIDUAL", replacement = "", x = colnames(datos)[2])
colnames(datos)[2] = paste0(colnames(datos)[2], "_", anios[1])


guardar = datos

for (i in 2:length(anios)) {
  datos = readxl::read_excel("Comision_Estatal_del_Agua(CEEA)_Mapa/assets/Datos/Historicos Acciones de Desinfección.xlsx", sheet = i, col_names = T, skip = 1)
  datos = datos |> dplyr::select(MUNICIPIOS, `MONITOREO DE CLORO LIBRE RESIDUAL`) |>
    dplyr::mutate(`MONITOREO DE CLORO LIBRE RESIDUAL` = ifelse(`MONITOREO DE CLORO LIBRE RESIDUAL` |> is.na(), yes = -1, no = `MONITOREO DE CLORO LIBRE RESIDUAL`)) |>
    dplyr::mutate(`MONITOREO DE CLORO LIBRE RESIDUAL` = ifelse(`MONITOREO DE CLORO LIBRE RESIDUAL` == "n/a", yes = -1, no = `MONITOREO DE CLORO LIBRE RESIDUAL`)) |>
    dplyr::mutate(`MONITOREO DE CLORO LIBRE RESIDUAL` = as.numeric(`MONITOREO DE CLORO LIBRE RESIDUAL`))
  
  colnames(datos)[2] = gsub(pattern = "MONITOREO DE ", replacement = "", x = colnames(datos)[2])
  colnames(datos)[2] = gsub(pattern = " LIBRE RESIDUAL", replacement = "", x = colnames(datos)[2])
  colnames(datos)[2] = paste0(colnames(datos)[2], "_", anios[i])
  guardar = merge(x = guardar, datos, by = "MUNICIPIOS", all.x = T)
}

guardar  = guardar |> dplyr::filter(MUNICIPIOS != "TOTALES:")
guardar$MUNICIPIOS[56] = "Santiago Tulantepec de Lugo Guerrero"


geometrias = sf::read_sf("../Importantes_documentos_usar/Municipios/municipiosjair.shp")
geometrias = geometrias |> dplyr::select(CVE_MUN, NOM_MUN, geometry)

limpiar_texto = function(texto) {
  texto = tolower(texto)  # Convertir a minúsculas
  texto = stringi::stri_trans_general(texto, "Latin-ASCII")  # Eliminar acentos
  texto = gsub("\\s+", " ", texto)  # Reemplazar espacios múltiples por uno solo
  texto = trimws(texto)  # Eliminar espacios al inicio y final
  return(texto)
}

interes = merge(x = guardar |> dplyr::mutate(MUNICIPIOS = limpiar_texto(MUNICIPIOS)),
                y = geometrias |> dplyr::mutate(NOM_MUN = limpiar_texto(NOM_MUN)), 
                by.x = "MUNICIPIOS", by.y = "NOM_MUN", all.x = T)

interes = merge(x = interes, y = geometrias |> dplyr::select(CVE_MUN, NOM_MUN) |> sf::st_drop_geometry(), 
                by = "CVE_MUN", all.x = T)

interes = interes |> dplyr::select(CVE_MUN, NOM_MUN,CLORO_2020:geometry)

sf::write_sf(interes, "CEEA_Mapa/assets/Datos/shp/Historicos_Acciones.shp")


################
### Regional ###
################

datos = sf::read_sf("Comision_Estatal_del_Agua(CEEA)_Mapa/assets/Datos/shp/Historicos_Acciones.shp")
regiones = readxl::read_excel("../Importantes_documentos_usar/Banco de datos infografias _Eduardo.xlsx")
regiones = regiones[-c(1,2, 87:97),]
regiones$CVE_MUN = sprintf("%03d", c(1:84))
regiones = regiones |> dplyr::select(CVE_MUN, Región)

nombres_regiones = regiones$Región |> unique()
nombres_regiones = gsub(x = nombres_regiones, pattern = "Región ", replacement = "")


regiones_geometria = sf::read_sf("../Importantes_documentos_usar/Regional/REGIONES_HGO.shp")
regiones_geometria = regiones_geometria |> dplyr::select(REGION)
nombres_correcion = function(str) {
  return(switch(as.character(str),
                "Región I. Tula" = "Tula",
                "Región X. Apan" = "Apan",
                "Región XI. Huichapan" = "Huichapan",
                "Región XII. Jacala" = "Jacala",
                "Región II. Tulancingo" = "Tulancingo",
                "Región III. Pachuca" = "Pachuca",
                "Región IV. Huejutla" = "Huejutla",
                "Región V. Mineral de la Reforma" = "Mineral de la Reforma",
                "Región VI. Tizayuca" = "Tizayuca",
                "Región VII. Actopan" = "Actopan",
                "Región VIII. Ixmiquilpan" = "Ixmiquilpan",
                "Región IX. Zacualtipán" = "Zacualtipán",
                str  # valor por defecto si no hay coincidencia
  ))
}
regiones_geometria$REGION =  sapply(regiones_geometria$REGION, nombres_correcion, simplify = T, USE.NAMES = F)

datos = merge(x = datos, y = regiones, by = "CVE_MUN")
datos = datos |> sf::st_drop_geometry()
datos = datos |> dplyr::select(CVE_MUN, NOM_MUN,Región, CLORO_2020:CLORO_2024)
datos[, c(4:8)] = lapply(datos[,c(4:8)], as.numeric)

datos_r = datos |>
  dplyr::select(Región, CLORO_2020:CLORO_2024) |>
  dplyr::group_by(Región) |> 
  dplyr::summarise_all(mean)

datos_r$Región = gsub(x = datos_r$Región, pattern = "Región ", replacement = "")

guardar = merge(x = datos_r, y = regiones_geometria, by.x = "Región",by.y = "REGION")
sf::write_sf(guardar, "CEEA_Mapa/assets/Datos/shp/Regional.shp")



d = sf::read_sf("CEEA_Mapa/assets/Datos/shp/Regional.shp")

########### Forma de juntar geometrias
geometrias_r = datos |> 
  dplyr::select(CLORO_2020:geometry) |>
  dplyr::group_by(Región) |> 
  dplyr::summarise(geometry=sf::st_union(geometry)) 


