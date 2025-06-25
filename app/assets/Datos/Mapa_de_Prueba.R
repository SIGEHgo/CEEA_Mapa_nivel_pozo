datos = readxl::read_xlsx("app/assets/Datos/DATOS_2012_2023.xlsx") 


poligonos = read.csv("app/assets/Datos/DATOS_2012_2023_Localidades_Urbanas_Correctas.csv", fileEncoding = "latin1")
poligonos = poligonos |> 
  dplyr::filter(!is.na(CVEGEO_LOC))

puntos = read.csv("app/assets/Datos/DATOS_2012_2023_Localidades_Rurales_Correctas_Faltantes.csv", fileEncoding = "latin1")
puntos = puntos |> 
  dplyr::filter(!is.na(CVEGEO_LOC))

faltantes = read.csv("app/assets/Datos/Faltantes.csv", fileEncoding = "latin1")

nrow(poligonos) + nrow(puntos) + nrow(faltantes)




## Parece que todo bien






#################################
### Buffer y pegar geometrias ###
#################################

# Poligonos

poligonos = read.csv("app/assets/Datos/DATOS_2012_2023_Localidades_Urbanas_Correctas.csv", fileEncoding = "latin1")
poligonos = poligonos |> 
  dplyr::filter(!is.na(CVEGEO_LOC))

localidades1 = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp1/13l.shp")
localidades1 = localidades1 |> 
  dplyr::select(CVEGEO)

poligonos = merge(x = poligonos, y =  localidades1, by.x = "CVEGEO_LOC", by.y = "CVEGEO", all.x = T)
poligonos = sf::st_as_sf(x = poligonos, crs = sf::st_crs(localidades1))

plot(poligonos$geometry)






### Puntos

puntos = read.csv("app/assets/Datos/DATOS_2012_2023_Localidades_Rurales_Correctas_Faltantes.csv", fileEncoding = "latin1")
puntos = puntos |> 
  dplyr::filter(!is.na(CVEGEO_LOC))

localidades2 = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp2/13lpr.shp")
localidades2 = localidades2 |> 
  dplyr::select(CVEGEO)

puntos = merge(x = puntos, y = localidades2, by.x = "CVEGEO_LOC", by.y = "CVEGEO")
puntos = sf::st_as_sf(x = puntos, crs = sf::st_crs(localidades2))

plot(puntos$geometry)

puntos_buffer = sf::st_transform(puntos, crs = sf::st_crs("EPSG:32614"))
puntos_buffer = sf::st_buffer(puntos_buffer, dist = 50)

plot(puntos_buffer$geometry)

puntos = sf::st_transform(puntos_buffer, crs = sf::st_crs(localidades1))


### Unir bases

datos = rbind(poligonos, puntos)
datos |>  dplyr::select(Arsenico.mg.L:Temperatura...C) |>  sf::st_drop_geometry()
lapply(datos |>  dplyr::select(Arsenico.mg.L:Temperatura...C) |>  sf::st_drop_geometry(), unique)
datos$Arsenico.mg.L |>  unique()

datos[datos == "ND"] = NA   #Sacar error por las geometrias
datos[datos == "NR"] = NA   #Sacar error por las geometrias

datos = datos |> 
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(., "ND")),
         across(dplyr::where(is.character), ~dplyr::na_if(., "NR"))) |>  
  dplyr::arrange(Año,NOM_MUN)



############
### Mapa ###
############

datos$Año |>  unique()

datos_mapa = datos |>  sf::st_centroid(datos)

datos_mapa = datos_mapa |> 
  dplyr::filter(Año == 2018) |> 
  dplyr::mutate(Arsenico.mg.L = as.numeric(Arsenico.mg.L))

datos_mapa = sf::st_transform(datos_mapa, crs = "WGS84")


# Pendiente
library(leaflet)
mapa_web = leaflet() |> 
  addTiles() |> 
  addPolygons(data = datos_mapa, label = datos_mapa$Fuente.de.Abastecimiento,
              popup = paste("Municipio:", "<b>", datos_mapa$NOM_MUN , "</b>",
                            "<br>", "Localidad: ",  "<b>", datos_mapa$Localidad, "</b>",
                            "<br>", "Fuente de Abastecimiento:", "<b>", datos_mapa$Fuente.de.Abastecimiento ,"</b>",
                            "<br>", "Arsenico:", "<b>", datos_mapa$Arsenico.mg.L ,"</b>"
                            ))
mapa_web  

###############
### Markers ###
###############
datos_mapa = datos |>  sf::st_centroid(datos)

datos_mapa = datos_mapa |> 
  dplyr::filter(Año == 2018) |> 
  dplyr::mutate(Arsenico.mg.L = as.numeric(Arsenico.mg.L))

datos_mapa = sf::st_transform(datos_mapa, crs = "WGS84")

leafIcons_Arsenico = icons(
  iconUrl = ifelse(
    datos_mapa$Arsenico.mg.L  > 0.1,
    "https://leafletjs.com/examples/custom-icons/leaf-red.png",
     ifelse(
       datos_mapa$Arsenico.mg.L > 0,
       "https://leafletjs.com/examples/custom-icons/leaf-orange.png",
       "https://leafletjs.com/examples/custom-icons/leaf-green.png"
     )
  ),
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "https://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

paleta = colorNumeric(palette = c("red", "orange", "green"), 
                      domain = datos_mapa$Arsenico.mg.L,
                      na.color = "transparent",
                      reverse = T)


mapa_web = leaflet() |> 
  addTiles() |> 
  addMarkers(data = datos_mapa, 
             label = datos_mapa$Fuente.de.Abastecimiento,
             popup = paste("Municipio:", "<b>", datos_mapa$NOM_MUN , "</b>",
                           "<br>", "Localidad: ",  "<b>", datos_mapa$Localidad, "</b>",
                           "<br>", "Fuente de Abastecimiento:", "<b>", datos_mapa$Fuente.de.Abastecimiento ,"</b>",
                           "<br>", "Arsenico:", "<b>", datos_mapa$Arsenico.mg.L ,"</b>"
             ), icon = leafIcons_Arsenico,
             clusterOptions = markerClusterOptions()) |> 
  addLegend(position = "bottomleft",
            pal = paleta,
            values = datos_mapa$Arsenico.mg.L,
            title = "Arsénico (mg/L)",
            labFormat = labelFormat(suffix = " mg/L"),
            opacity = 1)

mapa_web


lapply(\(x){
  #filtro por contaminante
  mapa_web |> addPolygons()
})


datos_mapa = datos_mapa |> 
  mutate(across(9:29, as.numeric))
lapply(datos_mapa[,c(9:29)], class)

mapa_web = leaflet() |> 
  addTiles()

columnas_interes = names(datos_mapa)[9:29]
lapply(columnas_interes, function(x) {
  mapa_web = mapa_web  |> 
  addMarkers(
    data = datos_mapa,
    label = datos_mapa$Fuente.de.Abastecimiento,
    popup = paste(
      "Municipio:", "<b>", datos_mapa$NOM_MUN, "</b>",
      "<br>Localidad:", "<b>", datos_mapa$Localidad, "</b>",
      "<br>Fuente de Abastecimiento:", "<b>", datos_mapa$Fuente.de.Abastecimiento, "</b>",
      "<br>", x, ": <b>", datos_mapa[[x]], "</b>"
    ),
    icon = leafIcons_Arsenico,
    clusterOptions = markerClusterOptions(),
    group = x
  )
})
mapa_web=mapa_web |> addLayersControl(baseGroups = columnas_interes)
mapa_web



mapa_web = leaflet() |> 
  addTiles()
for (x in seq_along(columnas_interes)) {
  print(columnas_interes[x ])
  mapa_web = mapa_web  |> 
    addMarkers(
      data = datos_mapa,
      label = datos_mapa$Fuente.de.Abastecimiento,
      popup = paste(
        "Municipio:", "<b>", datos_mapa$NOM_MUN, "</b>",
        "<br>Localidad:", "<b>", datos_mapa$Localidad, "</b>",
        "<br>Fuente de Abastecimiento:", "<b>", datos_mapa$Fuente.de.Abastecimiento, "</b>",
        "<br>", columnas_interes[x], ": <b>", datos_mapa[[columnas_interes[x]]], "</b>"
      ),
      icon = leafIcons_Arsenico,
      clusterOptions = markerClusterOptions(),
      group = columnas_interes[x]
    )
}
mapa_web=mapa_web |> 
  addLayersControl(baseGroups = columnas_interes)
mapa_web





















####################################################
### Mapa web segunda forma(mas rapido eficiente) ###
####################################################

datos_mapa = datos |>  sf::st_centroid(datos)
names(datos_mapa)[1:(ncol(datos_mapa) - 1)]

columnas_nombres = c(
  "CVEGEO_LOC",
  "AÑO",
  "NOM_MUN",
  "NOMGEO_LOC",
  "LOC",
  "f_abast",   # Fuente de abastecimiento
  "coli_total",
  "e_coli",
  "as",        # Arsénico
  "ba",        # Bario
  "cd",        # Cadmio
  "cu",        # Cobre
  "fe",        # Hierro
  "mn",        # Manganeso
  "pb",        # Plomo
  "zn",        # Zinc
  "cn",        # Cianuros
  "cl_res",    # Cloro residual
  "cl",        # Cloruros
  "dur",       # Dureza total
  "flu",       # Fluoruros
  "no3",       # Nitratos
  "no2",       # Nitritos
  "ph",        # pH
  "sdt",       # Sólidos disueltos totales
  "so4",       # Sulfatos
  "cl_tot",    # Cloro total
  "cond",      # Conductividad
  "temp"       # Temperatura
)

names(datos_mapa)[1:(ncol(datos_mapa) - 1)] = columnas_nombres
names(datos_mapa)

# Preguntar por los NA que avisa
datos_mapa = datos_mapa |> 
  dplyr::mutate(dplyr::across(9:29, as.numeric))

datos_mapa = sf::st_transform(datos_mapa, crs = "WGS84")

# Colores
getColor = function(columna) {
  sapply(columna, function(x) {
    if (is.na(x)) {
      "gray"
    } else if (x == 0) {
      "green"
    } else if (x > 0 && x <= 0.1) {
      "orange"
    } else if (x > 0.1) {
      "red"
    } else {
      "gray"
    }
  })
}

library(leaflet)

mapa_web = leaflet() |> 
  addTiles()

columnas_interes = names(datos_mapa)[9:29]


for (x in seq_along(columnas_interes)) {
  col_actual = columnas_interes[x]
  print(col_actual)
  
  colores = getColor(datos_mapa[[col_actual]])
  
  icons = awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = colores
  )
  
  mapa_web = mapa_web  |> 
    addAwesomeMarkers(
      data = datos_mapa,
      label = datos_mapa$f_abast,
      popup = paste(
        "Municipio:", "<b>", datos_mapa$NOM_MUN, "</b>",
        "<br>Localidad:", "<b>", datos_mapa$LOC, "</b>",
        "<br>Fuente de Abastecimiento:", "<b>", datos_mapa$f_abast, "</b>",
        "<br>", col_actual, ": <b>", datos_mapa[[col_actual]], "</b>"
      ),
      icon = icons,
      clusterOptions = markerClusterOptions(),
      group = col_actual
    )
}

mapa_web = mapa_web |> 
  addLayersControl(baseGroups = columnas_interes)

mapa_web

























#########################
### Mapa web Tercera ####
#########################


datos_mapa = datos |>  sf::st_centroid(datos)
names(datos_mapa)[1:(ncol(datos_mapa) - 1)]

columnas_nombres = c(
  "CVEGEO_LOC",
  "AÑO",
  "NOM_MUN",
  "NOMGEO_LOC",
  "LOC",
  "f_abast",   # Fuente de abastecimiento
  "coli_total",
  "e_coli",
  "as",        # Arsénico
  "ba",        # Bario
  "cd",        # Cadmio
  "cu",        # Cobre
  "fe",        # Hierro
  "mn",        # Manganeso
  "pb",        # Plomo
  "zn",        # Zinc
  "cn",        # Cianuros
  "cl_res",    # Cloro residual
  "cl",        # Cloruros
  "dur",       # Dureza total
  "flu",       # Fluoruros
  "no3",       # Nitratos
  "no2",       # Nitritos
  "ph",        # pH
  "sdt",       # Sólidos disueltos totales
  "so4",       # Sulfatos
  "cl_tot",    # Cloro total
  "cond",      # Conductividad
  "temp"       # Temperatura
)

names(datos_mapa)[1:(ncol(datos_mapa) - 1)] = columnas_nombres
names(datos_mapa)

# Preguntar por los NA que avisa
datos_mapa = datos_mapa |> 
  dplyr::mutate(dplyr::across(as:temp, as.numeric))

datos_mapa = sf::st_transform(datos_mapa, crs = "WGS84")

# Colores
getColor = function(columna) {
  sapply(columna, function(x) {
    if (is.na(x)) {
      "gray"
    } else if (x == 0) {
      "green"
    } else if (x > 0 && x <= 0.1) {
      "orange"
    } else if (x > 0.1) {
      "red"
    } else {
      "gray"
    }
  })
}


columnas_interes = names(datos_mapa)[9:29]
columnas_completas =  c(
  "Arsénico",
  "Bario",
  "Cadmio",
  "Cobre",
  "Hierro",
  "Manganeso",
  "Plomo",
  "Zinc",
  "Cianuros",
  "Cloro residual",
  "Cloruros",
  "Dureza total",
  "Fluoruros",
  "Nitratos",
  "Nitritos",
  "pH",
  "Sólidos disueltos totales",
  "Sulfatos",
  "Cloro total",
  "Conductividad",
  "Temperatura"
)


# Extras para el mapa 
municipios = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")



loc_map = datos_mapa |>
  sf::st_drop_geometry() |>
  dplyr::group_by(CVEGEO_LOC) |>
  dplyr::summarise(dplyr::across(as:temp, mean, na.rm = TRUE))

loc_geometry = datos |> 
  dplyr::select(CVEGEO_LOC,NOMGEO_LOC,geometry) |> 
  dplyr::distinct(geometry, .keep_all = TRUE) # Filtrar geometria unica

loc_map = merge(x = loc_map, y = loc_geometry, by = "CVEGEO_LOC")
loc_map = sf::st_as_sf(x = loc_map, crs = sf::st_crs(datos))
loc_map = sf::st_transform(x = loc_map, crs = sf::st_crs(datos_mapa))

plot(loc_map$geometry)

library(leaflet)
mapa_web = leaflet() |> 
  addTiles()

for (x in seq_along(columnas_interes)) {
  col_actual = columnas_interes[x]
  print(col_actual)
  
  colores = getColor(datos_mapa[[col_actual]])
  colores_localidad = getColor(loc_map[[col_actual]])
  
  icons = awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = colores
  )
  
  mapa_web = mapa_web  |> 
    addAwesomeMarkers(
      data = datos_mapa,
      label = datos_mapa$f_abast,
      popup = paste(
        "Municipio:", "<b>", datos_mapa$NOM_MUN, "</b>",
        "<br>Localidad:", "<b>", datos_mapa$LOC, "</b>",
        "<br>Fuente de Abastecimiento:", "<b>", datos_mapa$f_abast, "</b>",
        "<br>", columnas_completas[x], ": <b>", ifelse(test = is.na(datos_mapa[[col_actual]]), yes = "No hay dato", no = datos_mapa[[col_actual]]), "</b>"),
      icon = icons,
      clusterOptions = markerClusterOptions(),
      group = columnas_completas[x]
    ) |> 
    addPolygons(data = loc_map , label = loc_map$NOMGEO_LOC, color = "black", fillColor = colores_localidad, fillOpacity = 0.1, weight = 1, group = columnas_completas[x])
}



mapa_web = mapa_web |> 
  addLayersControl(baseGroups = columnas_completas, options = layersControlOptions(collapsed = F)) |> 
  addLegend("bottomleft", colors = c('green', 'orange', 'red', 'gray'), values = datos_mapa$as,
            title = "Simbología",
            labels = c("Bueno", "Medio", "Malo", "No hay dato"),
            opacity = 1)

mapa_web = mapa_web |> 
  addPolygons(data = municipios, label = municipios$NOM_MUN, fillColor = "gray", color = "gray", fillOpacity = 0.1, opacity = 1,weight = 0.5) #|> 

mapa_web



























































#######################
### Todos los mapas ###
#######################

datos_mapa = datos
datos_mapa = datos |>  sf::st_centroid(datos)
names(datos_mapa)[1:(ncol(datos_mapa) - 1)]

columnas_nombres = c(
  "CVEGEO_LOC",
  "AÑO",
  "NOM_MUN",
  "NOMGEO_LOC",
  "LOC",
  "f_abast",   # Fuente de abastecimiento
  "coli_total",
  "e_coli",
  "as",        # Arsénico
  "ba",        # Bario
  "cd",        # Cadmio
  "cu",        # Cobre
  "fe",        # Hierro
  "mn",        # Manganeso
  "pb",        # Plomo
  "zn",        # Zinc
  "cn",        # Cianuros
  "cl_res",    # Cloro residual
  "cl",        # Cloruros
  "dur",       # Dureza total
  "flu",       # Fluoruros
  "no3",       # Nitratos
  "no2",       # Nitritos
  "ph",        # pH
  "sdt",       # Sólidos disueltos totales
  "so4",       # Sulfatos
  "cl_tot",    # Cloro total
  "cond",      # Conductividad
  "temp"       # Temperatura
)

names(datos_mapa)[1:(ncol(datos_mapa) - 1)] = columnas_nombres
names(datos_mapa)

# Preguntar por los NA que avisa
datos_mapa = datos_mapa |> 
  dplyr::mutate(dplyr::across(as:temp, as.numeric))

datos_mapa = sf::st_transform(datos_mapa, crs = "WGS84")

# Colores
getColor = function(columna) {
  sapply(columna, function(x) {
    if (is.na(x)) {
      "gray"
    } else if (x == 0) {
      "green"
    } else if (x > 0 && x <= 0.1) {
      "orange"
    } else if (x > 0.1) {
      "red"
    } else {
      "gray"
    }
  })
}


columnas_interes = names(datos_mapa)[9:29]
columnas_completas =  c(
  "Arsénico",
  "Bario",
  "Cadmio",
  "Cobre",
  "Hierro",
  "Manganeso",
  "Plomo",
  "Zinc",
  "Cianuros",
  "Cloro residual",
  "Cloruros",
  "Dureza total",
  "Fluoruros",
  "Nitratos",
  "Nitritos",
  "pH",
  "Sólidos disueltos totales",
  "Sulfatos",
  "Cloro total",
  "Conductividad",
  "Temperatura"
)


# Extras para el mapa 
municipios = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")







anios = datos_mapa$AÑO |>  unique()
guardo = datos_mapa

todos_mapas = list()
for (i in seq_along(anios)) {
  library(leaflet)
  datos_mapa = guardo
  
  datos_mapa = datos_mapa |> 
    dplyr::filter(AÑO == anios[i])
  
  loc_map = datos_mapa |>
    sf::st_drop_geometry() |>
    dplyr::group_by(CVEGEO_LOC) |>
    dplyr::summarise(dplyr::across(as:temp, mean, na.rm = TRUE))
  
  loc_geometry = datos |> 
    dplyr::filter(Año == anios[i]) |> 
    dplyr::select(CVEGEO_LOC,NOMGEO_LOC,geometry) |> 
    dplyr::distinct(geometry, .keep_all = TRUE) # Filtrar geometria unica
  
  loc_map = merge(x = loc_map, y = loc_geometry, by = "CVEGEO_LOC")
  loc_map = sf::st_as_sf(x = loc_map, crs = sf::st_crs(datos))
  loc_map = sf::st_transform(x = loc_map, crs = sf::st_crs(datos_mapa))
  
  
  
  mapa_web = leaflet() |> 
  addTiles()

  for (x in seq_along(columnas_interes)) {
    col_actual = columnas_interes[x]
    print(col_actual)
    
    colores = getColor(datos_mapa[[col_actual]])
    colores_localidad = getColor(loc_map[[col_actual]])
    
    icons = awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = colores
    )
    
    mapa_web = mapa_web  |> 
      addAwesomeMarkers(
        data = datos_mapa,
        label = datos_mapa$f_abast,
        popup = paste(
          "Municipio:", "<b>", datos_mapa$NOM_MUN, "</b>",
          "<br>Localidad:", "<b>", datos_mapa$LOC, "</b>",
          "<br>Fuente de Abastecimiento:", "<b>", datos_mapa$f_abast, "</b>",
          "<br>", columnas_completas[x], ": <b>", ifelse(test = is.na(datos_mapa[[col_actual]]), yes = "No hay dato", no = datos_mapa[[col_actual]]), "</b>"),
        icon = icons,
        clusterOptions = markerClusterOptions(),
        group = columnas_completas[x]
      ) |> 
      addPolygons(data = loc_map, label = loc_map$NOMGEO_LOC, color = "black", fillColor = colores_localidad, fillOpacity = 0.1, weight = 1, group = columnas_completas[x])
  }



  mapa_web = mapa_web |> 
    addLayersControl(baseGroups = columnas_completas, options = layersControlOptions(collapsed = F)) |> 
    addLegend("bottomleft", colors = c('green', 'orange', 'red', 'gray'), values = datos_mapa$as,
              title = "Simbología",
              labels = c("Bueno", "Medio", "Malo", "No hay dato"),
              opacity = 1)
  
  mapa_web = mapa_web |> 
    addPolygons(data = municipios, label = municipios$NOM_MUN, fillColor = "gray", color = "gray", fillOpacity = 0.1, opacity = 1,weight = 0.5) #|> 
  
  mapa_web
  
  todos_mapas[[i]] = mapa_web
}

todos_mapas[[1]] 




sf::write_sf(datos_mapa, "app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_prueba.shp")


