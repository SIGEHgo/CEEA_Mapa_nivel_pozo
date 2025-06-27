# Para ejecutar de otro sitio source()

library(leaflet.extras)
library(leaflet.extras2)
library(htmltools)


modal_1 = HTML(paste0(
  '<div class="modal fade" id="infobox" role="dialog">',
  '<div class="modal-dialog">',
  '<div class="modal-content">',
  '<div class="modal-header">',
  '<button type="button" class="close" data-dismiss="modal">&times;</button>',
  '<h4 class="modal-title">Información Adicional</h4>',
  '</div>',
  '<div class="modal-body">',
  '<h4>Sobre los datos:</h4>',
  '<ul>',
  '<li>La Norma Oficial Mexicana <a href="https://www.dof.gob.mx/nota_detalle_popup.php?codigo=5650705" target="_blank">NOM-127-SSA1-2021</a> establece los límites permitidos de calidad del agua para uso y consumo humano. Estos límites están representados en cada simbología del mapa.</li>',
  '</ul>',
  '</div>',
  '<div class="modal-footer">',
  '<button type="button" class="btn btn-default" data-dismiss="modal">Regresar a explorar el mapa</button>',
  '</div>',
  '</div>',
  '</div>',
  '</div>'
))

datos = sf::read_sf("app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_prueba.shp")
datos_mapa = datos |>  sf::st_centroid(datos)

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

# Paleta de colores
limite_inferior = c(0,0,0,0,0,0,0,0,0,0.2,0,0,0,0,0,6.5,0,0,0,0,0) 
limite_superior = c(0.025, 1.3, 0.005, 2.0, 0.3, 0.15, 0.01, 5.0, 0.07, 1.5, 250, 500, 1.5, 11, 0.9, 8.5, 1000, 400, 1.1111, 2000, 25)
unidades = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L","mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "pH", "mg/L", "mg/L", "(no especificado)", "µS/cm", "°C") 
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
    dplyr::filter(AÑO == anios[i]) |> 
    dplyr::mutate(ID = paste(NOM_MUN, "-", NOMGEO_LOC)) |> 
    dplyr::select(CVEGEO_LOC,NOMGEO_LOC,ID, geometry) |> 
    dplyr::distinct(geometry, .keep_all = TRUE) # Filtrar geometria unica
  
  loc_map = merge(x = loc_map, y = loc_geometry, by = "CVEGEO_LOC")
  loc_map = sf::st_as_sf(x = loc_map, crs = sf::st_crs(datos))
  loc_map = sf::st_transform(x = loc_map, crs = sf::st_crs(datos_mapa))
  

  
  
  mapa_web = leaflet() |> 
    addTiles()
  
  for (x in seq_along(columnas_interes)) {
    col_actual = columnas_interes[x]
    print(col_actual)
    
    
    inferior = limite_inferior[x]
    superior = limite_superior[x]
    unidad = unidades[x]
    
    if (inferior == 0) {
      
      getColor = function(columna) {
        sapply(columna, function(x) {
          if (is.na(x)) {
            "gray"
          } else if (x >= inferior && x <= superior) {
            "green"
          } else if (x > superior) {
            "red"
          } else {
            "gray"
          }
          })
      }
      
    } else {
      
      getColor = function(columna) {
      sapply(columna, function(x) {
        if (is.na(x)) {
          "gray"
        } else if (x >= inferior && x <= superior) {
          "green"
        } else if (x > superior) {
          "yellow"
        } else if (x < inferior) {
          "red"
        } else {
          "gray"
        }
      })
      }
      
    }
    

    
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
          "<br>Localidad:", "<b>", datos_mapa$NOMGEO_LOC, "</b>",
          "<br>Fuente de Abastecimiento:", "<b>", datos_mapa$f_abast, "</b>",
          "<br>", columnas_completas[x], ": <b>", ifelse(test = is.na(datos_mapa[[col_actual]]), yes = "No hay dato", no = paste(datos_mapa[[col_actual]], unidad)), "</b>"),
        icon = icons,
        clusterOptions = markerClusterOptions(),
        group = columnas_completas[x]
      ) |> 
      addPolygons(data = loc_map, label = loc_map$ID, color = "black", fillColor = colores_localidad, fillOpacity = 0.1, weight = 1, group = columnas_completas[x]) |> 
      addLayersControl(baseGroups = columnas_completas, options = layersControlOptions(collapsed = F)) 
    
    
    if (inferior == 0) {
      mapa_web = mapa_web |> 
        addLegend("bottomleft", colors = c("gray", "red", "green"), values = datos_mapa[[col_actual]],
                  title = columnas_completas[x],
                  labels = c("No hay dato", paste(">", superior, unidad), paste("≤", superior, unidad)),
                  opacity = 1,
                  group = columnas_completas[x])
    } else {
      mapa_web = mapa_web |> 
        addLegend("bottomleft", colors = c("gray", "red", "green", "yellow"), values = datos_mapa[[col_actual]],
                  title =  columnas_completas[x],
                  labels = c("No hay dato", paste("<", inferior, unidad), paste(inferior, unidad, "≤", col_actual,  "≤", superior, unidad), paste(superior, unidad, ">")),
                  opacity = 1,
                  group = columnas_completas[x])
    }

  }
  
  mapa_web = mapa_web |> 
    addPolygons(data = municipios, label = municipios$NOM_MUN, fillColor = "gray", color = "gray", fillOpacity = 0.1, opacity = 1,weight = 0.5) |> 
    addPolygons(data = loc_map, label = loc_map$ID, fillColor = NA, fillOpacity = 0, color = NA, weight = 0, group = "buscador") |> 
    addSearchFeatures(targetGroups = "buscador",
                      options = searchFeaturesOptions(
                        zoom = 12, 
                        openPopup = F,
                        firstTipSubmit =F)) |> 
    leaflet.extras::addBootstrapDependency() |> 
    addEasyButton(easyButton(
      icon = "fa-info-circle", title = "Map Information",
      onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
    )) %>% # Trigger the infobox
    htmlwidgets::appendContent(modal_1) |> 
    htmlwidgets::onRender(
      "function(el, x) {
      var map = this;

      // 1) Insertar un mensaje encima del control de capas
      var layersControl = document.getElementsByClassName('leaflet-control-layers-list')[0];  // Devuelve un HTMLCollection con todos los nodos que tienen esa clase.
      var instrEmpty = document.createElement('div');   // Crea un <div> vacío en memoria.
      instrEmpty.style.height = '10px'; // Para dejar un margen visual.
      instrEmpty.innerHTML = ' ';  // Ponemos un espacio como contenido, para asegurar que el <div> no colapse totalmente.
      var instr = document.createElement('span'); // Línea que contendrá el mensaje.
      instr.style.fontSize = '16px';  
      instr.innerHTML = 'Capas';
      layersControl.insertBefore(instrEmpty, layersControl.firstChild);  // Inserta newNode justo antes de referenceNode.
      layersControl.insertBefore(instr, layersControl.firstChild);
      
      
      
      function actualizarLeyendas() {
        var baseLayers = document.querySelectorAll('.leaflet-control-layers-base input[type=radio]');
        var leyendas = document.getElementsByClassName('info legend leaflet-control');
        
        baseLayers.forEach(function(input) {
          if (input.checked) {
            var activa = input.nextSibling.textContent.trim();

            Array.from(leyendas).forEach(function(leyendaEl) {
              var textoLeyenda = leyendaEl.children[0]?.children[0]?.innerHTML?.trim();
      
              if (textoLeyenda === activa) {
                leyendaEl.style.display = 'block'; // Mostrar solo la leyenda activa
              } else {
                leyendaEl.style.display = 'none'; // Ocultar las demás
              }
            });
          }
        });
      }
      
      // Asignar el evento a cada input tipo radio
      var baseLayers = document.querySelectorAll('.leaflet-control-layers-base input[type=radio]');
      baseLayers.forEach(function(input) {
        input.addEventListener('change', actualizarLeyendas);
      });
      
      actualizarLeyendas();

    }"
    )
  
  mapa_web
  
  todos_mapas[[i]] = mapa_web
  
  htmlwidgets::saveWidget(mapa_web, paste0("app/assets/Datos/Mapas/", "Mapa_", anios[i],".html"), selfcontained = F, title = anios[i])
  
}














































































































##############################
### Actualizacion del mapa ###
##############################
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(htmltools)
library(htmlwidgets)

modal_1 = HTML(paste0(
  '<div class="modal fade" id="infobox" role="dialog">',
  '<div class="modal-dialog">',
  '<div class="modal-content">',
  '<div class="modal-header">',
  '<button type="button" class="close" data-dismiss="modal">&times;</button>',
  '<h4 class="modal-title">Información Adicional</h4>',
  '</div>',
  '<div class="modal-body">',
  '<h4>Sobre los datos:</h4>',
  '<ul>',
  '<li>La Norma Oficial Mexicana <a href="https://www.dof.gob.mx/nota_detalle_popup.php?codigo=5650705" target="_blank">NOM-127-SSA1-2021</a> establece los límites permitidos de calidad del agua para uso y consumo humano. Estos límites están representados en cada simbología del mapa.</li>',
  '</ul>',
  '</div>',
  '<div class="modal-footer">',
  '<button type="button" class="btn btn-default" data-dismiss="modal">Regresar a explorar el mapa</button>',
  '</div>',
  '</div>',
  '</div>',
  '</div>'
))

datos = sf::read_sf("app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_prueba.shp")
datos_mapa = sf::read_sf("app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_puntos_aleatorios_unicos.shp")

datos = datos |> 
  dplyr::mutate(ID = paste0(CVEGEO_LOC,"_",NOMGEO_LOC, "_", f_abast))
length(unique(datos$ID))

datos_mapa = datos_mapa |> 
  dplyr::mutate(ID = paste0(CVEGEO_LOC, "_", NOMGEO_LOC, "_", f_abast))
length(unique(datos_mapa$ID))


datos_mapa = merge(x = datos |>  sf::st_drop_geometry(), y = datos_mapa |>  dplyr::select(ID,geometry), by = "ID", all.x = T)
datos_mapa = datos_mapa |>  dplyr::select(-ID)
datos_mapa = sf::st_as_sf(x = datos_mapa, crs = sf::st_crs(datos))
datos = datos |>  dplyr::select(-ID)

  
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

# Paleta de colores
limite_inferior = c(0,0,0,0,0,0,0,0,0,0.2,0,0,0,0,0,6.5,0,0,0,0,0) 
limite_superior = c(0.025, 1.3, 0.005, 2.0, 0.3, 0.15, 0.01, 5.0, 0.07, 1.5, 250, 500, 1.5, 11, 0.9, 8.5, 1000, 400, 1.1111, 2000, 25)
unidades = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L","mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "pH", "mg/L", "mg/L", "(no especificado)", "µS/cm", "°C") 
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
    dplyr::filter(AÑO == anios[i]) |> 
    dplyr::mutate(ID = paste(NOM_MUN, "-", NOMGEO_LOC)) |> 
    dplyr::select(CVEGEO_LOC,NOMGEO_LOC,ID, geometry) |> 
    dplyr::distinct(geometry, .keep_all = TRUE) # Filtrar geometria unica
  
  loc_map = merge(x = loc_map, y = loc_geometry, by = "CVEGEO_LOC")
  loc_map = sf::st_as_sf(x = loc_map, crs = sf::st_crs(datos))
  loc_map = sf::st_transform(x = loc_map, crs = sf::st_crs(datos_mapa))
  
  
  
  
  mapa_web = leaflet() |> 
    addTiles()
  
  for (x in seq_along(columnas_interes)) {
    col_actual = columnas_interes[x]
    print(col_actual)
    
    
    inferior = limite_inferior[x]
    superior = limite_superior[x]
    unidad = unidades[x]
    
    if (inferior == 0) {
      
      getColor = function(columna) {
        sapply(columna, function(x) {
          if (is.na(x)) {
            "gray"
          } else if (x >= inferior && x <= superior) {
            "green"
          } else if (x > superior) {
            "red"
          } else {
            "gray"
          }
        })
      }
      
    } else {
      
      getColor = function(columna) {
        sapply(columna, function(x) {
          if (is.na(x)) {
            "gray"
          } else if (x >= inferior && x <= superior) {
            "green"
          } else if (x > superior) {
            "yellow"
          } else if (x < inferior) {
            "red"
          } else {
            "gray"
          }
        })
      }
      
    }
    
    
    
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
          "<br>Localidad:", "<b>", datos_mapa$NOMGEO_LOC, "</b>",
          "<br>Fuente de Abastecimiento:", "<b>", datos_mapa$f_abast, "</b>",
          "<br>", columnas_completas[x], ": <b>", ifelse(test = is.na(datos_mapa[[col_actual]]), yes = "No hay dato", no = paste(datos_mapa[[col_actual]], unidad)), "</b>"),
        icon = icons,
        #clusterOptions = markerClusterOptions(),
        group = columnas_completas[x]
      ) |> 
      addPolygons(data = loc_map, label = loc_map$ID, color = "black", fillColor = colores_localidad, fillOpacity = 0.1, weight = 1, group = columnas_completas[x]) |> 
      addLayersControl(baseGroups = columnas_completas, options = layersControlOptions(collapsed = F)) 
    
    
    if (inferior == 0) {
      mapa_web = mapa_web |> 
        addLegend("bottomleft", colors = c("gray", "red", "green"), values = datos_mapa[[col_actual]],
                  title = columnas_completas[x],
                  labels = c("No hay dato", paste(">", superior, unidad), paste("≤", superior, unidad)),
                  opacity = 1,
                  group = columnas_completas[x])
    } else {
      mapa_web = mapa_web |> 
        addLegend("bottomleft", colors = c("gray", "red", "green", "yellow"), values = datos_mapa[[col_actual]],
                  title =  columnas_completas[x],
                  labels = c("No hay dato", paste("<", inferior, unidad), paste(inferior, unidad, "≤", col_actual,  "≤", superior, unidad), paste(superior, unidad, ">")),
                  opacity = 1,
                  group = columnas_completas[x])
    }
    
  }
  
  mapa_web = mapa_web |> 
    addPolygons(data = municipios, label = municipios$NOM_MUN, fillColor = "gray", color = "gray", fillOpacity = 0.1, opacity = 1,weight = 0.5) |> 
    addPolygons(data = loc_map, label = loc_map$ID, fillColor = NA, fillOpacity = 0, color = NA, weight = 0, group = "buscador") |> 
    addSearchFeatures(targetGroups = "buscador",
                      options = searchFeaturesOptions(
                        zoom = 12, 
                        openPopup = F,
                        firstTipSubmit =F)) |> 
    leaflet.extras::addBootstrapDependency() |> 
    addEasyButton(easyButton(
      icon = "fa-info-circle", title = "Map Information",
      onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
    )) %>% # Trigger the infobox
    htmlwidgets::appendContent(modal_1) |> 
    htmlwidgets::onRender(
      "function(el, x) {
      var map = this;

      // 1) Insertar un mensaje encima del control de capas
      var layersControl = document.getElementsByClassName('leaflet-control-layers-list')[0];  // Devuelve un HTMLCollection con todos los nodos que tienen esa clase.
      var instrEmpty = document.createElement('div');   // Crea un <div> vacío en memoria.
      instrEmpty.style.height = '10px'; // Para dejar un margen visual.
      instrEmpty.innerHTML = ' ';  // Ponemos un espacio como contenido, para asegurar que el <div> no colapse totalmente.
      var instr = document.createElement('span'); // Línea que contendrá el mensaje.
      instr.style.fontSize = '16px';  
      instr.innerHTML = 'Capas';
      layersControl.insertBefore(instrEmpty, layersControl.firstChild);  // Inserta newNode justo antes de referenceNode.
      layersControl.insertBefore(instr, layersControl.firstChild);
      
      
      
      function actualizarLeyendas() {
        var baseLayers = document.querySelectorAll('.leaflet-control-layers-base input[type=radio]');
        var leyendas = document.getElementsByClassName('info legend leaflet-control');
        
        baseLayers.forEach(function(input) {
          if (input.checked) {
            var activa = input.nextSibling.textContent.trim();

            Array.from(leyendas).forEach(function(leyendaEl) {
              var textoLeyenda = leyendaEl.children[0]?.children[0]?.innerHTML?.trim();
      
              if (textoLeyenda === activa) {
                leyendaEl.style.display = 'block'; // Mostrar solo la leyenda activa
              } else {
                leyendaEl.style.display = 'none'; // Ocultar las demás
              }
            });
          }
        });
      }
      
      // Asignar el evento a cada input tipo radio
      var baseLayers = document.querySelectorAll('.leaflet-control-layers-base input[type=radio]');
      baseLayers.forEach(function(input) {
        input.addEventListener('change', actualizarLeyendas);
      });
      
      actualizarLeyendas();

    }"
    )
  
  mapa_web
  
  todos_mapas[[i]] = mapa_web
  
  #htmlwidgets::saveWidget(mapa_web, paste0("app/assets/Datos/Mapas/", "Mapa_", anios[i],".html"), selfcontained = F, title = anios[i])
  
}


todos_mapas[[2]]















