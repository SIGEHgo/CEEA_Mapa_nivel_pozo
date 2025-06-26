source("codigos/puras_librerias.R")
cabeza_porc=read_sf("SEMARNATH/inputs/SIGEH_B_EMISIONES_METANO_PECUARIO_061124_EOB.shp")
cabeza_porc$Cabezas_Bo=format(cabeza_porc$Cabezas_Bo,big.mark = ",",trim = T)
cabeza_porc$Cabezas_po=format(cabeza_porc$Cabezas_po,big.mark = ",",trim = T)
paleta_rojos=colorNumeric("Reds",domain = 0:1000/1000)
paleta_verde_rojos=colorNumeric(c("green","red"),domain = 0:1000/1000)
escala_01=function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
###
genera_puntos_aleatorios=function(poligono,n){
  if(n>0){
    frontera=poligono|>st_geometry()|>st_cast("MULTILINESTRING")|>st_cast("LINESTRING")|>st_line_sample(n=n,type = "regular")
    B <- st_centroid(poligono)
    
    # Extraer las coordenadas del multipunto y del centroide
    coords_A <- st_coordinates(frontera)
    coords_B <- st_coordinates(B)
    
    # Calcular el promedio euclidiano entre cada punto del multipunto y el centroide
    coords_C <- apply(coords_A, 1, function(coord) {
      x_avg <- (coord[1] + coords_B[1]) / 2
      y_avg <- (coord[2] + coords_B[2]) / 2
      c(x_avg, y_avg)
    })
    coords_C <- t(coords_C)  # Transponer para obtener un formato adecuado
    
    # Crear el nuevo multipunto resultante
    C <- st_sfc(st_multipoint(coords_C), crs = st_crs(poligono))
  }
  else{
    C=poligono|>st_geometry()|>st_cast("MULTILINESTRING")|>st_cast("LINESTRING")|>st_line_sample(n=n,type = "regular")
  }
  
  return(
    C
  )
}
#acciones_mitiga[48,]|>st_geometry()|>st_cast("MULTILINESTRING")|>st_cast("LINESTRING")|>st_line_sample(n=5,type = "regular")
#genera_puntos_aleatorios(acciones_mitiga[48,], 5)
##Son 8 posibles acciones de mitigacion. 
#nueva informacion
acciones_mitiga=read_sf("SEMARNATH/inputs/Acciones_SEMARTH_mitigacion_CO2.shp")
#acciones_mitiga$geometry[1]|>st_cast("MULTILINESTRING")|>st_cast("LINESTRING")|>st_line_sample(n=5,type = "regular")
acciones_mitiga$num_acc=rowSums(st_drop_geometry(acciones_mitiga[,5:12])>0)
acciones_mitiga=acciones_mitiga[order(acciones_mitiga$CVE_MUN|>as.numeric()),]
#
sem_2=read_sf("SEMARNATH/inputs/EMISIONES_METANO_ANUAL.shp")
sdf_puntos=read_sf("SEMARNATH/inputs/SDF_2021_PUNTOS.shp")

municipios=read_sf("rasters/municipiosjair.shp")
municipios=merge(x=municipios,y=acciones_mitiga|>st_drop_geometry()|>select(CVEGEO,num_acc),by="CVEGEO",all.x=T)
marker_sdf=makeIcon(
  iconUrl = "SEMARNATH/mapa web/imagenes/SDF.png",
  iconWidth = 40, iconHeight = 40,
)
#
puntos_list <- list()

# Iterar sobre cada municipio
for (mun in 1:84) {
  # Extraer el polígono del municipio actual
  mun_i_acc <- acciones_mitiga[mun, ]
  
  # Generar puntos aleatorios en el perímetro del municipio actual
  # Suponemos que `genera_puntos_aleatorios` devuelve un objeto `sf`
  puntos_mun <- genera_puntos_aleatorios(mun_i_acc, acciones_mitiga$num_acc[mun])
  
  # Añadir una columna con el identificador del municipio
  puntos_mun <- puntos_mun|>st_as_sf() %>% dplyr::mutate(municipio_id = mun)
  
  # Agregar los puntos a la lista
  puntos_list[[mun]] <- puntos_mun
}

# Combinar todos los puntos en un solo objeto `sf`
puntos_sf <- do.call(rbind, puntos_list)

####Otra opcion. 
#pivot_longer para pasar de columna a renglon. 
#Luego, habra tantas repeticiones de municipio como acciones
#desdoblo los puntos con un cast y simplemente lo pego porque debe tener la misma lingitud
#Entonces parece que puntos_sf si sirve. 
#puntos_sf2=st_disjoint(puntos_sf)#3No jalo
acciones_mitiga_2=acciones_mitiga|>tidyr::pivot_longer(cols = 5:12, 
                                                       names_to = "variable", 
                                                       values_to = "valor") %>%
  filter(valor != 0)
puntos_desdoblados=list()
for(i in 1:nrow(puntos_sf)){
  if(!st_is_empty(puntos_sf[i,]|>st_geometry())){
    puntos_desdoblados[[i]]=st_cast(puntos_sf[i,],"POINT")
  }
}
puntos_desdoblados=do.call(rbind,sapply(puntos_desdoblados, function(x) if(length(x)>0) return(x)))


puntos_acciones=cbind(acciones_mitiga_2|>dplyr::select(CVE_MUN,NOMGEO,num_acc,variable)|>st_drop_geometry(),y=puntos_desdoblados|>dplyr::mutate(municipio_id=sprintf("%03d",municipio_id)))



#colnames(acciones_mitiga)[5:12]
lista_acciones=list()
for(i in 1:length(unique(acciones_mitiga_2$variable))){
  lista_acciones[[i]]=puntos_acciones|>
    dplyr::filter(variable==unique(acciones_mitiga_2$variable)[i])|>
    st_set_geometry("y.x")
}

marker_default_size=20
marker_saneam=makeIcon(
  iconUrl = "SEMARNATH/mapa web/imagenes/saneam.png",
  iconWidth = marker_default_size, iconHeight = marker_default_size,
)
marker_energia=makeIcon(
  iconUrl = "SEMARNATH/mapa web/imagenes/Energía fotovoltaica.png",
  iconWidth = marker_default_size, iconHeight = marker_default_size,
)
marker_refor=makeIcon(
  iconUrl = "SEMARNATH/mapa web/imagenes/Reforestación.png",
  iconWidth = marker_default_size, iconHeight = marker_default_size,
)
marker_alianza=makeIcon(
  iconUrl = "SEMARNATH/mapa web/imagenes/reciclando transformamos.png",
  iconWidth = marker_default_size, iconHeight = marker_default_size,
)
marker_sfi=makeIcon(
  iconUrl = "SEMARNATH/mapa web/imagenes/Eatmosferica.png",
  iconWidth = marker_default_size, iconHeight = marker_default_size,
)
marker_recicla=makeIcon(
  iconUrl = "SEMARNATH/mapa web/imagenes/Reciclaje.png",
  iconWidth = marker_default_size, iconHeight = marker_default_size,
)
#####


semarnath=leaflet()|>setView(-98.88704,20.47901,zoom = 09)|>addTiles(options = tileOptions(minZoom=9,opacity = 0.7))
semarnath=semarnath|>
  addMarkers(data=lista_acciones[[1]]|>st_transform("EPSG:4326")|>as("Spatial"),icon = marker_alianza,group = "Acciones de Mitigación")|>
  addMarkers(data=lista_acciones[[2]]|>st_transform("EPSG:4326")|>as("Spatial"),icon = marker_saneam,group = "Acciones de Mitigación")|>
  addMarkers(data=lista_acciones[[3]]|>st_transform("EPSG:4326")|>as("Spatial"),icon = marker_sfi,group = "Acciones de Mitigación")|>
  addMarkers(data=lista_acciones[[4]]|>st_transform("EPSG:4326")|>as("Spatial"),icon = marker_refor,group = "Acciones de Mitigación")|>
  addMarkers(data=lista_acciones[[5]]|>st_transform("EPSG:4326")|>as("Spatial"),icon = marker_energia,group = "Acciones de Mitigación")|>
  addMarkers(data=lista_acciones[[6]]|>st_transform("EPSG:4326")|>as("Spatial"),icon = marker_recicla,group = "Acciones de Mitigación")

semarnath=semarnath|>
  addPolygons(data=municipios|>as("Spatial"),label=~paste(paste0("<b style='font-size:12px'>","Municipio: ","</b>",htmlEscape(municipios$NOM_MUN)),
                                                          paste0("<b style='font-size:12px'>","Número de Acciones de Mitigación: ",
                                                                 "</b>",htmlEscape(municipios$num_acc))
                                                          ,sep = "<br>")|>
                sapply(FUN = function(x) HTML(x),USE.NAMES = F)|>
                `class<-`("html"),labelOptions = labelOptions(textsize = 14,direction = "top")
                ,fillOpacity = 0,color = "black",weight = 2,dashArray = "5,5",group = "buscar",opacity = 0.5)|>
  addPolygons(data=cabeza_porc|>st_transform(st_crs(municipios)),
              color=paleta_verde_rojos(cabeza_porc$ton_1|>escala_01()),
              group = "Emisión de Metano por cría de Porcinos",
              weight = 4,
              label  = ~paste(paste0("<b style='font-size:12px'>","Municipio: ","</b>",htmlEscape(cabeza_porc$NOMGEO)),
                              paste0("<b style='font-size:12px'>","Toneladas de Metano (CH<sub>4</sub>): ",
                                     "</b>",htmlEscape(cabeza_porc$ton_1|>round(2))),
                              paste0("<b style='font-size:12px'>","Cabezas de ganado Porcino: " ,"</b>",htmlEscape(cabeza_porc$Cabezas_po)),
                              paste0("<b style='font-size:12px'>","Promedio de Emisión: ","</b>","1 kg CH<sub>4</sub>/cabeza")
                              ,sep = "<br>")|>
                sapply(FUN = function(x) HTML(x),USE.NAMES = F)|>
                `class<-`("html"),
              labelOptions = labelOptions(textsize = 14,direction = "top"))|>
  addPolygons(data=cabeza_porc|>st_transform(st_crs(municipios),
                                             highlightOptions = highlightOptions(color = "yellow",bringToFront = T,weight = 2)),
              color=paleta_verde_rojos(cabeza_porc$Ton|>escala_01()),
              group = "Emisión de Metano por cría de Bovinos",
              label  = ~paste(paste0("<b style='font-size:12px'>","Municipio: ","</b>",htmlEscape(cabeza_porc$NOMGEO)),
                              paste0("<b style='font-size:12px'>","Toneladas de Metano (CH<sub>4</sub>): ",
                                     "</b>",cabeza_porc$Ton|>round(2)|>format(big.mark = ",",trim = T)|>htmlEscape()),
                              paste0("<b style='font-size:12px'>","Cabezas de ganado Bovino: " ,"</b>",htmlEscape(cabeza_porc$Cabezas_Bo)),
                              paste0("<b style='font-size:12px'>","Promedio de Emisión: ","</b>","56 kg CH<sub>4</sub>/cabeza/año")
                              ,sep = "<br>")|>
                sapply(FUN = function(x) HTML(x),USE.NAMES = F)|>
                `class<-`("html"),
              labelOptions = labelOptions(textsize = 14,direction = "top"),
              ,highlightOptions = highlightOptions(color = "yellow",bringToFront = T,weight = 2))|>
  addPolygons(data=sem_2|>st_transform(st_crs("EPSG:4326"))|>as("Spatial"),
              label  = ~paste(paste0("<b style='font-size:12px'>","Municipio: ","</b>",htmlEscape(sem_2$NOMGEO)),
                              paste0("<b style='font-size:12px'>","Toneladas de Metano (CH<sub>4</sub>): ",
                                     "</b>",sem_2$METANO_ANU|>round(2)|>format(big.mark = ",",trim = T)|>htmlEscape())
                              ,sep = "<br>")|>
                sapply(FUN = function(x) HTML(x),USE.NAMES = F)|>
                `class<-`("html"),
              labelOptions = labelOptions(textsize = 14,direction = "top"),
              color = paleta_verde_rojos(sem_2$METANO_ANU|>escala_01()),
              highlightOptions = highlightOptions(color = "yellow",bringToFront = T,weight = 2),
              group = "Emisiones de Metano en SDF")|>
  addMarkers(data=sdf_puntos|>st_transform(st_crs("EPSG:4326"))|>as("Spatial"),
             icon = marker_sdf,
             group = "Emisiones de Metano en SDF",
             label=~paste0("<b style='font-size:12px'>","Municipio: ","</b>",htmlEscape(sdf_puntos$MUNICIPIO))|>
               sapply(FUN = function(x) HTML(x),USE.NAMES = F)|>
               `class<-`("html"))|>
  addLayersControl(baseGroups= c("Emisión de Metano por cría de Bovinos","Emisión de Metano por cría de Porcinos","Emisiones de Metano en SDF","Acciones de Mitigación"),
                  ,options = layersControlOptions(collapsed=F))|>
  addSearchFeatures(targetGroups = "buscar",
                    options = searchFeaturesOptions(
                      zoom = 12, 
                      openPopup = F,
                      firstTipSubmit =F,
                      hideMarkerOnCollapse =T))|>
  #addLegend(position = "bottomright",pal = paleta_rojos(c(0.25,0.5,0.8)),values=c("Alto","Medio","Bajo"))|>

  addLegendImage(images =c("SEMARNATH/mapa web/imagenes/alto.png","SEMARNATH/mapa web/imagenes/medio.png","SEMARNATH/mapa web/imagenes/bajo.png"),
                 labels = c("Alto","Medio","Bajo"),
                 orientation = 'vertical',
                 title = htmltools::tags$div('Emisión relativa',
                                             style = 'font-size: 24px; text-align: center;display:block;margin:0'),
                 position = 'bottomright')|>
  addLegendImage(images =c("SEMARNATH/mapa web/imagenes/alto.png","SEMARNATH/mapa web/imagenes/medio.png","SEMARNATH/mapa web/imagenes/bajo.png","SEMARNATH/mapa web/imagenes/SDF.png"),
                 labels = c("Alto","Medio","Bajo","Sitios de Disposición Final (SDF)"),width = 30, height = 30,
                 orientation = 'vertical',
                 title = htmltools::tags$div('Simbología',
                                             style = 'font-size: 24px; text-align: center;'),
                 position = 'bottomright')|>
  addLegendImage(images =c("SEMARNATH/mapa web/imagenes/reciclando transformamos.png","SEMARNATH/mapa web/imagenes/saneam.png","SEMARNATH/mapa web/imagenes/Eatmosferica.png","SEMARNATH/mapa web/imagenes/Reforestación.png","SEMARNATH/mapa web/imagenes/Energía fotovoltaica.png","SEMARNATH/mapa web/imagenes/Reciclaje.png"),
                 labels = c("Alianzas","Saneamiento","SFI","Reforestación","Sustentabilidad Energética","Reciclaje"),width = 20, height = 20,
                 orientation = 'vertical',
                 title = htmltools::tags$div('Simbología',
                                             style = 'font-size: 24px; text-align: center;'),
                 position = 'bottomright')|>
  addControl(html = "<img src='https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/explicacion_mitigaciones.png' style='width:520px; height:350px'>",position = "bottomleft",className = "logoMapaWebDinamic")|>
  addControl(html = "<img src='https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/explicacion_sdf.png' style='width:520px; height:350px'>",position = "bottomleft",className = "logoMapaWebDinamic")|>
  addControl(html = "<img src='https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/explicacion_porcino.png' style='width:520px; height:350px'>",position = "bottomleft",className = "logoMapaWebDinamic")|>
  addControl(html = "<img src='https://raw.githubusercontent.com/JairEsc/Gob/main//Otros_archivos/imagenes/explicacion_bovino.png' style='width:520px; height:350px'>",position = "bottomleft",className = "logoMapaWebDinamic")|>
  
  htmlwidgets::onRender( "function(el, x) {
    var map = this;
    var legendDiv = document.getElementsByClassName('info legend leaflet-control');
    legendDiv[2].children[0].children[0].style.marginBottom='5px'
    legendDiv[2].children[1].children[0].style.margin=0
    legendDiv[2].children[2].children[0].style.margin=0
    legendDiv[2].children[3].children[0].style.margin=0
    legendDiv[1].children[0].children[0].style.marginBottom='5px'
    legendDiv[1].children[1].children[0].style.margin=0
    legendDiv[1].children[2].children[0].style.margin=0
    legendDiv[1].children[3].children[0].style.margin=0
    var controls = document.getElementsByClassName('logoMapaWebDinamic leaflet-control');
    //console.log(controls)
    Array.from(controls).forEach((control, index) => {
        control.style.display = (index === 0) ? 'block' : 'none';
        //console.log(control.innerHTML)
      });
    Array.from(legendDiv).forEach((legend_div, index) => {
        legend_div.style.display = (index === 2) ? 'block' : 'none';
        //console.log(control.innerHTML)
      });
    
    map.on('baselayerchange', function (eventLayer) {
    map.whenReady(resizeMarkers);
    Array.from(controls).forEach((control, index) => {
        control.style.display = 'none';
      });
    switch(eventLayer.name) {
      case 'Emisión de Metano por cría de Bovinos':
        controls[0].style.display='block'
        break;
      case 'Emisión de Metano por cría de Porcinos':
        controls[1].style.display='block'
        break;
      case 'Emisiones de Metano en SDF':
        controls[2].style.display='block'
        break;
      case 'Acciones de Mitigación':
        controls[3].style.display='block'
        break;
    }
    Array.from(legendDiv).forEach((legend_div, index) => {
        //console.log(legend_div)
        legend_div.style.display = 'none';
      });
    switch(eventLayer.name) {
      case 'Emisión de Metano por cría de Bovinos':
        legendDiv[2].style.display='block'
        break;
      case 'Emisión de Metano por cría de Porcinos':
        legendDiv[2].style.display='block'
        break;
      case 'Emisiones de Metano en SDF':
        legendDiv[1].style.display='block'
        break;
      case 'Acciones de Mitigación':
        legendDiv[0].style.display='block'
        break;
    }
    
    })
    // Función para ajustar el tamaño del ícono según el nivel de zoom
    function resizeMarkers() {
      var zoom = map.getZoom();
      // Cambiar el tamaño de cada marcador
      map.eachLayer(function(layer) {
        if (layer.options && layer.options.icon && layer.options.icon.options) {
          var newSize;
          console.log(layer)
          if (layer.options.group === 'Emisiones de Metano en SDF'||layer.options.group==='Acciones de Mitigación'  ) {
            newSize = zoom==9?15:zoom==10?25:
            zoom==11?35:zoom==12?40:zoom==13?60:80
            // Ajustar el tamaño del icono
          var iconSize = [newSize, newSize];
          layer.setIcon(
            L.icon({
              iconUrl: layer.options.icon.options.iconUrl,
              iconSize: iconSize,
              iconAnchor: [iconSize[0] / 2, iconSize[1] / 2]
            })
          );
          } 

          
        }
      });
    }

    // Llamar a resizeMarkers en eventos de zoom y al cargar el mapa
    map.on('zoomend', resizeMarkers);
    map.whenReady(resizeMarkers);
  }")

semarnath

saveWidget(semarnath,"SEMARNATH/mapa web/semarnath.html",title = "SEMARNATH",selfcontained = T)

#
leaflet()|>
  addLegendImage(images =c("SEMARNATH/mapa web/imagenes/alto.png","SEMARNATH/mapa web/imagenes/medio.png","SEMARNATH/mapa web/imagenes/bajo.png"),
                 labels = c("Alto","Medio","Bajo"),labelStyle = "margin:0;gap:0;padding:0",
                 orientation = 'vertical',
                 title = htmltools::tags$div('Simbología',
                                             style = 'font-size: 24px; text-align: center;display:block;margin:0'),
                 position = 'bottomright')|>
  htmlwidgets::onRender("function(el, x) {
    var map = this;
    var legendDiv = document.getElementsByClassName('info legend leaflet-control'); 
    legendDiv[0].children[1].children[0].style.margin=0
    legendDiv[0].children[0].children[0].style.marginBottom='50px'
    legendDiv[0].children[2].children[0].style.margin=0
    legendDiv[0].children[3].children[0].style.margin=0
                        }")
