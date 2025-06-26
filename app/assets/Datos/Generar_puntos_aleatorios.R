datos = sf::read_sf("app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_prueba.shp")

pozos_unicos = datos |> 
  dplyr::select(CVEGEO_LOC,NOM_MUN,NOMGEO_LOC, f_abast)

pozos_unicos = unique(pozos_unicos)

p = sf::st_cast(x = pozos_unicos, to = "POLYGON")    # Lo pasa a poligonos

p = p |> 
  dplyr::mutate(area = sf::st_area(x = p),
                area = as.numeric(area)) |> 
  dplyr::arrange(dplyr::desc(area)) |>     # Ordena apartir del area de mayor a menor
  dplyr::group_by(CVEGEO_LOC,NOM_MUN,NOMGEO_LOC,f_abast) |> 
  dplyr::slice_head(n = 1) |>   # Para quedarnos con el que tiene mayor area
  dplyr::select(-area) |> 
  dplyr::arrange(CVEGEO_LOC, f_abast)  |> 
  dplyr::ungroup()


conteo = p |> sf::st_drop_geometry() |> 
  dplyr::group_by(CVEGEO_LOC) |> 
  dplyr::summarise(conteo = dplyr::n())

unicas_geometrias = p |>  dplyr::distinct(geometry,.keep_all = T) |> 
  dplyr::select(CVEGEO_LOC, geometry)

conteo = merge(x = conteo, y = unicas_geometrias, by = "CVEGEO_LOC", all.x = T)
conteo = sf::st_as_sf(x = conteo, crs = sf::st_crs(unicas_geometrias))

genera_puntos_aleatorios = function(poligono,n){
  library(sf)
  if(n>0){
    p_proj = st_transform(poligono, crs = 3857) 
    frontera = p_proj |> st_geometry() |> 
      st_cast("MULTILINESTRING") |> st_cast("LINESTRING") |>
      st_line_sample(n=n,type = "regular") 
    frontera = sf::st_transform(x = frontera, crs = sf::st_crs(poligono))
    B = st_centroid(poligono)
    
    # Extraer las coordenadas del multipunto y del centroide
    coords_A = st_coordinates(frontera)
    coords_B = st_coordinates(B)
    
    # Calcular el promedio euclidiano entre cada punto del multipunto y el centroide
    coords_C = apply(X = coords_A, MARGIN = 1, FUN = function(coord) {
      x_avg = (coord[1] + coords_B[1]) / 2
      y_avg = (coord[2] + coords_B[2]) / 2
      c(x_avg, y_avg)
    })
    coords_C = t(coords_C)  # Transponer para obtener un formato adecuado
    
    # Crear el nuevo multipunto resultante
    C = st_sfc(st_multipoint(coords_C), crs = st_crs(poligono))
  }
  else{
    C = poligono|>st_geometry()|>st_cast("MULTILINESTRING")|>st_cast("LINESTRING")|> st_line_sample(n=n,type = "regular")
  }
}  



puntos_list = list()
for (i in 1:nrow(conteo)) {   #nrow(conteo)
  #i = 240
  cat("Vamos en", i, "que es", conteo$CVEGEO_LOC[i], "\n")
  interes = conteo[i,]
  aleatorio = genera_puntos_aleatorios(poligono = interes, n = interes$conteo)
  aleatorio = aleatorio |>  sf::st_as_sf() |>  dplyr::mutate(id = interes$CVEGEO_LOC)
  puntos_list[[i]] = aleatorio  
}
puntos_sf = dplyr::bind_rows(puntos_list)
separador = sf::st_cast(x = puntos_sf, to = "POINT")

which(separador$id == "130480001")
separador = separador[-c(460:463),]    # Por el caso raro de 130480001 genero 4 puntos de mas

identical(separador$id, p$CVEGEO_LOC)


guardar = p |>  sf::st_drop_geometry() 
identical(separador$id, guardar$CVEGEO_LOC)

guardar = cbind(guardar, separador)
guardar = guardar |>  dplyr::select(CVEGEO_LOC:f_abast,x) |> 
  dplyr::rename(geometry = x)

guardar = sf::st_as_sf(x = guardar, crs = sf::st_crs(datos))

plot(datos$geometry)
plot(guardar$geometry, add = T, col = "red")

sf::write_sf(guardar, "app/assets/Datos/Datos_2012_2023_shp/Datos_2012_2023_puntos_aleatorios_unicos.shp")











# Extra
verificar = separador |>  sf::st_drop_geometry() |> 
  dplyr::group_by(id) |> 
  dplyr::summarise(conteo = dplyr::n())

colnames(verificar) = colnames(conteo)
verificar = as.data.frame(verificar)

conteo = conteo |>  sf::st_drop_geometry()

for (i in 1:nrow(verificar)) {
  if (identical(verificar[i, ], conteo[i, ]) == FALSE) {
    cat("Esta mal la columna", i, "\n")
  }
}

i = 240
identical(verificar[i, ], conteo[i, ])



i = 1
poligono = conteo[i,]

p_proj = st_transform(poligono, crs = 3857) 
frontera = p_proj |> st_geometry() |> 
  st_cast("MULTILINESTRING") |> st_cast("LINESTRING") |>
  st_line_sample(n=1,type = "regular") 
frontera = sf::st_transform(x = frontera, crs = sf::st_crs(poligono))
B = st_centroid(poligono)

# Extraer las coordenadas del multipunto y del centroide
coords_A = st_coordinates(frontera)
coords_B = st_coordinates(B)

# Calcular el promedio euclidiano entre cada punto del multipunto y el centroide
coords_C = apply(X = coords_A, MARGIN = 1, FUN = function(coord) {
  x_avg = (coord[1] + coords_B[1]) / 2
  y_avg = (coord[2] + coords_B[2]) / 2
  c(x_avg, y_avg)
})
coords_C = t(coords_C)  # Transponer para obtener un formato adecuado

# Crear el nuevo multipunto resultante
C = st_sfc(st_multipoint(coords_C), crs = st_crs(poligono))

