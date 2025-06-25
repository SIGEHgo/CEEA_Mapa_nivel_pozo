datos = sf::read_sf("app/assets/Datos/shp/Historicos_Acciones.shp")

base = readxl::read_xlsx("app/assets/Datos/DATOS_2012_2023.xlsx")

names(base)



interes = base |>  
  dplyr::select(`Arsenico
mg/L`:`Temperatura 
°C`) 
interes = interes |>  dplyr::select(-`Cloro residual 
mg/L`)
interes = names(interes)

interes = gsub(x = interes, pattern ="\n.*", replacement = "") |> stringr::str_trim()



municipal = datos
municipal = municipal |> 
  dplyr::mutate(Arsenico = sample(1:1000,84, replace=F),
                Bario = sample(1:1000,84, replace=F),
                Cadmio = sample(1:1000,84, replace=F),
                Cobre = sample(1:1000,84, replace=F),
                Hierro = sample(1:1000,84, replace=F),
                Manganeso = sample(1:1000,84, replace=F),
                Plomo = sample(1:1000,84, replace=F),
                Zinc = sample(1:1000,84, replace=F),
                Dureza_Total = sample(1:1000,84, replace=F),
                Fluoruros = sample(1:1000,84, replace=F),
                Nitratos = sample(1:1000,84, replace=F),
                Nitritos = sample(1:1000,84, replace=F),
                pH = sample(1:1000,84, replace=F),
                SDT = sample(1:1000,84, replace=F),
                Sulfatos = sample(1:1000,84, replace=F),
                Cloro_Total = sample(1:1000,84, replace=F),
                Conductividad = sample(1:1000,84, replace=F),
                Temperatura = sample(1:1000,84, replace=F),
                )



promedio = base |> 
  dplyr::mutate(Municipio = stringr::str_to_title(Municipio),
                Municipio = iconv(x = Municipio, from = "UTF-8", to = "ASCII//TRANSLIT"),
                Municipio = stringr::str_trim(Municipio)) |> 
  dplyr::select(Año, Municipio, `Cloro residual 
mg/L`) |> 
  dplyr::mutate(Id = paste0(Municipio, "_",Año))

promedio = promedio |> 
  dplyr::group_by(Id) |> 
  dplyr::summarise(promedio = max(as.numeric(`Cloro residual 
mg/L`),na.rm = T))






sf::write_sf(municipal, "app/assets/Datos/shp/Municipal_prueba.shp")







































####################
## Homologar base ##
####################

datos = readxl::read_xlsx("app/assets/Datos/DATOS_2012_2023.xlsx") 


mun = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")
mun = mun |>  
  dplyr::select(CVEGEO, NOM_MUN) |> 
  dplyr::mutate(Municipio = NOM_MUN,
                Municipio = stringr::str_to_title(Municipio),
                Municipio = iconv(x = Municipio, from = "UTF-8", to = "ASCII//TRANSLIT"),
                Municipio = stringr::str_trim(Municipio)
                ) |> 
  sf::st_drop_geometry()

datos = datos |> 
  dplyr::mutate(Municipio = stringr::str_to_title(Municipio),
                Municipio = iconv(x = Municipio, from = "UTF-8", to = "ASCII//TRANSLIT"),
                Municipio = stringr::str_trim(Municipio)) 

unicos = datos$Municipio |>  unique()
unicos[which(!unicos %in% mun$Municipio)]

datos = datos  |> 
  dplyr::mutate(Municipio = dplyr::case_when(
    Municipio == "San Agustin Tlaxiaca 2017" ~ "San Agustin Tlaxiaca",
    Municipio == "Santago Tulantepec" ~ "Santiago Tulantepec De Lugo Guerrero",
    Municipio == "San Agustin Tlaxiaca 2019" ~ "San Agustin Tlaxiaca",
    Municipio == "San Aguatin Tlaxiaca" ~ "San Agustin Tlaxiaca",
    Municipio == "Zacualtipan" ~ "Zacualtipan De Angeles",
    Municipio == "Tepehuacan" ~ "Tepehuacan De Guerrero",
    Municipio == "Huasca" ~ "Huasca De Ocampo",
    Municipio == "Cuautepec" ~ "Cuautepec De Hinojosa",
    Municipio == "Santiago Tulantepec (Chignahuapan)" ~ "Santiago Tulantepec De Lugo Guerrero",
    Municipio == "Mixquiahuala" ~ "Mixquiahuala De Juarez",
    Municipio == "Huejutla" ~ "Huejutla De Reyes",
    Municipio == "Atitalaquia (Atotonilco De Tula)" ~ "Atitalaquia",
    Municipio == "Jacala" ~ "Jacala De Ledezma",
    Municipio == "Agua Blanca" ~ "Agua Blanca De Iturbide",
    Municipio == "Zacualtipan Angeles" ~ "Zacualtipan De Angeles",
    Municipio == "Villas De Tezontepec" ~ "Villa De Tezontepec",
    Municipio == "Tepeji Del Rio" ~ "Tepeji Del Rio De Ocampo",
    Municipio == "Santiago Tulantepec" ~ "Santiago Tulantepec De Lugo Guerrero",
    Municipio == "Molango" ~ "Molango De Escamilla",
    Municipio == "Juarez De Hidalgo" ~ "Juarez Hidalgo",
    TRUE ~ Municipio
    ))

colnames(mun)[1] = "CVEGEO_MUN"

datos = merge(x = datos, y = mun, by = "Municipio", all.x = T)

###########################
### Localidades Urbanas ###
###########################

loc = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp1/13l.shp")
loc = loc[-which(loc$CVEGEO == 130350034),]  # Para evitar error de repetido es 13035_Metepec
loc = loc |>  
  dplyr::select(CVEGEO, NOMGEO) |> 
  dplyr::mutate(Localidad = NOMGEO,
                Localidad = stringr::str_to_title(Localidad),
                Localidad = iconv(x = Localidad, from = "UTF-8", to = "ASCII//TRANSLIT"),
                Localidad = stringr::str_trim(Localidad),
                Id = paste0(substr(x = CVEGEO, start = 1, stop = 5), "_", Localidad)
  ) 






datos = datos |> 
  dplyr::mutate(Localidad = stringr::str_to_title(Localidad),
                Localidad = iconv(x = Localidad, from = "UTF-8", to = "ASCII//TRANSLIT"),
                Localidad = stringr::str_trim(Localidad),
                Id = paste0(CVEGEO_MUN, "_", Localidad)) 

unicos = datos$Id |>  unique()
unicos[which(!unicos %in% loc$Id)]


interes = unicos[which(!unicos %in% loc$Id)] 
for (i in seq_along(interes)) {
  cat('Id == "', interes[i], '" ~ Id,\n', sep = "")
}





datos = datos |> 
  dplyr::mutate(Id = dplyr::case_when(
    Id == "13001_Colonia 28 De Mayo" ~ "13001_28 De Mayo (Santa Rosa) [Colonia]",
    Id == "13001_San Bartolo El Llano" ~ "13001_San Bartolo (El Llano)",
    Id == "13001_Col. 28 De Mayo (Santa Rosa)" ~ "13001_28 De Mayo (Santa Rosa) [Colonia]",
    Id == "13001_El Sabino" ~ "13001_El Sabino (La Barranca)",
    
    Id == "13002_Barrio Tlatzintla" ~ "13002_Tlatzintla",
    Id == "13002_Barrio Tlaltegco" ~ "13002_Tlaltegco (Venta Quemada)",
    Id == "13002_Apapaxtla El Grande" ~ "13002_Apapaxtla El Grande (Altamira)",
    Id == "13002_Tlamimolpan" ~ "13002_Tlamimilolpa",
    
    Id == "13003_Benito Juarez" ~ Id,              # No esta
    Id == "13003_El Huaxto" ~ "13003_El Huaxtho",
    Id == "13003_Agua Blanca De Iturbide" ~ Id,    # No esta
    
    Id == "13004_Ejido San Pedrito" ~ "13004_Ejido San Pedrito (Potrerillos)",
    Id == "13004_Vicente Guerrero" ~ Id,       # No esta
    Id == "13004_La Laguna" ~ Id,             #No esta
    Id == "13004_El Remudadero" ~ Id,         # No esta
    Id == "13004_Ajacuba" ~ Id,               # No esta(posible mal localizado)
    
    Id == "13005_San Francisco Sacachichilco" ~ Id,   #No esta(posible mal localizado)
    Id == "13005_La Huapilla" ~ Id,        #No esta(posible mal localizado)
    
    Id == "13006_Almolaya" ~ Id,         #No esta(posible mal localizado)
    
    Id == "13007_Almolaya" ~ "13007_Almoloya",
    Id == "13007_Apan" ~ Id,            #No esta(posible mal localizado)
    
    Id == "13008_Tezoyo" ~ Id,           #No esta
    Id == "13008_Atlapexco" ~ Id,       #No esta(posible mal localizado)
    Id == "13008_Tecolotitla" ~ Id,    #No esta(posible mal localizado)
    
    Id == "13010_San Geronimo Tlamaco" ~ "13010_Tlamaco (San Geronimo Tlamaco)",
    Id == "13010_Tlaminulpa" ~ "13010_Tlalminulpa",
    Id == "13010_San Jose Bojay El Grande" ~ "13010_Unidad Habitacional Antonio Osorio De Leon (Bojay)",  #Duda
    Id == "13010_El Cardonal" ~ "13010_Cardonal",
    Id == "13010_Tlamaco" ~ "13010_Tlamaco (San Geronimo Tlamaco)",
    Id == "13010_San Jeronimo Tlamaco" ~ "13010_Tlamaco (San Geronimo Tlamaco)",
    Id == "13010_Atitalaquia (El Tablon)" ~ "13010_Atitalaquia",
    
    Id == "13011_Santa Maria Amajac" ~ Id,    #No esta(posible mal localizado)
    Id == "13011_Agua Limpia" ~ Id,           #No esta(posible mal localizado)
    Id == "13011_Calnali" ~ Id,               #No esta(posible mal localizado)
    Id == "13011_Tenexco 1" ~ "13011_Tenexco I",
    Id == "13011_La Pena" ~ Id,               # No esta
    Id == "13011_El Panteon" ~ Id,            # No esta
    
    Id == "13013_San Jose Bojay" ~ Id,        # No esta
    
    Id == "13012_Pozuelos" ~ Id,              #No esta(posible mal localizado)
    Id == "13012_Los Banos De Santa Maria Amajac" ~ "13012_Santa Maria Amajac",
    Id == "13012_Pezmatlan" ~ Id,             #No esta(posible mal localizado)
    Id == "13012_El Paso De Amajac" ~ "13012_Paso De Amajac",
    Id == "13012_Los Tapancos" ~ Id,          # No esta
    Id == "13012_La Loma Del Zapote" ~ Id,    # No esta
    
    Id == "13014_San Antonio Sabanillas" ~ Id, #No esta(posible mal localizado)
    Id == "13014_Texcaco" ~ Id,                #No esta
    Id == "13014_Santa Maria Amealco" ~ Id,    #No esta(posible mal localizado) 
    Id == "13014_San Andres" ~ "13014_San Andres (San Andres Chichayotla)",
    Id == "13014_Chicuapa" ~ Id,              # No esta
    Id == "13014_Texcaco (Sagrado Corazon De Jesus)" ~ Id,    #No esta
    Id == "13014_Barrio San Juan Mezcalpa" ~ Id,   # No esta
    Id == "13014_Barrio Nuevo" ~ Id,          # No esta
    
    Id == "13015_Emilio Hernandez La Florida" ~ "13015_La Florida", 
    Id == "13015_Chapantongo" ~ Id,             #No esta(posible mal localizado) 
    Id == "13015_El Cubo" ~ "13015_Manzana Del Cubo",
    Id == "13015_Iglesia Vieja (Iglesia Nueva)" ~ Id, #No esta(posible mal localizado) 
    
    Id == "13017_La Estancia" ~ Id,   #No esta(posible mal localizado) 
    Id == "13017_El Huizachal" ~ "13017_El Huizachal (San Isidro)",
    Id == "13017_Neblinas" ~ Id, #No esta(posible mal localizado) 
    
    Id == "13018_El Sabino" ~ Id,   #No esta
    Id == "13018_El Aguacate" ~ Id, #No esta
    Id == "13018_Xayahualulco" ~ Id, #No esta(posible mal localizado) 
    
    Id == "13019_El Sabino" ~ Id,  # No esta
    Id == "13019_San Juan Hueyapan" ~ Id, #No esta(posible mal localizado)
    Id == "13019_El Dontzhi (Col. Alamos)" ~ "13019_El Dontzhi (Colonia Alamos)",
    Id == "13019_Xayahualulco" ~ Id, #No esta(posible mal localizado)
    Id == "13019_Dontzhi" ~ "13019_El Dontzhi (Colonia Alamos)",
    Id == "13019_Tecaxtepec" ~ "13019_Texcatepec",
    
    Id == "13016_San Jeronimo" ~ Id,   # No esta
    Id == "13016_Cuautepec De Hinojosa" ~ "13016_Cuautepec",
    Id == "13016_Tanque Santa Maria Nativas" ~ "13016_Santa Maria Nativitas",
    Id == "13016_San Pedro Gilo (Gilo)" ~ Id,  #No esta(posible mal localizado)
    Id == "13016_El Bocja" ~ Id,           #No esta(posible mal localizado)
    
    Id == "13009_Col. 20 De Noviembre" ~ "13009_20 De Noviembre [Colonia]",
    Id == "13009_Chimalpa" ~ Id,  #No esta(posible mal localizado)
    Id == "13009_Colonia 20 De Mayo" ~ Id,    #No esta
    Id == "13009_Tepatepec" ~ Id,   #No esta(posible mal localizado)
    Id == "13009_Colonia 20 De Noviembre" ~ "13009_20 De Noviembre [Colonia]",
    Id == "13009_Eloxochitlan" ~ Id,   #No esta
    Id == "13009_Bocya" ~ Id,     #No esta
    
    Id == "13020_Almolaya" ~ Id,   #No esta
    Id == "13020_San Pedro Gilo" ~ "13020_San Pedro Gilo (Gilo)",
    Id == "13020_San Miguel Regla" ~ Id,    #No esta(posible mal localizado)
    Id == "13020_La Loma" ~ Id,
    Id == "13020_Almoloya" ~ Id,    #No esta
    
    Id == "13021_Jose Ma. Morelos" ~ "13021_Jose Maria Morelos (San Jose)",
    
    Id == "13022_San Miguel Nopala" ~ "13022_San Miguel Nopalapa",
    
    Id == "13023_El Quince" ~ Id,    #No esta
    Id == "13023_Lazaro Cardenas (El Mexe)" ~ Id,  #No esta
    Id == "13023_Los Cerezos" ~ Id,  #No esta(posible mal localizado)
    
    Id == "13024_Huazalingo" ~ Id,  #No esta(posible mal localizado)
    Id == "13024_Tamoyon I" ~ Id,  ##No esta(posible mal localizado)
    
    Id == "13025_Huazalingo" ~ Id,  #No esta(posible mal localizado)
    Id == "13025_Barrio Hondo" ~ Id, #No esta
    Id == "13025_Huehuetla" ~ Id,  #No esta(posible mal localizado)
    
    Id == "13026_Huejutla De Reyes" ~ Id, #No esta(posible mal localizado)
    Id == "13026_Huehuetla" ~ Id, #No esta(posible mal localizado)
    
    Id == "13027_La Esperanza" ~ "13027_La Esperanza Numero 1",
    Id == "13027_Minas Viejas" ~ Id,  #No esta(posible mal localizado)
    Id == "13027_Bosque" ~ Id,  #No esta
    Id == "13027_Teacala" ~ Id,  #No esta
    
    Id == "13028_Chililico (Upn)" ~ "13028_Chililico",
    Id == "13028_Huejutla" ~ "13028_Huejutla De Reyes",
    Id == "13028_Tlahuatempa" ~ Id,  #No esta
    Id == "13028_Tezohual" ~ Id,   # No esta
    Id == "13028_Jaltocan" ~ Id, #No esta(posible mal localizado)
    Id == "13028_Agua Fria Grande" ~ Id,   #No esta(posible mal localizado)
    Id == "13028_Macuxtepleta" ~ "13028_Macuxtepetla",
    
    Id == "13030_Santa Ana" ~ Id,  #No esta
    Id == "13030_Botanguedho" ~ "13030_Botenguedho",
    Id == "13030_La Huerta" ~ "13030_La Huerta Capula",
    Id == "13030_Barrio Fitzhi" ~ Id, # No esta
    Id == "13030_Bangando" ~ "13030_Bangandho",
    
    Id == "13031_Juarez" ~ Id,     #No esta
    Id == "13031_El Aguaje" ~ Id,  #No esta(posible mal localizado)
    Id == "13031_Puerto De La Zorra" ~ Id,   # No esta
    Id == "13031_El Cobre (Nicolas Flores)" ~ "13031_San Nicolas",
    Id == "13031_Revolucion Mexicana" ~ Id,   #No esta
    
    Id == "13032_Juarez" ~ Id,         #No esta
    Id == "13032_Puerto La Piedra" ~ Id,   # No esta
    
    Id == "13033_Lolotla" ~ Id,  #No esta(posible mal localizado)
    Id == "13033_La Mision" ~ Id, #No esta
    Id == "13033_Juarez" ~ "13033_Juarez Hidalgo",
    
    Id == "13040_Lagunita De Pilas" ~ "13040_Las Pilas",
    Id == "13040_Barrio Santa Maria" ~ Id,    # No esta
    Id == "13040_Los Castros" ~ Id,           # No esta
    Id == "13040_Tetlapaya" ~ Id,             #No esta(posible mal localizado)
    Id == "13040_Metepec" ~ Id,               #No esta
    
    Id == "13034_Metepec" ~ Id,               #No esta(posible mal localizado)
    Id == "13034_Santiago Tezontlale" ~ Id,   #No esta(posible mal localizado)
    Id == "13034_Itztazacuala" ~ Id,          #No esta(posible mal localizado)
    Id == "13035_Pena Colorada" ~ Id,         #No esta
    Id == "13035_Estacionde Apulco" ~ "13035_Estacion De Apulco",
    Id == "13035_Carboneras" ~ Id,            #No esta
    Id == "13035_Tecruz Copaza" ~ Id,         #No esta
    
    Id == "13037_Meztitlan" ~ "13037_Metztitlan",
    Id == "13037_Mineral Del Chico" ~ Id,    #No esta(posible mal localizado)
    Id == "13037_Colonia Tenhe" ~ Id,        #No esta(posible mal localizado)
    Id == "13037_Tecruz Cosapa" ~ "13037_Tecruz Cozapa",        
    Id == "13037_Pie De La Cuesta" ~ Id,     #No esta(posible mal localizado)
    Id == "13037_Meztitln" ~ "13037_Metztitlan",
    
    Id == "13038_Manzanas Iii" ~ Id,         #No esta
    Id == "13038_Plan Grande" ~ Id,          #No esta
    Id == "13038_Colonia Tenhe" ~ Id,        #No esta(posible mal localizado)
    Id == "13038_Carbonera" ~ "13038_Carboneras",
    Id == "13038_El Durazno" ~ Id,           #No esta
    Id == "13038_El Paraiso" ~ Id,           #No esta
    
    Id == "13039_Llano Grande" ~ Id,         #No esta
    
    Id == "13041_Cinta Larga Seccion 22" ~ Id,   #No esta
    Id == "13041_Colonia El Tenhe" ~ "13041_Colonia Tenhe",
    Id == "13041_Molango" ~ Id,              #No esta(posible mal localizado)
    Id == "13041_Mixquiahuala" ~ "13041_Mixquiahuala De Juarez",          #Pendiente
    Id == "13041_Mixquiahuala (Col. Taxhuada)" ~ Id,    #Pendiente
    Id == "13041_Mixquiahuala  (La Pena)" ~ Id,         #Pendiente
    Id == "13041_Mixquiahuala  (Donfhi)" ~ Id,          #Pendiente
    Id == "13041_Colonia El Durazno" ~ "13041_El Durazno",
    
    Id == "13042_Molango" ~ "13042_Molango De Escamilla",
    Id == "13042_Santa Cruz" ~ Id,          #No esta(posible mal localizado)
    Id == "13042_Maravillas" ~ Id,          #No esta(posible mal localizado)
    Id == "13042_El Dothu" ~ Id,            #No esta
    
    Id == "13043_Omitlan De Juarez" ~ Id,   #No esta(posible mal localizado)
    Id == "13043_El Dathu" ~ Id,            #No esta
    Id == "13043_Loma Del Progreso" ~ Id,   #No esta(posible mal localizado)
    
    Id == "13044_Canada" ~ "13044_La Canada",
    Id == "13044_El Pedregoso" ~ Id,        #No esta(posible mal localizado)
    Id == "13044_El Tecojote" ~ Id,         #No esta
    Id == "13044_Pacula" ~ Id,              #No esta(posible mal localizado)
    Id == "13044_Mizquiapan" ~ Id,          #No esta
    Id == "13044_El Tejocote" ~ Id,         #No esta(posible mal localizado)
    Id == "13044_El Sauz" ~ Id,             #No esta
    Id == "13044_Ojo De Agua" ~ Id,         #No esta
    
    Id == "13045_Pisaflores" ~ Id,          #No esta(posible mal localizado)
    Id == "13045_Canoas" ~ Id,              #No esta(posible mal localizado)
     
    Id == "13047_Metzquititlan" ~ Id,       #No esta
    Id == "13047_Pisaflores" ~ Id,          #No esta(posible mal localizado)
    Id == "13047_Vicente Guerrero" ~ "13047_Vicente Guerrero (Presidio)",
    Id == "13047_Jilipan" ~ "13047_Jiliapan",
    
    Id == "13049_El Limonsito" ~ "13049_El Limoncito",
    Id == "13049_San Bartolo Tutotepec" ~ Id,   #No esta
    Id == "13049_Cieneguillas" ~ Id,            #No esta(posible mal localizado)
    Id == "13049_Pisa Flores" ~ "13049_Pisaflores",
    Id == "13049_Cerrito Del Carmen" ~ "13049_Cerro Del Carmen",
    
    Id == "13050_Progreso" ~ "13050_Progreso De Obregon",
    Id == "13050_Col. Tenhe (Mixquiahuala)" ~ Id,   #No esta(posible mal localizado)
    
    Id == "13036_Orizatlan" ~ Id,          #No esta(posible mal localizado)
    Id == "13036_Zahustipan" ~ "13036_Zahuastipan",
    Id == "13036_San Agustin Metzquititlan" ~ "13036_Mezquititlan",
    Id == "13036_Metzquititlan" ~ "13036_Mezquititlan",
    Id == "13036_San Bartolo Tutotepec" ~ Id,   #No esta
    Id == "13036_Metztquititlan" ~ "13036_Mezquititlan",
    Id == "13036_San Juan Solis" ~ Id,   #No esta
    Id == "13036_Ixcuinquilapilco" ~ Id, #No esta(posible mal localizado)
    Id == "13036_San Bernardo" ~ Id,   #No esta
    
    Id == "13052_San Francisco Tecajite" ~ "13052_San Francisco Tecajique",
    Id == "13052_San Agustin Tlaxiaca (Mexiquito)" ~ Id,           #Pendiente
    Id == "13052_Rancho Los Garambullos" ~ Id,                     #No esta
    Id == "13052_San Agustin Tlaxiaca (Barrio Casa Grande)" ~ Id,  #Pendiente
    Id == "13052_Barrio El Tepozon" ~ Id,                          #No esta
    Id == "13052_Guadalupe Victoria" ~ "13052_Guadalupe Victoria [Colonia]",
    Id == "13052_Chapultepec De Pozo" ~ "13052_Chapultepec De Pozos",
    Id == "13052_Colonia Nueva Tlaxiaca" ~ "13052_Nueva Tlaxiaca",
    Id == "13052_Ixcuinquitlapico" ~ "13052_Ixcuinquitlapilco",
    Id == "13052_San Agustin Tlaxiaca  (Barrio Casa Grande)" ~ Id, #Pendiente
    
    Id == "13053_San Salvador" ~ Id,    #No esta(posible mal localizado)
    Id == "13053_Orizatlan" ~ Id,       #No esta
    
    Id == "13046_Orizatlan" ~ "13046_San Felipe Orizatlan",
    Id == "13046_Acuapa" ~ Id,             # No esta
    Id == "13046_San Salvador" ~ Id,       # No esta
    Id == "13046_Pacheco De Allende" ~ Id,  #No esta(posible mal localizado)
    Id == "13046_Manzanas Iii" ~ Id,       # No esta
    
    Id == "13054_El Puerto Lazaro Cardenaz" ~ "13054_El Puerto Lazaro Cardenas",
    Id == "13054_Pacheco" ~ "13054_Pacheco De Allende",
    Id == "13054_El Bandho" ~ "13054_El Bondho",
    Id == "13054_Teofni" ~ "13054_Teofani",
    Id == "13054_Yolotepec" ~ Id,   #No esta(posible mal localizado)
    Id == "13054_Zaragoza" ~ "13054_San Antonio Zaragoza",
    Id == "13054_Puerto El Lazaro Cardenas" ~ "13054_El Puerto Lazaro Cardenas",
    Id == "13054_Ventoquipa" ~ Id,  #No esta(posible mal localizado)
    
    Id == "13055_La Blanca" ~ "13055_La Blanca (Taxtho La Blanca)",
    Id == "13055_Palmar" ~ "13055_El Palmar",
    Id == "13055_Portezuelo" ~ Id,           # No esta
    Id == "13055_La Joya (Fraccionamiento)" ~ Id,   #No esta
    
    Id == "13056_Potrerillos" ~ Id, #No esta
    Id == "13056_La Viejita" ~ Id,  #No esta
    Id == "13056_Tasquillo" ~ Id,   #No esta(posible mal localizado)
    Id == "13056_Plan De Ayala" ~ Id, #No esta(posible mal localizado)
    
    Id == "13057_Susto" ~ "13057_El Susto",
    
    Id == "13058_El Bondhi" ~ "13058_Bondhi",
    Id == "13058_Tenango De Doria" ~ Id,  #No esta(posible mal localizado)
    Id == "13058_Frente Al Vivero" ~ Id,  #No esta
    
    Id == "13059_Col. 20 De Noviembre" ~ Id,  #No esta(posible mal localizado)
    Id == "13059_Barrio Morales" ~ Id,  #No esta
    Id == "13059_Ejido Tagui 3 (Localidad)" ~ Id,  #No esta
    Id == "13059_Rancho Pathesito" ~ Id,  #No esta
    
    Id == "13060_Acatlajapa" ~ Id,   #No esta(posible mal localizado)
    Id == "13060_La Cuarta Manzana" ~ Id, #No esta(posible mal localizado)
    Id == "13060_Amola De Ocampo" ~ Id,   #No esta
    
    Id == "13061_Fco. Sarabia (Corralillos)" ~ "13061_Francisco Sarabia (Corralillos)",
    Id == "13061_La Estacion Ocho" ~ Id,  #No esta
    
    Id == "13062_Xilipa" ~ "13062_Xilitla",
    Id == "13062_Manantiales De Cerro Colorado" ~ Id,   #No esta(posible mal localizado)
    Id == "13062_La Ermita" ~ Id,  #No esta(posible mal localizado)
    
    Id == "13063_Tepeji Del Rio" ~ "13063_Tepeji Del Rio De Ocampo",
    Id == "13063_Localidad El Eden" ~ Id,   #No esta
    Id == "13063_Tianguistengo" ~ "13063_Tianguistengo (La Romera)",
    Id == "13063_Col. Sta Maria Quelites" ~ "13063_Santa Maria Quelites",
    
    Id == "13064_Tianguistengo" ~ Id,   #No esta
    
    Id == "13067_Colonia San Juan Solis" ~ "13067_San Juan [Colonia]",
    Id == "13067_San Juan (Colonia)" ~ "13067_San Juan [Colonia]",
    Id == "13067_Tlahuelilpan" ~ Id,  #No esta(posible mal localizado)
    
    Id == "13068_Las Cantinas" ~ Id,     #No esta
    Id == "13068_San Jose Las Cruces" ~ Id, #No esta
    Id == "13068_Tlahuelilpan" ~ Id,  #No esta(posible mal localizado)
    
    Id == "13070_Tlahuiltepa" ~ Id,   #No esta(posible mal localizado)
    Id == "13070_Col. Cerro De La Cruz" ~ "13070_Cerro De La Cruz [Colonia]",
    Id == "13070_Tlanchinol" ~ Id,  #No esta(posible mal localizado)
    Id == "13070_Colonia Cerro De La Cruz" ~ "13070_Cerro De La Cruz [Colonia]",
    
    Id == "13071_Tlanchinol" ~ Id,  #No esta(posible mal localizado)
    Id == "13071_Tulancingo" ~ Id,  #No esta(posible mal localizado)
     
    Id == "13073_Tanchinol" ~ "13073_Tlanchinol",
    Id == "13073_Atlalco" ~ Id,     #No esta(posible mal localizado)
    Id == "13073_Tulancingo" ~ Id,  #No esta(posible mal localizado)
    
    Id == "13074_Teltipan" ~ "13074_Teltipan De Juarez",
    
    Id == "13076_San Miguel Las Piedras (1ra Seccion)" ~ "13076_San Miguel De Las Piedras Primera Seccion",
    Id == "13076_Santa Ana Ahuehupan" ~ "13076_Santa Ana Ahuehuepan",
    
    Id == "13077_Tepaltzingo" ~ "13077_Tepalzingo",
    Id == "13077_Tulancingo De Bravo" ~ "13077_Tulancingo",
    Id == "13077_Xochitl" ~ Id,      #No esta(posible mal localizado)
    Id == "13077_Xochicoatlan" ~ Id, #No esta(posible mal localizado)
    
    Id == "13066_Villa De Tezontepec" ~ "13066_Tezontepec",
    
    Id == "13078_Xochicoatlan" ~ Id,   #No esta(posible mal localizado)
    Id == "13078_Santa Teresa" ~ Id,   #No esta(posible mal localizado)
    Id == "13078_Huitznopala" ~ Id,    #No esta(posible mal localizado)
    
    Id == "13079_Chacalapa" ~ Id,    #No esta
    Id == "13079_Zacualtipan" ~ Id,  #No esta(posible mal localizado)
     
    Id == "13080_Xindho San Pedro" ~ Id,  #No esta
    Id == "13080_Tepeyocatitla" ~ "13080_Pepeyocatitla",
    Id == "13080_Huetacetl" ~ Id,     #No esta
    
    Id == "13081_Doxhi" ~ Id,     #No esta(posible mal localizado)
    
    Id == "13084_Doxhi" ~ "13084_Doxthi La Sabina (La Sabina)",
    Id == "13084_Bonhu" ~ Id,    #No esta
    Id == "13084_Coaxithi" ~ Id,   #No esta(posible mal localizado)
    Id == "13084_Lazaro Cardenas" ~ "13084_Lazaro Cardenas (Remedios)",
    TRUE ~ Id
  ))



loc_filtrado = loc |> 
  dplyr::group_by(Id) |> 
  dplyr::filter(dplyr::n()>1)


unicos = datos$Id |>  unique()
unicos[which(!unicos %in% loc$Id)]


names(loc)[1:2] = c("CVEGEO_LOC", "NOMGEO_LOC")

loc = loc |>  dplyr::select(CVEGEO_LOC,NOMGEO_LOC,Id,geometry)

datos_merge = merge(x = datos, y = loc, by = "Id", all.x = T)

datos_merge = datos_merge |>  
  dplyr::select(Año,NOM_MUN,CVEGEO_LOC,NOMGEO_LOC, Localidad, `Fuente de Abastecimiento`:`Temperatura 
°C`) |>  
  dplyr::arrange(Año,NOM_MUN)

write.csv(datos_merge, "app/assets/Datos/DATOS_2012_2023_Localidades_Urbanas_Correctas.csv",row.names = F, fileEncoding = "latin1")
# Para geometria unir desde loc con geo apartir de CVEGEO_LOC
####################################
### El repetido es 13035_Metepec ###
####################################


















###########################
### Localidades Rurales ###
###########################

datos = read.csv("app/assets/Datos/DATOS_2012_2023_Localidades_Urbanas_Correctas.csv", fileEncoding = "latin1")
datos = datos |> 
  dplyr::filter(is.na(CVEGEO_LOC)) |> 
  dplyr::mutate(Id = paste0(NOM_MUN, "_", Localidad)) |> 
  dplyr::select(-CVEGEO_LOC,-NOMGEO_LOC)


localidades_2 = sf::read_sf("../../Importantes_documentos_usar/Localidades/shp2/13lpr.shp", options = "ENCODING=LATIN1")
localidades_2 = localidades_2 |> 
  dplyr::select(CVEGEO, NOMGEO) |> 
  dplyr::mutate(NOMGEO_L = stringr::str_to_title(NOMGEO),
                NOMGEO_L = iconv(x = NOMGEO_L, from = "UTF-8", to = "ASCII//TRANSLIT"),
                NOMGEO_L = stringr::str_trim(NOMGEO_L)
  ) |> 
  dplyr::mutate(CVE_MUN = substr(x = CVEGEO, start = 3, stop = 5)) 


mun = sf::read_sf("../../Importantes_documentos_usar/Municipios/municipiosjair.shp")
mun = mun |>  dplyr::select(CVE_MUN,NOM_MUN) |>  sf::st_drop_geometry()

localidades_2 = merge(x = localidades_2, y = mun, by = "CVE_MUN")
localidades_2 = localidades_2 |> 
  dplyr::mutate(Id = paste0(NOM_MUN, "_", NOMGEO_L)) |> 
  sf::st_drop_geometry()




library(fuzzyjoin);
library(dplyr);
interes = datos |>  dplyr::select(Id) 
prueba = stringdist_join(interes, localidades_2 |>  dplyr::select(Id,CVEGEO), 
                by = "Id",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist")  |> 
  group_by(Id.x)  |> 
  slice_min(order_by = dist, n = 1) |> 
  dplyr::filter(dist > 0) |>  
  dplyr::arrange(dist)

# Correctamente escrita es la Id.y, dado que la original es Id.x






unicos = datos$Id |>  unique()
unicos[which(!unicos %in% localidades_2$Id)]

interes = unicos[which(!unicos %in% localidades_2$Id)] 
for (i in seq_along(interes)) {
  cat('Id == "', interes[i], '" ~ Id,\n', sep = "")
}


for (i in 1:nrow(prueba)) {
  cat('Id == "', prueba$Id.x[i], '" ~ Id,\n', sep = "")
}

write.csv(prueba, "app/assets/Datos/Localidad_Rural_basarse.csv", row.names = F, fileEncoding = "latin1")
# Correctamente escrita es la Id.y, dado que la original es Id.x

datos = datos  |> 
  dplyr::mutate(Id = dplyr::case_when(
    Id == "Nopala de Villagrán_El Tecojote" ~ "Nopala de Villagrán_El Tejocote",
    Id == "Nicolás Flores_El Dathu" ~ "Nicolás Flores_El Dothu",
    Id == "San Felipe Orizatlán_Acuapa" ~ Id,
    Id == "Yahualica_Huetacetl" ~ "Yahualica_Hueyactetl",
    Id == "Calnali_Texcaco" ~ Id,                                     # Si existe en google
    Id == "Eloxochitlán_Almolaya" ~ "Eloxochitlán Almoloya", 
    Id == "Huehuetla_Bosque" ~ "Huehuetla_El Bosque",
    Id == "Tianguistengo_Tlahuelilpan" ~ "Tianguistengo_Tlahuiltepa",
    Id == "Nopala de Villagrán_Pacula" ~ Id,                          # Pendiente
    Id == "Molango de Escamilla_Maravillas" ~ Id,
    Id == "Santiago Tulantepec de Lugo Guerrero_La Viejita" ~ Id,     
    Id == "Francisco I. Madero_Lazaro Cardenas (El Mexe)" ~ "Francisco I. Madero_Lazaro Cardenas (El Mexe) [Colonia]",
    Id == "Huautla_Huazalingo" ~ Id,                                 #Duda
    Id == "El Arenal_Chimalpa" ~ "El Arenal_Chimilpa",
    Id == "Xochiatipan_Xochicoatlan" ~ Id,
    Id == "Apan_Tezoyo" ~ "Apan_El Tezoyo",
    Id == "Atotonilco el Grande_Los Tapancos" ~ Id,
    Id == "Tepehuacán de Guerrero_La Ermita" ~ Id,
    Id == "Huejutla de Reyes_Jaltocan" ~ Id,
    Id == "Santiago Tulantepec de Lugo Guerrero_Tasquillo" ~ Id,
    Id == "Santiago Tulantepec de Lugo Guerrero_Plan De Ayala" ~ Id,
    Id == "Santiago Tulantepec de Lugo Guerrero_Plan De Ayala" ~ Id,
    Id == "Cuautepec de Hinojosa_El Bocja" ~ Id,
    Id == "San Felipe Orizatlán_San Salvador" ~ Id,
    Id == "Xochicoatlán_Zacualtipan" ~ Id,
    Id == "Francisco I. Madero_Los Cerezos" ~ Id,
    Id == "Santiago Tulantepec de Lugo Guerrero_Potrerillos" ~ Id,
    Id == "San Agustín Metzquititlán_San Bernardo" ~ Id,
    Id == "Molango de Escamilla_Santa Cruz" ~ Id,
    Id == "San Agustín Metzquititlán_San Juan Solis" ~ Id,
    Id == "San Bartolo Tutotepec_Orizatlan" ~ Id,
    Id == "Zacualtipán de Ángeles_Doxhi" ~ Id,
    Id == "San Bartolo Tutotepec_San Salvador" ~ Id,
    Id == "Cardonal_Iglesia Vieja (Iglesia Nueva)" ~ Id,
    Id == "Huejutla de Reyes_Tlahuatempa" ~ Id,
    Id == "El Arenal_Bocya" ~ "El Arenal_El Bocja",
    Id == "Tlahuiltepa_Tulancingo" ~ Id,
    Id == "San Agustín Metzquititlán_Ixcuinquilapilco" ~ Id,
    Id == "Atotonilco el Grande_Pozuelos" ~ Id,
    Id == "Atlapexco_La Pena" ~ Id,
    Id == "Cuautepec de Hinojosa_San Jeronimo" ~ Id,
    Id == "Cuautepec de Hinojosa_San Jeronimo" ~ Id,
    Id == "Mixquiahuala de Juárez_Molango" ~ Id,
    Id == "Mixquiahuala de Juárez_Molango" ~ Id,
    Id == "Mixquiahuala de Juárez_Molango" ~ Id,
    Id == "Mixquiahuala de Juárez_Molango" ~ Id,
    Id == "Mixquiahuala de Juárez_Molango" ~ Id,
    Id == "Mixquiahuala de Juárez_Molango" ~ Id,
    Id == "San Agustín Metzquititlán_Orizatlan" ~ Id,
    Id == "Mineral del Chico_El Paraiso" ~ Id,
    Id == "Tlanchinol_Atlalco" ~ Id,
    Id == "Francisco I. Madero_El Quince" ~ Id,
    Id == "Molango de Escamilla_El Dothu" ~ Id,
    Id == "Tlanchinol_Tulancingo" ~ Id,
    Id == "San Agustín Tlaxiaca_Barrio El Tepozon" ~ "San Agustín Tlaxiaca_El Tepozan",
    Id == "San Agustín Tlaxiaca_Rancho Los Garambullos" ~ "San Agustín Tlaxiaca_Los Garambullos",
    Id == "Atotonilco de Tula_San Jose Bojay" ~ Id,
    Id == "Tlahuiltepa_Tlanchinol" ~ Id,
    Id == "Santiago de Anaya_Portezuelo" ~ Id,
    Id == "Calnali_San Antonio Sabanillas" ~ Id,
    Id == "Atotonilco el Grande_Pezmatlan" ~ Id,
    Id == "Mineral del Chico_El Durazno" ~ Id,
    Id == "Tulancingo de Bravo_Xochitl" ~ Id,
    Id == "Tulancingo de Bravo_Xochitl" ~ Id,
    Id == "Mineral del Chico_Manzanas Iii" ~ "Mineral del Chico_Las Manzanas",
    Id == "Mineral del Chico_Manzanas Iii" ~ "Mineral del Chico_Las Manzanas",
    Id == "Mineral del Chico_Manzanas Iii" ~ "Mineral del Chico_Las Manzanas",
    Id == "Mineral del Chico_Manzanas Iii" ~ "Mineral del Chico_Las Manzanas",
    Id == "Omitlán de Juárez_Canoas" ~ Id,
    Id == "Agua Blanca de Iturbide_Ajacuba" ~ Id,
    Id == "Huautla_Huehuetla" ~ Id,
    Id == "Tulancingo de Bravo_Xochicoatlan" ~ Id,
    Id == "San Felipe Orizatlán_Manzanas Iii" ~ Id,
    Id == "Nopala de Villagrán_Mizquiapan" ~ Id,
    Id == "Huasca de Ocampo_Tamoyon I" ~ Id,
    Id == "Jacala de Ledezma_Juarez" ~ Id,
    Id == "Mineral del Chico_Plan Grande" ~ Id,
    Id == "Tecozautla_Rancho Pathesito" ~ Id,
    Id == "Omitlán de Juárez_Pisaflores" ~ Id,
    Id == "Tepeji del Río de Ocampo_Localidad El Eden" ~ Id,
    Id == "Tepeji del Río de Ocampo_Localidad El Eden" ~ Id,
    Id == "Xochicoatlán_Chacalapa" ~ Id,
    Id == "Eloxochitlán_La Loma" ~ Id,
    Id == "Pisaflores_Cieneguillas" ~ Id,
    Id == "Xochiatipan_Huitznopala" ~ Id,
    Id == "Metztitlán_Colonia Tenhe" ~ Id,
    Id == "San Agustín Metzquititlán_San Bartolo Tutotepec" ~ Id,
    Id == "Ixmiquilpan_Santa Ana" ~ Id,
    Id == "Huasca de Ocampo_Huazalingo" ~ Id,
    Id == "Chapulhuacán_Xayahualulco" ~ Id,
    Id == "Chilcuautla_Xayahualulco" ~ Id,
    Id == "Nicolás Flores_Omitlan De Juarez" ~ Id,
    Id == "Tezontepec de Aldama_Tlahuelilpan" ~ Id,
    Id == "Lolotla_Metepec" ~ Id,
    Id == "Atlapexco_El Panteon" ~ Id,
    Id == "Calnali_Barrio Nuevo" ~ Id,
    Id == "Tianguistengo_Las Cantinas" ~ Id,
    Id == "Lolotla_Santiago Tezontlale" ~ Id,
    Id == "Juárez Hidalgo_Lolotla" ~ Id,
    Id == "Lolotla_Itztazacuala" ~ Id,
    Id == "Tenango de Doria_Acatlajapa" ~ Id,
    Id == "Huejutla de Reyes_Agua Fria Grande" ~ Id,
    Id == "Huehuetla_Teacala" ~ Id,
    Id == "San Felipe Orizatlán_Pacheco De Allende" ~ Id,
    Id == "Alfajayucan_Almolaya" ~ Id,
    Id == "Alfajayucan_Almolaya" ~ Id,
    Id == "Chapantongo_Neblinas" ~ Id,
    Id == "San Salvador_Ventoquipa" ~ Id,
    Id == "Cuautepec de Hinojosa_San Pedro Gilo (Gilo)" ~ Id,
    Id == "Cuautepec de Hinojosa_San Pedro Gilo (Gilo)" ~ Id,
    Id == "Tenango de Doria_Amola De Ocampo" ~ Id,
    Id == "Xochiatipan_Santa Teresa" ~ Id,
    Id == "Tlahuelilpan_Tlahuiltepa" ~ Id,
    Id == "Agua Blanca de Iturbide_Vicente Guerrero" ~ Id,
    Id == "San Agustín Tlaxiaca_San Agustin Tlaxiaca (Mexiquito)" ~ Id,
    Id == "San Agustín Tlaxiaca_San Agustin Tlaxiaca (Mexiquito)" ~ Id,
    Id == "San Agustín Tlaxiaca_San Agustin Tlaxiaca (Mexiquito)" ~ Id,
    Id == "San Agustín Tlaxiaca_San Agustin Tlaxiaca (Mexiquito)" ~ Id,
    Id == "Almoloya_Apan" ~ Id,
    Id == "Almoloya_Apan" ~ Id,
    Id == "Tenango de Doria_La Cuarta Manzana" ~ Id,
    Id == "Mineral del Chico_Colonia Tenhe" ~ Id,
    Id == "Atlapexco_Santa Maria Amajac" ~ Id,
    Id == "Atlapexco_Calnali" ~ Id,
    Id == "Ixmiquilpan_Barrio Fitzhi" ~ Id,
    Id == "La Misión_Tetlapaya" ~ Id,
    Id == "Jacala de Ledezma_Revolucion Mexicana" ~ Id,
    Id == "Nicolás Flores_Loma Del Progreso" ~ Id,
    Id == "La Misión_Metepec" ~ Id,
    Id == "San Salvador_Yolotepec" ~ Id,
    Id == "Eloxochitlán_San Miguel Regla" ~ Id,
    Id == "Calnali_Santa Maria Amealco" ~ Id,
    Id == "Huazalingo_Huehuetla" ~ Id,
    Id == "El Arenal_Eloxochitlan" ~ Id,
    Id == "Huehuetla_Minas Viejas" ~ Id,
    Id == "Cardonal_Chapantongo" ~ Id,
    Id == "Ajacuba_San Francisco Sacachichilco" ~ Id,
    Id == "Atlapexco_Agua Limpia" ~ Id,
    Id == "Juárez Hidalgo_La Mision" ~ Id,
    Id == "Tepehuacán de Guerrero_Manantiales De Cerro Colorado" ~ Id,
    Id == "Mixquiahuala de Juárez_Mixquiahuala  (Donfhi)" ~ Id,
    Id == "Mixquiahuala de Juárez_Mixquiahuala  (La Pena)" ~ Id,
    Id == "Tecozautla_Barrio Morales" ~ Id,
    Id == "Ajacuba_La Huapilla" ~ Id,
    Id == "Jaltocán_Juarez" ~ Id,
    Id == "Mixquiahuala de Juárez_Mixquiahuala (Col. Taxhuada)" ~ Id,
    Id == "Metztitlán_Mineral Del Chico" ~ Id,
    Id == "Tlahuelilpan_Tlanchinol" ~ Id,
    Id == "Pacula_Metzquititlan" ~ Id,
    Id == "Santiago de Anaya_La Joya (Fraccionamiento)" ~ Id,
    Id == "Progreso de Obregón_Col. Tenhe (Mixquiahuala)" ~ Id,
    Id == "Progreso de Obregón_Col. Tenhe (Mixquiahuala)" ~ Id,
    Id == "Tianguistengo_San Jose Las Cruces" ~ Id,
    Id == "Jaltocán_Puerto La Piedra" ~ Id,
    Id == "San Agustín Tlaxiaca_San Agustin Tlaxiaca (Barrio Casa Grande)" ~ Id,
    Id == "Chilcuautla_San Juan Hueyapan" ~ Id,
    Id == "San Agustín Tlaxiaca_San Agustin Tlaxiaca  (Barrio Casa Grande)" ~ Id,
    Id == "Tepetitlán_Tianguistengo" ~ Id,
    Id == "Tepetitlán_Tianguistengo" ~ Id,
    Id == "Metepec_Carboneras" ~ Id,
    Id == "Tecozautla_Col. 20 De Noviembre" ~ Id,
    Id == "Pacula_Pisaflores" ~ Id,
    Id == "Pisaflores_San Bartolo Tutotepec" ~ Id,
    Id == "Calnali_Barrio San Juan Mezcalpa" ~ Id,
    Id == "La Misión_Barrio Santa Maria" ~ Id,
    Id == "El Arenal_Tepatepec" ~ Id,
    Id == "Apan_Tecolotitla" ~ Id,
    Id == "Metepec_Tecruz Copaza" ~ Id,
    Id == "Tasquillo_Tenango De Doria" ~ Id,
    Id == "Tasquillo_Tenango De Doria" ~ Id,
    Id == "El Arenal_Colonia 20 De Mayo" ~ Id,
    Id == "Apan_Atlapexco" ~ Id,
    Id == "Tecozautla_Ejido Tagui 3 (Localidad)" ~ Id,
    Id == "Yahualica_Xindho San Pedro" ~ Id,
    Id == "Yahualica_Xindho San Pedro" ~ Id,
    Id == "Yahualica_Xindho San Pedro" ~ Id,
    Id == "Yahualica_Xindho San Pedro" ~ Id,
    Id == "Huazalingo_Huejutla De Reyes" ~ Id,
    Id == "Actopan_Agua Blanca De Iturbide" ~ Id,
    Id == "Actopan_Agua Blanca De Iturbide" ~ Id,
    TRUE ~ Id
  ))

checar_repetidos = localidades_2 |> 
  dplyr::group_by(Id) |> 
  dplyr::filter(dplyr::n()>1)

which(checar_repetidos$Id %in% datos$Id)

repetidos = checar_repetidos[which(checar_repetidos$Id %in% datos$Id),]

localidades_2 = localidades_2 |> dplyr::select(Id,CVEGEO,NOMGEO)
c("1303701220012800", "1303701230046800")
quitar = which(localidades_2$CVEGEO %in% c("1303701220012800", "1303701230046800"))
localidades_2 = localidades_2[-quitar,]
names(localidades_2)[c(2,3)] = c("CVEGEO_LOC","NOMGEO_LOC")


datos_merge = merge(x = datos, y = localidades_2, by = "Id", all.x = T)


datos_merge = datos_merge |>  
  dplyr::select(Año, NOM_MUN,CVEGEO_LOC,NOMGEO_LOC,Localidad,Fuente.de.Abastecimiento:Temperatura...C) |> 
  dplyr::arrange(Año,NOM_MUN)

write.csv(datos_merge, "app/assets/Datos/DATOS_2012_2023_Localidades_Rurales_Correctas_Faltantes.csv", fileEncoding = "latin1", row.names = F)



faltantes = read.csv("app/assets/Datos/DATOS_2012_2023_Localidades_Rurales_Correctas_Faltantes.csv", fileEncoding = "latin1")
faltantes = faltantes |> 
  dplyr::filter(is.na(CVEGEO_LOC)) |> 
  dplyr::select(-CVEGEO_LOC, -NOMGEO_LOC)


write.csv(faltantes, "app/assets/Datos/Faltantes.csv", fileEncoding = "latin1", row.names = F)

