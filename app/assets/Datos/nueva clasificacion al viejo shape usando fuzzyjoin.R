nueva_clasificacion=read_excel("Listado de Problemas públicos_lz.xlsx")
shape_oportunidades=read_sf("Problemas_a_nivel_municipal1.shp")|>st_transform(st_crs("EPSG:4326"))
shape_oportunidades$C5[shape_oportunidades$CVE_MUN=='031']='Infraestructura carretera'
shape_oportunidades$P5[shape_oportunidades$CVE_MUN=='031']='Mal estado del transporte público intermunicipal'
shape_oportunidades=shape_oportunidades |> dplyr::arrange(CVEGEO)


#[Intento de Fuzzy match]
library(fuzz)

a <- data.frame(name = c('Ace Co', 'Bayes', 'asd', 'Bcy', 'Baes', 'Bays'),
                price = c(10, 13, 2, 1, 15, 1))
b <- data.frame(name = c('Ace Co.', 'Bayes Inc.', 'asdf'),
                qty = c(9, 99, 10))
#install.packages("fuzzyjoin")
library(fuzzyjoin);
library(dplyr);

guardar = stringdist_join(a, b, 
                by = "name",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist")  |> 
  group_by(name.x)  |> 
  slice_min(order_by = dist, n = 1)

parte1 = stringdist_join(a, b, 
                         by = "name",
                         mode = "left",
                         ignore_case = FALSE, 
                         method = "jw", 
                         max_dist = 99, 
                         distance_col = "dist")



###Nuestro ejemplo. 
shape_oportunidades_pivotes=shape_oportunidades |> st_drop_geometry()
shape_oportunidades_pivotes$orden_original=1:nrow(shape_oportunidades_pivotes)
shape_oportunidades_pivotes=shape_oportunidades_pivotes |> dplyr::mutate(
  CP1=paste(C1,P1),
  CP2=paste(C2,P2),
  CP3=paste(C3,P3),
  CP4=paste(C4,P4),
  CP5=paste(C5,P5)
)
shape_oportunidades_pivotes=shape_oportunidades_pivotes |> pivot_longer(cols =c(CP1:CP5) ,names_to = "CS",values_to = "CSValues")
#shape_oportunidades_pivotes=shape_oportunidades_pivotes |> pivot_longer(cols =c(P1,P2,P3,P4,P5) ,names_to = "PS",values_to = "PSValues")
shape_oportunidades_pivotes=shape_oportunidades_pivotes |> 
  dplyr::select(CVEGEO:Municipio,CSValues)


colnames(nueva_clasificacion)[1]="CSValues"
nueva_clasificacion$CSValues=gsub(pattern = "Problemas sociales","Problemas Sociales",nueva_clasificacion$CSValues)

zzz=stringdist_join(shape_oportunidades_pivotes |> dplyr::select(CVEGEO,CSValues), nueva_clasificacion, 
                    by = "CSValues",
                    mode = "left",
                    ignore_case = T, 
                    method = "jw", 
                    max_dist = 99, 
                    distance_col = "dist") %>%
  group_by(CSValues.x) %>%
  slice_min(order_by = dist, n = 1,with_ties = F)
###Corregimos a mano el fuzzyjoin
zzz |> write.csv("fuzzyjoin.csv",fileEncoding = "latin1",row.names = F)
zzz =read.csv("fuzzyjoin.csv",fileEncoding = "latin1")


shape_oportunidades_pivotes=merge(shape_oportunidades_pivotes,zzz,by.x='CSValues',by.y='CSValues.x',)
shape_oportunidades_pivotes$CS=stringi::stri_extract(str = shape_oportunidades_pivotes$CSValues.y,regex  = "Medio Ambiente|Infraestructura y Transporte|Seguridad y corrupcion|Economia y Empleo|Agua y Crisis Hidrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Riesgos|Salud") 
shape_oportunidades_pivotes$PS=stringi::stri_split_regex(str = shape_oportunidades_pivotes$CSValues.y,pattern = "Medio Ambiente|Infraestructura y Transporte|Seguridad y corrupcion|Economia y Empleo|Agua y Crisis Hidrica|Educacion|Gestion territorial y desarrollo urbano|Problemas Sociales|Riesgos|Salud",n = 2) |> lapply(\(x) x[2]) |> unlist() |> stringr::str_squish()

shape_oportunidades_pivotes=shape_oportunidades_pivotes |> 
  dplyr::arrange(orden_original)
shape_oportunidades_pivotes$orden_original=rep(1:5,84)
shape_oportunidades_pivoted_wider=shape_oportunidades_pivotes |> dplyr::group_by(CVEGEO.x) |> 
  tidyr::pivot_wider(values_from = CS,names_from = orden_original)

shape_oportunidades_pivoted_wider2=shape_oportunidades_pivotes |> dplyr::group_by(CVEGEO.x) |> 
  tidyr::pivot_wider(values_from = PS,names_from = orden_original)




shape_oportunidades$C1=
shape_oportunidades_pivoted_wider$`1`[1:84*5-4]
shape_oportunidades$C2=
shape_oportunidades_pivoted_wider$`2`[1:84*5-3]
shape_oportunidades$C3=
shape_oportunidades_pivoted_wider$`3`[1:84*5-2]
shape_oportunidades$C4=
shape_oportunidades_pivoted_wider$`4`[1:84*5-1]
shape_oportunidades$C5=
shape_oportunidades_pivoted_wider$`5`[1:84*5]

shape_oportunidades$P1=
shape_oportunidades_pivoted_wider2$`1`[1:84*5-4]
shape_oportunidades$P2=
shape_oportunidades_pivoted_wider2$`2`[1:84*5-3]
shape_oportunidades$P3=
shape_oportunidades_pivoted_wider2$`3`[1:84*5-2]
shape_oportunidades$P4=
shape_oportunidades_pivoted_wider2$`4`[1:84*5-1]
shape_oportunidades$P5=
shape_oportunidades_pivoted_wider2$`5`[1:84*5]

