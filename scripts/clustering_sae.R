library(data.table)
library(sf)
library(tidyverse)
library(viridis)
library(osmdata)
library(ggmap)
library(geosphere)
library(openxlsx)
library(janitor)
library(leaflet)
library(osrm)


#Levantamos el archivo
sae <-  fread("C:/Users/Marto/Desktop/sae/sae.csv", sep = ";",encoding = 'UTF-8', header = TRUE)


#Levantamos el xls que tiene la cantidad de clusters por municipio
municipios <- read.xlsx("C:/Users/Marto/Desktop/sae/recorridosxmunicipio.xlsx", sheet = 1)

#Ordeno por municipio
municipios <- municipios[order(municipios$municipio),]

#Redondeo valores
municipios <- municipios %>% 
  mutate(clusters_2=round(clusters)) %>% 
  select(-escuelas)

sae <- left_join(sae,municipios)


#Creamos funcion que splitea el df por 2 variables y asigna nombre a cada lista
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = "_")))
  grouped %>%
    group_split() %>%
    rlang::set_names(names)
}

#Separo los municipios de Guido, Lavalle, Las Heras, Lezama, Pellegrini, Pila y Tordillo para analizar despues
sae_7_municipios <- sae %>% 
  filter(municipio=="GENERAL GUIDO" | 
         municipio=="GENERAL LAVALLE" | 
         municipio=="TORDILLO" |
         municipio=="PELLEGRINI" | 
         municipio=="GENERAL LAS HERAS" | 
         municipio=="PILA"| 
         municipio=="LEZAMA")

sae <- sae %>% 
  filter(municipio!="GENERAL GUIDO" & 
           municipio!="GENERAL LAVALLE" & 
           municipio!="TORDILLO" & 
           municipio!="PELLEGRINI" & 
           municipio!="GENERAL LAS HERAS" & 
           municipio!="PILA"& 
           municipio!="LEZAMA")


#Divido el dataset en escuelas rurales y urbanas
sae_urbano <- filter(sae,zona=="Urbano")
sae_rural<- filter(sae,zona=="Rural")


#Excluimos los 7 municipios del listado
municipios_urbanos <- municipios %>% 
  filter(municipio!="GENERAL GUIDO" & 
           municipio!="GENERAL LAVALLE" & 
           municipio!="TORDILLO" & 
           municipio!="PELLEGRINI" & 
           municipio!="GENERAL LAS HERAS" & 
           municipio!="PILA"& 
           municipio!="LEZAMA")

#A los rurales los vamos asignar forzadamente luego al cluster más cercano


#Divido df por municipio
sae_urbano_lista <- sae_urbano %>%
  named_group_split(municipio)



#Loopeamos por cada df y asignamos cluster a cada escuela en base al valor de clusters en municipios
for(i in 1:length(sae_urbano_lista)){
  a <- kmeans(cbind(sae_urbano_lista[[i]]$lat, sae_urbano_lista[[i]]$long), centers = municipios_urbanos[i,4])
  sae_urbano_lista[[i]]$cluster_asignado_2 <- a$cluster
}



#Vuelvo a agrupar todo en un solo df
sae_urbano<-sae[-c(1:nrow(sae)),]

for(i in 1:length(sae_urbano_lista)){
  sae_urbano <- rbind(sae_urbano,sae_urbano_lista[[i]])
}

#Generamos una columna que concatena municipio y cluster como id unico
sae_urbano <- sae_urbano %>% 
  mutate(muni_cluster=paste0(municipio_code,"-",cluster_asignado_2))


View(filter(filter(sae_urbano,sae_urbano$municipio=="SAN PEDRO")))

#Contamos escuelas por cluster
#Identificamos los clusters que tienen menos de 6 escuelas
cluster_count<- sae_urbano %>%
  count(muni_cluster) %>%
  arrange(desc(n))

#Joineamos la cantidad de escuelas por cluster
sae_urbano <- left_join(sae_urbano,cluster_count)


#Separamos las escuelas que pertenecian a un cluster que tenia menos de 5 escuelas y lo unimos al sae_rural
sae_sin_cluster <- sae_urbano %>% 
  filter(n<5) %>% 
  select(-cluster_asignado_2,-muni_cluster,-n) %>% 
  rbind(sae_rural)

#Nos quedamos solo con las escuelas con cluster OK para armar centroides
sae_urbano <- filter(sae_urbano,n>=5)

#Chequeamos que todo este OK
nrow(sae_sin_cluster)+nrow(sae_urbano)+nrow(sae_7_municipios)


#Volvemos a dividir el df de escuelas urbanas que filtramos
sae_urbano_lista <- sae_urbano %>%
  named_group_split(municipio)


#Loopeamos y agregamos filas con los centroides de cada cluster en cada df de municipio
for(i in 1:length(sae_urbano_lista)){
  lat_c <- sae_urbano_lista[[i]] %>% 
    group_by(cluster_asignado_2) %>% 
    summarise(lat = mean(lat))
  long_c <- sae_urbano_lista[[i]] %>% 
    group_by(cluster_asignado_2) %>% 
    summarise(long = mean(long))
  centroide <- cbind(lat_c,select(long_c,long))
  centroide <- centroide %>%
    mutate(municipio_sae=as.character(sae_urbano_lista[[i]][1,1]),
           clave_provincial="cluster",
           municipio_code=as.character(sae_urbano_lista[[i]][1,3]),
           number_mun_code="cluster",
           municipio=as.character(sae_urbano_lista[[i]][1,5]),
           tipo="cluster",
           rama="cluster",
           escuela="cluster",
           number_escuela="cluster",
           cue="cluster",
           cs="cluster",
           dmclc="cluster",
           dmc="cluster",
           cupo_total="cluster",
           cupo="cluster",
           clusters="cluster",
           clusters_2="cluster",
           ubicacion_cue="cluster",
           zona="cluster",
           muni_cluster="cluster",
           n="cluster"
    )
  centroide<-centroide[c("municipio_sae","clave_provincial", "municipio_code", 
                         "number_mun_code","municipio","tipo","rama","escuela",
                         "number_escuela","cue","cs","dmclc","dmc","cupo_total",
                         "long","lat","ubicacion_cue","zona","cupo","clusters","clusters_2",
                         "cluster_asignado_2","muni_cluster","n")]
  sae_urbano_lista[[i]] <- rbind(sae_urbano_lista[[i]],centroide)
}



#Chequeamos
View(sae_urbano_lista[[113]])

#Volvemos a agrupar todas las listas en un solo df
sae_urbano<-sae_urbano[-c(1:nrow(sae)),]

for(i in 1:length(sae_urbano_lista)){
  sae_urbano <- rbind(sae_urbano,sae_urbano_lista[[i]])
}

sae_urbano <- sae_urbano %>% 
  mutate(muni_cluster = paste0(municipio_code,"-",cluster_asignado_2))

#Chequeamos que todo este OK
nrow(sae_sin_cluster)+nrow(filter(sae_urbano,sae_urbano$clave_provincial!="cluster"))+nrow(sae_7_municipios)

View(filter(sae_urbano,sae_urbano$municipio_sae=="SAN PEDRO"))

#Tenemos 3 df importantes
#Sae_urbano tiene todos los datos de escuelas urbanas clusterizadas con N>=6 y los centroides
#sae_sin_cluster tiene las escuelas a las que hay que imputarle el cluster mas cercano
#sae_7_municipios tiene los 7 municipios que trataremos luego especialmente

#A los que nos quedaron sin clusterizar le asignamos el cluster de la escuela mas cercana clusterizada


#Tenemos que quedarnos con 2 listas de df que tengan la misma extension, es decir que coincidan los municipios que queremos hacer coincidir
muni_sin_cluster <- unique(sae_sin_cluster$municipio_sae)

#Filtramos las escuelas urbanas con los municipios a clusterizar de sae_sin_cluster
sae_urbano_join<- sae_urbano %>%
  filter(municipio_sae %in% muni_sin_cluster & clave_provincial!="cluster")


#Pasamos a objeto geo los 2 df

sae_urbano_join_geo <- sae_urbano_join %>% 
  st_as_sf(coords=c("long","lat"),crs=4326) %>% 
  arrange(municipio_sae)

sae_sin_cluster_geo <- sae_sin_cluster %>% 
  st_as_sf(coords=c("long","lat"),crs=4326) %>% 
  arrange(municipio_sae)
  
#Chequeamos
View(unique(sae_urbano_join_geo$municipio_sae))
View(unique(sae_sin_cluster_geo$municipio_sae))

#Pasamos los 2 df a lista agrupada
sae_urbano_join_geo_lista <- sae_urbano_join_geo %>%
  named_group_split(municipio_sae)

sae_sin_cluster_geo_lista <- sae_sin_cluster_geo %>%
  named_group_split(municipio_sae)

#Hacemos join espacial

for(i in 1:length(sae_sin_cluster_geo_lista)){
  sae_sin_cluster_geo_lista[[i]]<- st_join(sae_sin_cluster_geo_lista[[i]],sae_urbano_join_geo_lista[[i]],st_nearest_feature)
}

View(sae_sin_cluster_geo_lista[[1]])


#Sacar de listas
#Renombrar y seleccionar columnas de las nuevas clusterizadas
#Unir 2 df (sae_urbano y sae_sin_cluster)

#Volvemos a agrupar todas las listas en un solo df
sae_imputados<-sae_sin_cluster_geo_lista[[1]]
sae_imputados <- sae_imputados[-c(1:nrow(sae_imputados)),]

for(i in 1:length(sae_sin_cluster_geo_lista)){
  sae_imputados <- rbind(sae_imputados,sae_sin_cluster_geo_lista[[i]])
}

#Volvemos a tener columnas lat long
sae_imputados <- sae_imputados %>% extract(geometry, c('long', 'lat'), '\\((.*), (.*)\\)', convert = TRUE) 
sae_imputados <- as.data.frame(sae_imputados)

View(sae_imputados)

colnames(sae_imputados)
colnames(sae_urbano)

sae_imputados <- sae_imputados %>% 
  select(municipio_sae.x, clave_provincial.x, municipio_code.x, number_mun_code.x,
         municipio.x, tipo.x, rama.x, escuela.x, number_escuela.x, cue.x, cs.x,
         dmclc.x, dmc.x, cupo_total.x, long, lat, ubicacion_cue.x, zona.x, 
         cupo.x, clusters.x, clusters_2.x, cluster_asignado_2, muni_cluster, n) %>% 
  rename(municipio_sae=municipio_sae.x,
         clave_provincial=clave_provincial.x, 
         municipio_code=municipio_code.x, 
         number_mun_code=number_mun_code.x,
         municipio=municipio.x, 
         tipo=tipo.x, 
         rama=rama.x, 
         escuela=escuela.x, 
         number_escuela=number_escuela.x, 
         cue=cue.x, 
         cs=cs.x,
         dmclc=dmclc.x, 
         dmc=dmc.x, 
         cupo_total=cupo_total.x, 
         ubicacion_cue=ubicacion_cue.x, 
         zona=zona.x, 
         cupo=cupo.x, 
         clusters=clusters.x, 
         clusters_2=clusters_2.x)


#Unimos 2 df (sae_urbano y los imputados)
sae_ruteo <- rbind(sae_urbano,sae_imputados)

esc_count <- sae_ruteo %>% 
  count(municipio)

prueba <- sae_ruteo %>% 
  count(muni_cluster,municipio, clusters_2)

View(filter(sae_ruteo,sae_ruteo$muni_cluster=="98-1"))
View(filter(sae_urbano,sae_urbano$municipio_sae=="SAN PEDRO"))

write.csv(sae_ruteo,"C:/Users/Marto/Desktop/sae/sae_ruteo.csv")
 
#RUTEO

library(dplyr)
library(purrr)
library(ggmap)
library(gmapsdistance)
library(TSP)
library(rlist)

sae_ruteo2 <- sae_ruteo

api_key = Sys.getenv("INSERTAR-API-KEY")
register_google(key = "INSERTAR-API-KEY")

#Reemplazo punto por coma para que tome bien las lat long
sae_ruteo2[] <- lapply(sae_ruteo2, gsub, pattern=",", replacement=".")

#Paso a numericos las lat y long
sae_ruteo2 <- sae_ruteo2 %>%
  mutate(lat=as.numeric(lat)) %>% 
  mutate(long=as.numeric(long))%>%
  rename(label=escuela)

#Creo variable nueva que concatena lat long
sae_ruteo2 <- sae_ruteo2 %>% 
  mutate(latlon = sprintf("%f+%f", lat, long))


#Creo matriz de distancia

#Pasamos a lista cada cluster para hacer el loop por cada ruta
sae_ruteo_lista <- sae_ruteo2 %>%
  named_group_split(muni_cluster)

sae_ruteo_lista_1_2 <- sae_ruteo_lista[3:4]


for(i in 1:length(sae_ruteo_lista_1_2)){
  assign(paste0("sae_distance_", i), gmapsdistance(origin = sae_ruteo_lista_1_2[[i]]$latlon,
                                     destination = sae_ruteo_lista_1_2[[i]]$latlon,
                                     combinations = "all",
                                     key = "INSERTAR-API-KEY",
                                     mode = "driving")$Distance[, -1])
}

View(sae_ruteo_lista_1_2[[2]])  
my_list <- list(sae_distance_1,sae_distance_2)

for(i in 1:length(my_list)){
  my_list[[i]] <- as.matrix(my_list[[i]]) / 1000
  colnames(my_list[[i]]) <- sae_ruteo_lista_1_2[[i]]$label
  rownames(my_list[[i]]) <- sae_ruteo_lista_1_2[[i]]$label
  my_list[[i]] <- as.dist(my_list[[i]])
  assign(paste0("tsp_", i),TSP(my_list[[i]]))
}
    

my_list_tsp <- list(tsp_1,tsp_2)

# my_list <- list.append(my_list, sae)


methods <- c(
  "nearest_insertion",
  "farthest_insertion",
  "cheapest_insertion",
  "arbitrary_insertion",
  "nn",
  "repetitive_nn",
  "two_opt"
)

for(i in 1:length(my_list_tsp)){
 tours <- methods %>% map(function(method) {
    solve_TSP(my_list_tsp[[i]], method)})
 assign(paste0("tours_", i),tours)
  }         

my_list_tours <- list(tours_1,tours_2)

for(i in 1:length(my_list_tsp)){
  tour <- solve_TSP(my_list_tsp[[i]])
  assign(paste0("tour_", i),tour)
}

my_list_tour <- list(tour_1,tour_2)

for(i in 1:length(my_list_tour)){
  assign(paste0("tour_order_", i),as.integer(my_list_tour[[i]]))
}

my_list_tour_order <- list(tour_order_1,tour_order_2)

for(i in 1:length(sae_ruteo_lista_1_2)){
  sae_ruteo_lista_1_2[[i]] <- sae_ruteo_lista_1_2[[i]][my_list_tour_order[[i]],] 
}


# BUILD ROUTE ---------------------------------------------------------------------------------------------------------

for(i in 1:length(sae_ruteo_lista_1_2)){
  route <- lapply(seq(nrow(sae_ruteo_lista_1_2[[1]]) - 1), function(n) {
    print(n)
    route(sae_ruteo_lista_1_2[[1]]$latlon[n], sae_ruteo_lista_1_2[[1]]$latlon[n+1], structure = "route") %>%
      mutate(section = n)
  })
  assign(paste0("route_",i),route)
  
}

my_list_route <- list(route_1,route_2)

for(i in 1:length(my_list_route)){
  route <- lapply(seq(nrow(sae_ruteo_lista_1_2[[i]]) - 1), function(n) {
    print(n)
    route(sae_ruteo_lista_1_2[[i]]$latlon[n], sae_ruteo_lista_1_2[[i]]$latlon[n+1], structure = "route") %>%
      mutate(section = n)
  })
  route <- route %>% bind_rows()
  route <-  filter(route,!is.na(minutes))
  assign(paste0("route_",i),route)
}

my_list_route_2 <- list(route_1,route_2)

minutos <- list()  
km <- list()
ruta <- list()

for(i in 1:length(my_list_route_2)){
  minutos <- list.append(minutos,sum(my_list_route_2[[i]]$minutes))
  km <- list.append(km,sum(my_list_route_2[[i]]$km))
  ruta <- list.append(ruta,names(sae_ruteo_lista_1_2)[i])
}

df <- cbind(minutos,km, ruta)
df <- as.tibble(df)

View(df)


#Mapeamos para ver cOmo quedan las escuelas y el centroide de un cluster
#La Matanza
bbox <- c(min(sae_urbano_lista[[120]]$long, na.rm = TRUE)-0.1,
          min(sae_urbano_lista[[120]]$lat, na.rm = TRUE)-0.1,
          max(sae_urbano_lista[[120]]$long, na.rm = TRUE)+0.1,
          max(sae_urbano_lista[[120]]$lat, na.rm = TRUE)+0.1)

pba <- get_stamenmap(bbox = bbox,
                     maptype = "toner-background",
                     zoom = 11)

#Mapeamos La Matanza
ggmap(pba)+
  geom_point(data=filter(sae_urbano_lista[[120]],cue!="cluster"), aes(x = long, y = lat, color=as.factor(cluster_asignado_2)),size=1)+
  labs(title="La Matanza: Clusterizacion de escuelas")+
  theme_void()+
  theme(legend.position="none")










