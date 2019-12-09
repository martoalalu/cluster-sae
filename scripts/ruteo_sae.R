library(data.table)
library(sf)
library(tidyverse)
library(osrm)
library(dplyr)
library(purrr)
library(ggmap)
library(gmapsdistance)
library(TSP)
library(rlist)


#Levantamos el archivo

sae_ruteo2 <-  read.csv("C:/Users/20332842324/Desktop/sae/sae_ruteo4.csv", sep = ";",encoding = 'UTF-8', header = TRUE)


api_key = Sys.getenv("AIzaSyD22ckgDJ7xCEJh697hps8BYUaRVrmEzzk")
register_google(key = "AIzaSyD22ckgDJ7xCEJh697hps8BYUaRVrmEzzk")

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


named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = "_")))
  grouped %>%
    group_split() %>%
    rlang::set_names(names)
}

#Pasamos a lista cada cluster para hacer el loop por cada ruta
sae_ruteo_lista <- sae_ruteo2 %>%
  named_group_split(muni_cluster)


sae_ruteo_lista_124_125 <- sae_ruteo_lista[124:125]


for(i in 1:length(sae_ruteo_lista_124_125)){
  assign(paste0("sae_distance_", i), gmapsdistance(origin = sae_ruteo_lista_124_125[[i]]$latlon,
                                                   destination = sae_ruteo_lista_124_125[[i]]$latlon,
                                                   combinations = "all",
                                                   key = "AIzaSyD22ckgDJ7xCEJh697hps8BYUaRVrmEzzk",
                                                   mode = "driving")$Distance[, -1])
}

sae_distance_92 <- gmapsdistance(origin = sae_ruteo_lista_124_125[[10]]$latlon,
              destination = sae_ruteo_lista_83_100[[10]]$latlon,
              combinations = "all",
              key = "AIzaSyD22ckgDJ7xCEJh697hps8BYUaRVrmEzzk",
              mode = "driving")$Distance[, -1]


my_list <- list(sae_distance_1,
                sae_distance_2)

for(i in 1:length(my_list)){
  my_list[[i]] <- as.matrix(my_list[[i]]) / 1000
  colnames(my_list[[i]]) <- sae_ruteo_lista_124_125[[i]]$label
  rownames(my_list[[i]]) <- sae_ruteo_lista_124_125[[i]]$label
  my_list[[i]] <- as.dist(my_list[[i]])
  assign(paste0("tsp_", i),TSP(my_list[[i]]))
}


my_list_tsp <- list(tsp_1,
                    tsp_2)

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


my_list_tours <- list(tours_1,
                      tours_2)

for(i in 1:length(my_list_tsp)){
  tour <- solve_TSP(my_list_tsp[[i]])
  assign(paste0("tour_", i),tour)
}

my_list_tour <- list(tour_1,
                     tour_2)

for(i in 1:length(my_list_tour)){
  assign(paste0("tour_order_", i),as.integer(my_list_tour[[i]]))
}

my_list_tour_order <- list(tour_order_1,
                           tour_order_2)

for(i in 1:length(sae_ruteo_lista_124_125)){
  sae_ruteo_lista_124_125[[i]] <- sae_ruteo_lista_124_125[[i]][my_list_tour_order[[i]],] 
}



# BUILD ROUTE ---------------------------------------------------------------------------------------------------------
set.api.key("AIzaSyD22ckgDJ7xCEJh697hps8BYUaRVrmEzzk")




for(i in 1:length(sae_ruteo_lista_124_125)){
  route <- lapply(seq(nrow(sae_ruteo_lista_124_125[[i]]) - 1), function(n) {
    print(n)
    route(sae_ruteo_lista_124_125[[i]]$latlon[n], sae_ruteo_lista_124_125[[i]]$latlon[n+1], structure = "route") %>%
      mutate(section = n)
  })
  route <- route %>% bind_rows()
  route <-  filter(route,!is.na(minutes))
  assign(paste0("route_",i),route)
}

my_list_route <- list(route_1,
                      route_2)

minutos <- list()  
km <- list()
ruta <- list()

for(i in 1:length(my_list_route)){
  minutos <- list.append(minutos,sum(my_list_route[[i]]$minutes))
  km <- list.append(km,sum(my_list_route[[i]]$km))
  ruta <- list.append(ruta,names(sae_ruteo_lista_124_125)[i])
}

df <- cbind(ruta, km, minutos)
df <- as.data.frame(df)


fwrite(df,"C:/Users/20332842324/Desktop/sae/rutas_124_125.csv")
