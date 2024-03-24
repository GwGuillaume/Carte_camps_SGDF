library(shiny)
library(leaflet)
library(readxl)
library(writexl)
library(searcher)
library(osmdata)
library(nominatimlite)
library(stringr)
library(lubridate)
library(dplyr)
library(tm)
library(sf)
library(geosphere)
library(purrr)

in_filename <- "./data/lieux_de_camp_2008_2017.xlsx"
out_filename <- str_replace(string = in_filename, pattern = ".xlsx", replacement = "_avec_adresses.xlsx")

search_adress <- function(researched_address){
  addr_df = list()
  attempts <- 0
  while(length(addr_df) == 0 & nchar(researched_address) > 0){
    attempts <- attempts + 1
    addr_df <- getbb(researched_address, format_out = "data.frame", limit = 1)
    # Remove the first string in adress
    researched_address <- ifelse(test = length(addr_df) == 0,
                                 yes = str_sub(researched_address, str_locate(string = researched_address, pattern = " ")[1]+1, -1),
                                 no = researched_address)
  }
  if(nchar(researched_address) == 0){
    addr_df$lon <- NA
    addr_df$lat <- NA
    addr_df$display_name <- NA
  }
  addr_df <- addr_df %>% 
    mutate("Tentatives" = attempts)
  return(addr_df)
}

if(file.exists(out_filename)){
  rm_prev_file <- askYesNo(msg = paste("Le fichier", out_filename, "existe déjà. Voulez-vous le supprimer ?", sep = " "), default = FALSE, prompts = getOption("askYesNo", gettext(c("Oui", "Non", "Annuler"))))
}
if(isTRUE(rm_prev_file)){
  print("Suppression du fichier existant et traitement du fichier brut")
  file.remove(out_filename)
  data <- read_xlsx(path = in_filename, progress = readxl_progress())
  data$long_camp <- NA
  data$lat_camp <- NA
  data$`Nombre tentatives recherche adresse` <- NA
  data$`Adresse recherchée` <- NA
  data$`Adresse trouvée` <- NA
  data$`Nombre tentatives de recherche d'adresse` <- NA
  data[c("Indice structure organisatrice", "Nom structure organisatrice")] <- str_split_fixed(string = data$`Structures organisatrices`, pattern = " - ", n = 2)
  data$`Nom structure organisatrice` <- str_remove(string = data$`Nom structure organisatrice`, pattern = ",")
  data[c("Indice structure participante", "Nom structure participante")] <- str_split_fixed(string = data$`Structures participantes`, pattern = " - ", n = 2)
  data$`Nom structure participante` <- str_remove(string = data$`Nom structure participante`, pattern = ",")
  data[c("Date début", "Date fin")] <- str_split_fixed(string = gsub("du |au |", "", data$Dates),
                                                       pattern = " ", n = 2)
  data$`Date début` <- strptime(as.character(data$`Date début`), "%d/%m/%Y")
  data$`Date fin` <- strptime(as.character(data$`Date fin`), "%d/%m/%Y")
  data$Année <- year(data$`Date début`)
  data$`Numéro Département` <- as.integer(substr(sgdf_data$`Code postal`, start = 1, stop = 2))

  # Ajout de l'unité à partir du nom de la structure participante
  data <- data %>%
    mutate("Unité" = case_when(
      str_detect(string = `Nom structure participante`, pattern = "FARFADETS") ~ 'FARFADETS',
      str_detect(string = `Nom structure participante`, pattern = "FARFADET") ~ 'FARFADETS',
      str_detect(string = `Nom structure participante`, pattern = "LOUVETEAUX - JEANNETTES") ~ 'LOUVETEAUX - JEANNETTES',
      str_detect(string = `Nom structure participante`, pattern = "LOUVETEAUX") ~ 'LOUVETEAUX',
      str_detect(string = `Nom structure participante`, pattern = "JEANNETTES") ~ 'JEANNETTES',
      str_detect(string = `Nom structure participante`, pattern = "SCOUTS GUIDES")  ~ 'SCOUTS GUIDES',
      str_detect(string = `Nom structure participante`, pattern = "SCOUTS")  ~ 'SCOUTS',
      str_detect(string = `Nom structure participante`, pattern = "GUIDES")  ~ 'GUIDES',
      str_detect(string = `Nom structure participante`, pattern = "PIONNIERS CARAVELLES")  ~ 'PIONNIERS CARAVELLES',
      str_detect(string = `Nom structure participante`, pattern = "PION/PIONNIERE")  ~ 'PIONNIERS CARAVELLES',
      str_detect(string = `Nom structure participante`, pattern = "PIONNIERS") ~ 'PIONNIERS',
      str_detect(string = `Nom structure participante`, pattern = "CARAVELLES")  ~ 'CARAVELLES',
      str_detect(string = `Nom structure participante`, pattern = "COMPAGNONS")  ~ 'COMPAGNONS',
      str_detect(string = `Nom structure participante`, pattern = "GROUPE")  ~ 'GROUPE',
      TRUE ~ 'AUTRES'))

  # Ajout de l'unité lorsqu'elle est à 'AUTRES' à partir du nom de la structure participante
  data[data$Unité == 'AUTRES', ] <- data %>%
    filter(Unité == 'AUTRES') %>%
    mutate("Unité" = case_when(
      str_detect(string = `Nom structure organisatrice`, pattern = "FARFADETS") ~ 'FARFADETS',
      str_detect(string = `Nom structure organisatrice`, pattern = "FARFADET") ~ 'FARFADETS',
      str_detect(string = `Nom structure organisatrice`, pattern = "LOUVETEAUX - JEANNETTES") ~ 'LOUVETEAUX - JEANNETTES',
      str_detect(string = `Nom structure organisatrice`, pattern = "LOUVETEAUX") ~ 'LOUVETEAUX',
      str_detect(string = `Nom structure organisatrice`, pattern = "JEANNETTES") ~ 'JEANNETTES',
      str_detect(string = `Nom structure organisatrice`, pattern = "SCOUTS GUIDES")  ~ 'SCOUTS GUIDES',
      str_detect(string = `Nom structure organisatrice`, pattern = "SCOUTS")  ~ 'SCOUTS',
      str_detect(string = `Nom structure organisatrice`, pattern = "GUIDES")  ~ 'GUIDES',
      str_detect(string = `Nom structure organisatrice`, pattern = "PIONNIERS CARAVELLES")  ~ 'PIONNIERS CARAVELLES',
      str_detect(string = `Nom structure organisatrice`, pattern = "PIONNIERS - CARAVELLES")  ~ 'PIONNIERS CARAVELLES',
      str_detect(string = `Nom structure organisatrice`, pattern = "PION/PIONNIERE")  ~ 'PIONNIERS CARAVELLES',
      str_detect(string = `Nom structure organisatrice`, pattern = "PIONNIERS") ~ 'PIONNIERS',
      str_detect(string = `Nom structure organisatrice`, pattern = "CARAVELLES")  ~ 'CARAVELLES',
      str_detect(string = `Nom structure organisatrice`, pattern = "COMPAGNONS")  ~ 'COMPAGNONS',
      str_detect(string = `Nom structure organisatrice`, pattern = "GROUPE")  ~ 'GROUPE',
      TRUE ~ 'AUTRE'))
  # Ajout de la couleur à partir de l'unité
  data <- data %>%
    mutate("Couleur" = case_when(
      Unité == "FARFADETS" | Unité == "FARFADET" ~ "rgb(103, 172, 54)",
      Unité == "LOUVETEAUX - JEANNETTES" | Unité == "LOUVETEAUX" | Unité == "JEANNETTES" ~ "rgb(239, 114, 19)",
      Unité == "SCOUTS GUIDES" | Unité == "SCOUTS" | Unité == "GUIDES" ~ "rgb(0, 119, 183)",
      Unité == "PIONNIERS CARAVELLES" | Unité == "PION/PIONNIERE" | Unité == "PIONNIERS" | Unité == "CARAVELLES" ~ "rgb(223, 41, 17)",
      Unité == "COMPAGNONS" ~ "rgb(34, 107, 34)",
      Unité == "GROUPE" ~ "rgb(102, 26, 126)",
      Unité == "AUTRE" ~ "rgb(0, 0, 0)",
      TRUE ~ 'AUTRE'))
  # Nom du groupe
  remove_strings <- unlist(strsplit(paste(paste0(c("1ERE", "1ER"), collapse = ","), 
                                          paste0(seq(2, 40), "EME", collapse = ","), 
                                          paste0(c("FARFADETS", "FARFADET", "LOUVETEAUX - JEANNETTES", "LOUVETEAUX", "JEANNETTES", 
                                                   "SCOUTS GUIDES", "SCOUTS", "GUIDES", "PIONNIERS CARAVELLES", "PIONNIERS - CARAVELLES", 
                                                   "PION/PIONNIERE", "PIONNIERS", "CARAVELLES", "COMPAGNONS", "GROUPE"), collapse = ","), 
                                          paste0(c("-", ",", " "), collapse = ","), 
                                          sep = ", "), 
                                    split = ","))
  data$`Adresse groupe` <- removeNumbers(str_remove_all(data$`Nom structure participante`, paste(remove_strings, collapse = "|")))
  nb_camps <- nrow(data)
  adress_cols <- c("Adresse du lieu de camp (à préciser si besoin)", "Code postal", "Ville")
  print("Recherche des adresses des camps")
  for(id_camp in seq(nb_camps)){
    searched_address_camp <- paste(cbind(data[id_camp, adress_cols], "FRANCE"), collapse = " ")
    print(paste0("...", searched_address_camp, " (", round(x=100*id_camp/nb_camps, digits = 1), "%)"))
    data$`Adresse recherchée`[id_camp] <- searched_address_camp
    adress_camp_df <- search_adress(searched_address_camp)
    data$`Nombre tentatives de recherche d'adresse`[id_camp] <- adress_camp_df$Tentatives
    data$`Adresse trouvée`[id_camp] <- adress_camp_df$display_name
    data$long_camp[id_camp] <- as.numeric(adress_camp_df$lon)
    data$lat_camp[id_camp] <- as.numeric(adress_camp_df$lat)
    # Recherche de l'adreese de départ
    searched_address_depart <- paste(cbind(data$`Adresse groupe`[id_camp], "FRANCE"), collapse = " ")
    adress_depart_df <- search_adress(searched_address_depart)
    data$long_depart[id_camp] <- as.numeric(adress_depart_df$lon)
    data$lat_depart[id_camp] <- as.numeric(adress_depart_df$lat)
    # Save data to new file
    write_xlsx(x = data, path = out_filename)
  }
} else {  # Charger le fichier déjà existant
  print("Chargement du fichier déjà existant")
  data <- read_xlsx(path = out_filename, progress = readxl_progress())
}

sgdf_data <- sgdf_data %>%
  mutate(`Distance lieu de camp` = distm(x = cbind(sgdf_data$long_camp, sgdf_data$lat_camp),
                                         y = cbind(sgdf_data$long_depart, sgdf_data$lat_depart),
                                         fun = distHaversine)[, 1])

getColor <- function(df) {
  sapply(df$Unité, function(Unité) {
    if(Unité == "FARFADETS" | Unité == "FARFADET") {
      "rgb(103, 172, 54)"
    } else if(Unité == "LOUVETEAUX - JEANNETTES" | Unité == "LOUVETEAUX" | Unité == "JEANNETTES") {
      "rgb(239, 114, 19)"
    } else if(Unité == "SCOUTS GUIDES" | Unité == "SCOUTS" | Unité == "GUIDES") {
      "rgb(0, 119, 183)"
    } else if(Unité == "PIONNIERS CARAVELLES" | Unité == "PION/PIONNIERE" | Unité == "PIONNIERS" | Unité == "CARAVELLES") {
      "rgb(223, 41, 17)"
    } else if(Unité == "COMPAGNONS") {
      "rgb(34, 107, 34)"
    } else if(Unité == "GROUPE") {
      "rgb(102, 26, 126)"
    } else if(Unité == "AUTRE") {
      "rgb(0, 0, 0)"
    }
  })
}

# icons <- awesomeIcons(
#   icon = 'ios-close',
#   iconColor = 'black',
#   library = 'ion',
#   markerColor = getColor(data)
# )

map <- leaflet(data) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~long_camp, lat = ~lat_camp, radius = 5, group = ~Unité, color = ~Couleur, opacity = 0.6, fill = TRUE, fillColor = ~Couleur, fillOpacity = 0.4, 
                   popup = paste0(
                     "<b>Ville : </b>"
                     , data$Ville
                     , "<br>"
                     , "<b>Unité : </b>"
                     , data$Unité
                     , "<br>"
                     , "<b>Date début : </b>"
                     , data$`Date début`
                     , "<br>"
                     , "<b>Date fin : </b>"
                     , data$`Date fin`
                     , "<br>"
                     , "<b>Lieu : </b>"
                     , data$Ville, " (", data$Département, " - ", data$Région, ")"
                     , "<br>"
                     , "<b>Nom structure participante : </b>"
                     , data$`Nom structure participante`
                     , "<br>"
                   )) %>% 
  addCircleMarkers(lng = ~long_depart, lat = ~lat_depart, radius = '3', group = ~Unité, color = "rgb(0, 0, 0)", opacity = 0.6, fill = TRUE, fillColor = "rgb(0, 0, 0)", fillOpacity = 0.4)

mydf2 <- data.frame(lat = c(data$lat_depart, data$lat_camp),
                    long = c(data$long_depart, data$long_camp))
map <- map %>% 
  addPolylines(data = mydf2, lng = ~long, lat = ~lat)

map