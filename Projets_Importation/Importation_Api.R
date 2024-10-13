library(ggplot2)
library(leaflet)
library(DT)
library(httr)
library(jsonlite)
library(shinyjs)
library(stringr)
library(dplyr)
library(readxl)
library(curl)
library(readr)
library(rsconnect) #ce sont tout les packages du code rshiny mais cela fera tres bien marcher le code

#tout les fichier adresse on ete recuperer sur le site du gouvernement et on ete ouvert en local pour creer mon adresse final
csv = read.csv("adresses-69.csv",sep = ";") 
all_data = rbind(all_data, csv) 
csv = read.csv("adresses-25.csv",sep = ";")
all_data = rbind(all_data, csv) 
csv = read.csv("adresses-2A.csv",sep = ";")
all_data = rbind(all_data, csv) 
csv = read.csv("adresses-2B.csv",sep = ";")
all_data = rbind(all_data, csv) 
csv = read.csv("adresses-75.csv",sep = ";")
all_data = rbind(all_data, csv)
csv = read.csv("adresses-92.csv",sep = ";")
all_data = rbind(all_data, csv)
nouveau_df <- data.frame(id = all_data$id,code_postal = all_data$code_postal,lon = all_data$lon, lat = all_data$lat)
write.csv(nouveau_df, "adresse_final2", row.names = FALSE)

#creation de mon fichier adresse qui sera mis sur dropbox pour etres recuperable via un lien 


url <- "https://www.dropbox.com/scl/fi/ynrx2vhsuz7qhwridk80g/adresse_final2.csv?rlkey=b84yh3410a976h2j3u7aoslq3&st=lrq44zv7&dl=1"
adresse <- read.csv(url,   sep = ",", 
                    header = TRUE, 
                    check.names = FALSE)



base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"


cp <- unique(coordoné[["Code_postal_(BAN)"]])


all_data <- data.frame()
date_debut <- "1900-01-01"
date_fin <- data.frame(Date = as.Date(paste0(2000:2024, "-01-01")))

#importation des donnée via l'api
for (code_postal in cp) {
  params <- list(
    page = 1,
    select = "N°DPE,Etiquette_DPE,Date_réception_DPE,Code_postal_(BAN),Identifiant__BAN",
    size = 10000,
    q = as.character(code_postal),
    q_fields = "Code_postal_(BAN)",
    qs = paste0("Date_réception_DPE:[", date_debut, " TO ", date_fin, "]")
  )
  
  response <- GET(base_url, query = params)
  
  if (response$status_code == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))$results
    
    if (!is.null(data)) {
      df <- as.data.frame(data)
      all_data <- rbind(all_data, df)
    }
  }
  date_debut <- date_fin()
}

#rajout des colonnes de adresse dans mon all_data pour pouvoir avoir les coordonnées geographique relier a des habtitions 
merged_data <- merge(all_data, adresse, by.x = "Identifiant__BAN", by.y = "id", all.x = TRUE)

# Suppression des lignes où la colonne 'lat' est NA
merged_data <- merged_data[!is.na(merged_data$lat), ]  
#creation du fichier qui sera de meme deposer sur dropbox 
write.csv(merged_data, "coordonée2", row.names = FALSE)
