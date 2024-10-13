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
all_data <- data.frame()
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

#creation de mon fichier adresse qui sera mis sur dropbox pour etres recuperable via un lien Partie un 

#Importation avec le fichier adresse parti 2 
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
library(rsconnect)

url <- "https://www.dropbox.com/scl/fi/ynrx2vhsuz7qhwridk80g/adresse_final2.csv?rlkey=b84yh3410a976h2j3u7aoslq3&st=lrq44zv7&dl=1"
adresse <- read.csv(url, sep = ",", header = TRUE, check.names = FALSE)

base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"

cp <- unique(adresse[["code_postal"]])

all_data <- data.frame()
date_debut <- "1900-01-01"
dates <- c("2000-01-01", "2002-01-01", "2004-01-01", "2006-01-01", "2008-01-01", 
               "2010-01-01", "2011-01-01", "2012-01-01", "2013-01-01", "2014-01-01",
               "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01", 
               "2020-01-01", "2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01", "2025-01-01")

# Importation des données via l'API
for (code_postal in cp) {
  
  # Réinitialiser la date de début pour chaque code postal
  date_debut <- "1900-01-01"
  
  # Boucle sur chaque période (chaque élément dans `dates`)
  for (i in seq_along(dates)) {
    

    date_fin <- dates[i]
    print(date_debut)
    print(date_fin)
    # Paramètres pour la requête avec la plage de dates mise à jour
    params <- list(
      page = 1,
      select = "N°DPE,Etiquette_DPE,Date_réception_DPE,Code_postal_(BAN),Identifiant__BAN",
      size = 10000,
      q = as.character(code_postal),
      q_fields = "Code_postal_(BAN)",
      qs = paste0("Date_réception_DPE:[", date_debut, " TO ", date_fin, "]")  # Plage de dates
    )
    
    # Envoi de la requête GET à l'API
    response <- GET(base_url, query = params)
    
    # Parsing du contenu de la réponse et conversion en data frame
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))$results
    df <- as.data.frame(data)
    
    # Fusion des nouvelles données avec les données précédentes en utilisant rbind
    all_data <- rbind(all_data, df)
    print(nrow(all_data))
    # Mettre à jour la date_debut pour la prochaine requête
    date_debut <- date_fin
  
  }
}

# Rajout des colonnes de adresse dans mon all_data
merged_data <- merge(all_data, adresse, by.x = "Identifiant__BAN", by.y = "id", all.x = TRUE)

# Suppression des lignes où la colonne 'lat' est NA
merged_data <- merged_data[!is.na(merged_data$lat), ]  

# Création du fichier qui sera déposé sur Dropbox 
write.csv(merged_data, "coordonnee2.csv", row.names = FALSE)
