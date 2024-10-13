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
options(rsconnect.max.bundle.size = 300000000000)
options(rsconnect.max.bundle.files = 165000)

# Charger les données
data_initial <- read.csv("https://www.dropbox.com/scl/fi/kmrtwqkbqof6ofa94mvvr/cordonn-_2.csv?rlkey=9j5uftpb4c6reu9o4ll3h2gtl&st=oic1udaa&dl=1", 
                         sep = ",", 
                         header = TRUE, 
                         check.names = FALSE)



fetch_new_data <- function() {
  
  url <- "https://www.dropbox.com/scl/fi/ynrx2vhsuz7qhwridk80g/adresse_final2.csv?rlkey=b84yh3410a976h2j3u7aoslq3&st=lrq44zv7&dl=1"
  adresse <- read.csv(url,   sep = ",", 
                      header = TRUE, 
                      check.names = FALSE)
  url <- "https://www.dropbox.com/scl/fi/kmrtwqkbqof6ofa94mvvr/cordonn-_2.csv?rlkey=9j5uftpb4c6reu9o4ll3h2gtl&st=6jlnuga2&dl=1"
  coordoné <- read.csv(url,   sep = ",", 
                       header = TRUE, 
                       check.names = FALSE)
  
  
  base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
  
  
  cp <- unique(coordoné[["Code_postal_(BAN)"]])
  
  
  all_data <- data.frame()
  date_debut <- "2024-10-09"
  date_fin <- Sys.Date()
  
  
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
  }
  date_debut <- Sys.Date()
  
  # Vérification si all_data est un data frame et s'il est vide
  if (is.data.frame(all_data) && nrow(all_data) == 0) {
    # Si all_data est vide, merged_data2 prend les valeurs de data_initial2
    merged_data2 <- coordoné
    
  } else {
    # Si all_data n'est pas vide, on vérifie les colonnes
    if (!all(c("Identifiant__BAN", "lon", "lat", "code_postal") %in% colnames(coordoné))) {
      stop("Les colonnes 'Identifiant__BAN', 'lon', 'lat', ou 'code_postal' manquent dans data2.")
    }
    
    # Fusionner all_data avec les colonnes spécifiques de data2
    merged_data <- merge(all_data, adresse, by.x = "Identifiant__BAN", by.y = "id", all.x = TRUE)
    
    # Suppression des lignes où la colonne 'lat' est NA
    merged_data <- merged_data[!is.na(merged_data$lat), ]
    
    # Combiner les données fusionnées avec data_initial2
    merged_data2 <- rbind(merged_data, coordoné)
  }
  
  return(merged_data2)
}


ui <- fluidPage(
  useShinyjs(),
  
  # Ajout des styles CSS
  tags$head(
    tags$style(HTML("
      .login-container {
        width: 300px;
        padding: 20px;
        border-radius: 10px;
        background-color: #f9f9f9;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      }
      
      .login-container input {
        width: 100%;
        padding: 10px;
        margin-bottom: 10px;
        border-radius: 5px;
        border: 1px solid #ccc;
        box-sizing: border-box;
      }
      
      .login-container button {
        width: 100%;
        padding: 10px;
        background-color: #337ab7;
        color: white;
        border: none;
        border-radius: 5px;
        cursor: pointer;
      }
      
      .login-container button:hover {
        background-color: #286090;
      }
      
      .login-message {
        text-align: center;
        font-size: 14px;
        margin-top: 15px;
        color: #666;
      }

      .sidebar-content {
        width: 300px;
        padding: 10px;
        background-color: #f7f7f7;
        height: 100vh;
        overflow-y: auto;
        box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
      }

      .map-content {
        height: 100vh;
      }

      .info-complementaire {
        width: 300px;
        margin-top: 20px;
        padding: 15px;
        border: 1px solid #ccc;
        border-radius: 5px;
        background-color: #f9f9f9;
        position: absolute;
        bottom: 20px;
        left: 20px;
      }

      .info-title {
        font-weight: bold;
        margin-bottom: 10px;
      }

      .export-buttons {
        text-align: left;
        margin-top: 15px;
      }

      .button-spacing {
        margin-bottom: 10px; /* Espace de 10 pixels entre les boutons */
      }

      /* Thème Clair */
      body.light-theme {
        background-color: #ffffff;
        color: #000000;
      }
      
/* Thème Sombre */
body.dark-theme {
    background-color: #333333;
    color: #ffffff;
}

/* Applique le thème sombre à tous les éléments de la page */
body.dark-theme * {
    color: #ffffff; /* Texte en blanc dans le thème sombre */
}

/* Applique un fond noir aux conteneurs en mode sombre */
body.dark-theme .login-container,
body.dark-theme .sidebar-content,
body.dark-theme .info-complementaire,
body.dark-theme .content-container, /* Conteneur principal des autres pages */
body.dark-theme .tab-content, /* Conteneur pour les onglets */
body.dark-theme .leaflet-container, /* Conteneur pour la carte interactive */
body.dark-theme .plot-container, /* Conteneur pour les graphiques */
body.dark-theme .table-container { /* Conteneur pour les tableaux */
    background-color: #000000; /* Fond noir pour tous les conteneurs */
    border-color: #444444; /* Bordures plus sombres */
}

/* Champ de texte et labels en mode sombre */
body.dark-theme input {
    background-color: #444444; /* Fond sombre pour les champs de texte */
    color: #ffffff; /* Texte blanc */
}

body.dark-theme label {
    color: #ffffff; /* Texte blanc pour les labels */
}

    body.dark-theme input[type='checkbox'] {
        accent-color: #ffffff; /* Couleur blanche pour les cases cochées */
    }

    body.dark-theme input[type='checkbox'] + label {
        color: #ffffff; /* Couleur du texte associé aux cases à cocher */
    }

/* Boutons en mode sombre */
body.dark-theme button {
    background-color: #555555; /* Fond sombre pour les boutons */
    color: #ffffff; /* Texte blanc sur les boutons */
}

/* Thème GIF - Fond avec un GIF */
body.gif-theme {
    background-image: url('https://media.giphy.com/media/7b8jdNUoFBdcoILjjv/giphy.gif?cid=ecf05e4703ykh41eb5h4oi3lu8e5cp48cci9d3hgj0y8ibrk&ep=v1_gifs_related&rid=giphy.gif&ct=g');
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    z-index: -1; /* S'assurer que le GIF est en arrière-plan */
    width: 100vw; /* Largeur de 100% de la fenêtre */
    height: 100vh; /* Hauteur de 100% de la fenêtre */
    object-fit: cover; /* Couvre l'élément sans déformer l'image */
    background-repeat: no-repeat; /* Ne pas répéter le fond */
    background-size: cover; /* Couvrir tout l'élément */
}
.plot-container {
    max-height: 80vh; /* Ajustez la hauteur maximale selon vos besoins */
    overflow-y: auto; /* Permet le défilement vertical */
    padding: 10px; /* Ajoutez un peu de padding si nécessaire */
}



  "))
  ),
  
  selectInput("theme_choice", "Choisissez le thème :", choices = list("Clair" = "light", "Sombre" = "dark", "GIF" = "gif")),
  uiOutput("loginUI"),
  uiOutput("appUI"),
  
  # Afficher le logo uniquement si l'utilisateur est authentifié
  conditionalPanel(
    condition = "output.isAuthenticated",  # Condition qui vérifie si l'utilisateur est authentifié
    tags$img(src = "https://github.com/Utopie81/iut_sd2_rshiny_enedis_EMS2/raw/main/Projets_Importation/logo.webp", class = "footer-logo")
  )
)

# Serveur
server <- function(input, output, session) {
  
  user_authenticated <- reactiveVal(FALSE)
  last_refresh_date <- reactiveVal("2024-10-9")  # Variable pour la date du dernier clic
  user_message <- reactiveVal("")  # Variable pour le message utilisateur
  
  observeEvent(input$theme_choice, {
    if (input$theme_choice == "gif") {
      addClass(selector = "body", class = "gif-theme")
      removeClass(selector = "body", class = "light-theme dark-theme")
    } else if (input$theme_choice == "light") {
      addClass(selector = "body", class = "light-theme")
      removeClass(selector = "body", class = "dark-theme gif-theme")
    } else {
      addClass(selector = "body", class = "dark-theme")
      removeClass(selector = "body", class = "light-theme gif-theme")
    }
  })
  
  
  observeEvent(input$login, {
    if (input$username == "ahouy" && input$password == "KgTHFJrULZwau7yXno") {
      user_authenticated(TRUE)
      showNotification("Connexion réussie!", type = "message")
    } else {
      showNotification("Nom d'utilisateur ou mot de passe incorrect", type = "error")
    }
  })
  
  output$loginUI <- renderUI({
    if (!user_authenticated()) {
      div(
        class = "login-container",
        textInput("username", "Nom d'utilisateur"),
        passwordInput("password", "Mot de passe"),
        actionButton("login", "Se connecter"),
        div(class = "info-complementaire",
            div(class = "info-title", "Information Complémentaire"),
            textOutput("username_info"),
            textOutput("password_info"),
            tags$p("Nom d'utilisateur : ahouy"),
            tags$p("Mots de passe : KgTHFJrULZwau7yXno") 
        )
      )
    }
  })
  
  
  # Ajout d'une zone de texte pour afficher la date de dernière actualisation
  output$appUI <- renderUI({
    if (user_authenticated()) {
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            width = 3,
            checkboxGroupInput("dpe_filter", "Sélectionnez les étiquettes DPE :", 
                               choices = sort(unique(data_initial$Etiquette_DPE))),
            actionButton("refresh_data", "Actualiser les données", class = "button-spacing"),
            textOutput("last_update"),  
            tags$br(),
            textOutput("prevention_text")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Cartographie Interactif", leafletOutput("map", height = "85vh"),
                       tags$img(src = "https://github.com/Utopie81/iut_sd2_rshiny_enedis_EMS2/raw/main/Projets_Importation/logo.webp",
                                style = "position: fixed; bottom: 10px; left: 10px; height: 250px;")
              ),
              tabPanel("Graphiques",
                       tags$br(),
                       downloadButton("download_barplot", "Télécharger Diagramme en barres", class = "button-spacing"),
                       plotOutput("barplot"),
                       tags$br(),
                       
                       
              ),
              tabPanel(
                "Tableau des données", 
                dataTableOutput("data_table"),
                downloadButton("download_data", "Télécharger les données", class = "button-spacing"),
                tags$img(src = "https://github.com/Utopie81/iut_sd2_rshiny_enedis_EMS2/raw/main/Projets_Importation/logo.webp",
                         style = "position: fixed; bottom: 10px; left: 10px; height: 250px;")
              )
            )
          )
        )
      )
    }
  })
  output$last_update <- renderText({
    paste("Dernière actualisation :", format(Sys.Date() - 1, "%Y-%m-%d"))  # Date par défaut : hier
  })
  
  
  output$prevention_text <- renderText({
    "Au vue du nombre de donées le refresh données peut prendre quelque minutes"
  })
  
  
  reactive_data <- reactiveVal(data_initial)
  
  
  observeEvent(input$refresh_data, {
    result <- fetch_new_data() 
    reactive_data(result) 
    
    
    output$last_update <- renderText({
      paste("Dernière actualisation :", format(Sys.Date(), "%Y-%m-%d"))
    })
    
    
    showNotification(paste("Nouvelles données importées :", nrow(result)), type = "message")
  })
  
  
  filtered_data <- reactive({
    data_filtered <- reactive_data() 
    
    if (!is.null(input$code_postal) && length(input$code_postal) > 0) {
      data_filtered <- subset(data_filtered, Code_Postal %in% input$code_postal)
    }
    
    if (!is.null(input$dpe_filter) && length(input$dpe_filter) > 0) {
      data_filtered <- subset(data_filtered, Etiquette_DPE %in% input$dpe_filter)
    }
    
    data_filtered$color <- color_map[data_filtered$Etiquette_DPE]
    
    return(data_filtered)
  })
  
  
  output$data_table <- renderDataTable({
    
    data_to_display <- filtered_data()
    
    if (nrow(data_to_display) == 0) {
      showNotification("Aucune donnée trouvée pour les filtres appliqués", type = "warning")
      return(data.frame(Message = "Aucune donnée disponible avec les filtres actuels"))
    }
    
    
    datatable(data_to_display, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_table <- renderDataTable({
    
    data_to_display <- filtered_data()
    
    
    if (nrow(data_to_display) == 0) {
      showNotification("Aucune donnée trouvée pour les filtres appliqués", type = "warning")
      return(data.frame(Message = "Aucune donnée disponible avec les filtres actuels"))
    }
    
    
    datatable(data_to_display, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  
  
  
  
  
  output$barplot <- renderPlot({
    effectif_habitations <- as.data.frame(table(filtered_data()$Etiquette_DPE))
    colnames(effectif_habitations) <- c("Etiquette_DPE", "Effectif")
    
    
    effectif_habitations$color <- color_map[effectif_habitations$Etiquette_DPE]
    
    ggplot(effectif_habitations, aes(x = Etiquette_DPE, y = Effectif, fill = color)) +
      geom_bar(stat = "identity") +
      labs(title = "Diagramme en barres", x = "Etiquette DPE", y = "Nombre d'habitations") +
      theme_minimal() +
      geom_text(aes(label = Effectif), position = position_stack(vjust = 0.5), size = 5, color = "black") +
      scale_fill_identity()  
  })
  
  
  color_map <- c(
    "A" = "#00FF00",        
    "B" = "#7FFF00",   
    "C" = "yellow",       
    "D" = "gold",       
    "E" = "orange",   
    "F" = "#FF4500",          
    "G" = "red"       
  )
  
  
  output$map <- renderLeaflet({
    
    unique_labels <- unique(filtered_data()$Etiquette_DPE)
    
    data_to_plot <- filtered_data()
    data_to_plot$Etiquette_DPE <- as.character(data_to_plot$Etiquette_DPE)
    
    
    data_to_plot$color <- color_map[data_to_plot$Etiquette_DPE]
    
    
    
    leaflet(data_to_plot) %>% 
      addTiles() %>% 
      addCircleMarkers(
        ~lon, 
        ~lat, 
        popup = ~paste("Étiquette DPE : ", Etiquette_DPE),
        color = ~color, 
        opacity = 0.8,   
        fillOpacity = 0.8, 
        clusterOptions = markerClusterOptions()
      )
  })
  
  
  
  output$download_barplot <- downloadHandler(
    filename = function() {
      paste("diagramme_barres", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(ggplot2::last_plot())
      dev.off()
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      
      req(filtered_data())  
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  observeEvent(input$refresh_data, {
    result <- fetch_new_data()
    reactive_data(result)
    showNotification(paste("Nouvelles données importées :", nrow(result)), type = "message")
  })
}
shinyApp(ui = ui, server = server)