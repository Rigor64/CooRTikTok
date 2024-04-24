
#Usiamo CoorTweet per l'individuazione di account coordinati in TikTok
#Uso di un LLM per l'identificazione dei diversi network coinvolti e che cosa hanno in comune
#Visualizzazione dei risultati tramite un grafo


#API tiktok per la restituzioine delle informazioni sull'account
#https://developers.tiktok.com/doc/research-api-specs-query-user-info/

#Creazione di una tabella con le informazioni sugli account coordinati, restituite dall'API di tiktok
#Gestione della funzione generate_label per la chiamata alle API di chatGPT
#Risoluzione dei problemi
#Utilizzare lo stesso naming di CoorNet
#gestire la questione di cluster e component

#-----------------------------------------------------------------------------------------------------------------------------------------------

# Caricamento delle librerie
#remotes::install_github("JBGruber/traktok")
library(CooRTweet)
library(igraph)
library(traktok)
library(readr)
library(dplyr)

source("./R/generate_label.R")
source("./R/create_entity.R")
source("./R/detect_cluster.R")

#-----------------------------------------------------------------------------------------------------------------------------------------------

#set.seed(123)

# Verifico che  ci siano le variabili d'ambiente
if (is.na(Sys.getenv("OPENAI_API_KEY")) ) {
  stop("Environment variables for OpenAI API not set. Please set OPENAI_API_KEY.")
}

#Sys.getenv("OPENAI_API_KEY")

#-----------------------------------------------------------------------------------------------------------------------------------------------

#Leggo il CSV e verifico se sono presenti eventuali errori
tryCatch({
  database <- readr::read_csv("./data/tiktok_database.csv",
                                    col_types = cols(video_id = col_character(),
                                                     music_id = col_character()))
}, error = function(e) {
  stop("Failed to read TikTok coordinated accounts ID CSV. Error: ", e$message)
})

#View(tiktok_database)

#-----------------------------------------------------------------------------------------------------------------------------------------------

#Cambio le colonne per renderle compatibili con l'analisi di CoorTweet
change_column <- CooRTweet::prep_data(x = database,
                                  object_id = "video_description", # contenuto del video
                                  account_id = "author_name",      # autore del video
                                  content_id = "video_id",         # id del video
                                  timestamp_share = "create_time") # orario della creazione del video

#Uniformiamo i caratteri della video description
change_column$object_id <- tolower(change_column$object_id)


#Avvio l'analisi dul dataframe modificato, dati i parametri di coordinamento
#Tutti i video che sono stati condivisi in un certo lasso di tempo da un account ad un altro
result <- CooRTweet::detect_groups(x = change_column,
                                   time_window = 150, # intervallo di tempo
                                   min_participation = 2, # numero minimo di ripetizioni
                                   remove_loops = T)



#genero il grafo relativo ai risultati ottenuti
graph <- CooRTweet::generate_coordinated_network(x = result,
                                                       edge_weight = 0.5, # default 0.5
                                                       objects = TRUE,)

#-----------------------------------------------------------------------------------------------------------------------------------------------

#dataframe che somma tutte le informazzioni che abbiamo riguardo i component di account coordinati e le relative descrizioni dei video
summary_entity <- create_entity(graph = graph, database = database)

#generiamo le label a partire dalla descrizione dei video
tiktok_df <- generate_label(summary_entity)

tiktok_df


