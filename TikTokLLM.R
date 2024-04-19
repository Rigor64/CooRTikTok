
#Usiamo CoorTweet per l'individuazione di account coordinati in TikTok
#Uso di un LLM per l'identificazione dei diversi network coinvolti e che cosa hanno in comune
#Visualizzazione dei risultati tramite un grafo

#-----------------------------------------------------------------------------------------------------------------------------------------------

# Caricamento delle librerie

library(CooRTweet)
library(igraph)
library(traktok)
library(readr)
library(dplyr)

#-----------------------------------------------------------------------------------------------------------------------------------------------

set.seed(123)

# Verifico che  ci siano le variabili d'ambiente
if (is.na(Sys.getenv("OPENAI_API_KEY")) ) {
  stop("Environment variables for OpenAI API not set. Please set OPENAI_API_KEY.")
}



#Leggo il CSV e verifico se sono presenti eventuali errori
tryCatch({
  tiktok_database <- readr::read_csv("./data/tiktok_database.csv",
                                    col_types = cols(video_id = col_character(),
                                                     music_id = col_character()))
}, error = function(e) {
  stop("Failed to read TikTok coordinated accounts ID CSV. Error: ", e$message)
})

View(tiktok_database)

#-----------------------------------------------------------------------------------------------------------------------------------------------

#Cambio le colonne per renderle compatibili con l'analisi di CoorTweet
change_column <- CooRTweet::prep_data(x = tiktok_database,
                                  object_id = "video_description", # contenuto del video
                                  account_id = "author_name",      # autore del video
                                  content_id = "video_id",         # id del video
                                  timestamp_share = "create_time") # orario della creazione del video

#Avvio l'analisi dul dataframe modificato, dati i parametri di coordinamento
#Tutti i video che sono stati condivisi in un certo lasso di tempo da un account ad un altro
result <- CooRTweet::detect_groups(x = change_column,
                                   time_window = 150, # intervallo di tempo
                                   min_participation = 2, # numero minimo di ripetizioni
                                   remove_loops = T)

#genero il grafo relativo ai risultati ottenuti
tiktok_graph <- CooRTweet::generate_coordinated_network(x = result,
                                                       edge_weight = 0.5, # default 0.5
                                                       objects = TRUE,)

#Estraggo per ciascun componente della rete la lista degli object usati

# Convertiamo il grafo in due dataframe: uno per gli edge e l'altro per i vertici
tiktok_edge_df <- igraph::as_data_frame(tiktok_graph, what = "edges")
tiktok_vertices_df <- igraph::as_data_frame(tiktok_graph, what = "vertices")

#Creo un dataframe unico in cui aggiungo il component corrispondente ad ogni account coinvolto
tiktok_df <- merge(tiktok_edge_df, tiktok_vertices_df, by.x = "from", by.y = "name")

#salvo il numero di component, trovando quello con l'identificativo più alto
n_components <- length(unique(tiktok_df$component))

#-----------------------------------------------------------------------------------------------------------------------------------------------

#Chiamata all'API di ChatGPT

#Verifico che ci sia la chiave
if (labels & Sys.getenv("OPENAI_API_KEY") != "") {

  #inizializzo un dataframe temporaneo
  temp_df <- data.frame(cluster_id = integer(), label = character())

  cat("\nAuto-labelling clusters with OpenAI gpt-3.5-turbo (https://platform.openai.com/)...\n")

  pb <- utils::txtProgressBar(min = 0, max = n_components, width = 100, style = 3)

  for (j in 1:n_components) {

    cluster_accounts <- subset(tiktok_df,
                               tiktok_df$component == 1)
    cluster_accounts <- arrange(cluster_accounts, avg_time_delta)

    n <- ifelse(nrow(cluster_accounts) / 100 * 20 > 5,
                round(nrow(cluster_accounts) / 100 * 20, 0), 5)

    cluster_accounts <- dplyr::slice_head(.data = cluster_accounts, n = n)
    #cluster_accounts$comtxt <- paste(cluster_accounts$object_ids, ifelse(cluster_accounts$object_ids != "NA", cluster_accounts$object_ids, ""))

    #compongo la lista di tutte le descrizioni dei video associate a quel component
    text <- paste(trimws(cluster_accounts$object_ids), collapse = "\n")
    text

    #scrivo il messaggio da inviare a chatgpt
    msg <- list(list("role" = "system",
                     "content" = "You are a researcher investigating coordinated and inauthentic behavior on TikTok. Your objective is to generate concise, descriptive labels in English that capture the shared video description of clusters of TikTok.\n\n"),
                list("role" = "user",
                     "content" = paste("I will supply a list of video descriptions for each cluster. Identify the shared features among these descriptions:\n\n",
                                       text,
                                       "\n\n",
                                       "English label:")))

    #richiamiamo la versione di chatgtp da utilizzare e passo il messaggio
    res <- tryCatch(
      {
        openai::create_chat_completion(model = "gpt-3.5-turbo",
                                       messages = msg,
                                       temperature = 0,
                                       top_p = 1,
                                       max_tokens = 256)
      },
      error = function(cond) {
        return(NULL)
      })

    #se abbiamo avuto risoslta da chatgpt, accodiamo il risultato del component j-esimo
    if (!is.null(res)) {
      temp_df <- rbind(temp_df, data.frame(cluster_id = j, label = res$choices$message.content))
      # print(paste(j, "-", res$choices$message.content))
    }
    utils::setTxtProgressBar(pb, pb$getVal() + 1)
    text <- NULL
    Sys.sleep(0.5)
  }

  #aggiungo al dataframe che abbiamo tutte le descrizioni dei component che abbiamo
  tiktok_df <- merge(tiktok_df, temp_df, by.x = "component", by.y = "cluster_id")
}

#-----------------------------------------------------------------------------------------------------------------------------------------------

#Visualizzazione dei risultati

tiktok_df









#Prove

#-----------------------------------------------------------------------------------------------------------------------------------------------

#Calcoliamo le statistiche relative ai gruppi e quelle relative agli account
tryCatch({
  group_stat <- CooRTweet::group_stats(coord_graph = tiktok_graph, weight_threshold = "full")
  account_stat <- CooRTweet::account_stats(coord_graph = tiktok_graph, result = result, weight_threshold = "full")
}, error = function(e) {
  stop("Failed to calculate summary statistics for groups or accounts: ", e$message)
})

# Uniamo i dataframe sulla base della corrispondenza tra video_description e object_id
correlated_accounts <- tiktok_database %>%
  filter(video_description %in% group_stat$object_id) %>%
  select(author_name)

# Aggiungiamo la nuova colonna al dataframe group_stat
group_stat <- group_stat %>%
  mutate(correlated_account = list(correlated_accounts$author_name))


#aggiungiamo al dataframe account_stat le descrizioni dei video associate, sulla base del dataframe originale

# Aggiungo le informazioni del database iniziale agli account coordinati trovati
coordinated_account_stat <- account_stat %>%
  left_join(tiktok_database, by = c("account_id" = "author_name"))





