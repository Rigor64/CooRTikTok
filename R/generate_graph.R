
generate_label <- function(graph,
){

}



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

    component_edges <- subset(tiktok_df,tiktok_df$component == j)
    component_edges <- arrange(component_edges, weight)


    n <- ifelse(nrow(component_edges) / 100 * 20 > 5,
                round(nrow(component_edges) / 100 * 20, 0), 5)

    component_edges <- dplyr::slice_head(.data = component_edges, n = n)

    #compongo la lista di tutte le descrizioni dei video associate a quel component
    text <- paste(trimws(component_edges$object_ids), collapse = "\n")

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
