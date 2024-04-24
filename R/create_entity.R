
create_entity <- function(graph, database, type = TRUE, mode = 0) {

  source("./R/detect_cluster.R")


	if (type == TRUE) {
		# Individuazione dei componenti
		data <- components(graph)$membership

		# Aggiunta del parametro componente al grafo
		V(graph)$component <- data

	} else if (type == FALSE) {
		# Individuazione dei claster
		data <- CooRTweet::detect_cluster(graph, mode)

		# Aggiunta del parametro componente al grafo
		V(graph)$component <- data$membership
	} else {
			stop("Invalid input.\n")
	}

  graph_vertices_df <- igraph::as_data_frame(graph, what = "vertices")

  #Calcoliamo le statistiche relative ai gruppi e quelle relative agli account
  tryCatch({
    group_stat <- CooRTweet::group_stats(coord_graph = graph, weight_threshold = "full")
    account_stat <- CooRTweet::account_stats(coord_graph = graph, result = result, weight_threshold = "full")
  },
  error = function(e) {
    stop("Failed to calculate summary statistics for groups or accounts: ", e$message)
  })

  # Uniamo i dataframe sulla base della corrispondenza tra video_description e object_id
  correlated_accounts <- database %>%  filter(video_description %in% group_stat$object_id) %>%
    select(author_name)
  # Aggiungiamo la nuova colonna al dataframe group_stat
  group_stat <- group_stat %>%
    mutate(correlated_account = list(correlated_accounts$author_name))

  #aggiungiamo al dataframe account_stat le descrizioni dei video associate, sulla base del dataframe originale
  # Aggiungo le informazioni del database iniziale agli account coordinati trovati
  coordinated_account_stat <- account_stat %>%  left_join(database, by = c("account_id" = "author_name"))

  coordinated_account_stat <- merge(coordinated_account_stat, graph_vertices_df, by.x ="account_id", by.y  = "name")

	# summerise
	summary_entity <- coordinated_account_stat %>%
	dplyr::group_by(component) %>%
	dplyr::reframe(
		num_account = n(),
		avg.views = mean(view_count),
		avg.comments = mean(comment_count),
		avg.shares = mean(share_count),
		avg.likes = mean(like_count),
		most_frequent_region = names(sort(table(region_code), decreasing = TRUE))[1],
		video_descriptions = list(names(sort(table(video_description), decreasing = TRUE)))
	)


	return(summary_entity)
}
