create_entity <- function(graph, type, mode = 0, database) {
	
	if (type == "component") {
		data <- components(graph)$membership
	} else if (type == "cluster") {
		data <- CooRTweet::detect_cluster(graph, mode)
	} else {
			stop("Invalid input.\n")
		}

	#Calcoliamo le statistiche relative ai gruppi e quelle relative agli account
	tryCatch({
	  group_stat <- CooRTweet::group_stats(coord_graph = data, weight_threshold = "full")
	  account_stat <- CooRTweet::account_stats(coord_graph = data, result = result, weight_threshold = "full")
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

	# summerise
	summary_entities <- coordinated_account_stat %>%
	dplyr::group_by(account_id, video_description) %>%
	dplyr::reframe(
		entities = n(),
		account_id = account_id,
		avg.views = mean(view_count),
		avg.comments = mean(comment_count),
		avg.shares = mean(share_count),
		avg.likes = mean(like_count),
		region = region_code,
		video_description = video_description,
		.groups = 'drop'
	)


	return(summary_entities)
}


# summary_entities <- coordinated_account_stat %>%
# 	dplyr::group_by(account_id, video_description) %>%
# 	dplyr::summarise(
# 		entities = n(),
# 		account_id = account_id,
# 		avg.views = mean(view_count),
# 		avg.comments = mean(comment_count),
# 		avg.shares = mean(share_count),
# 		avg.likes = mean(like_count),
# 		region = region_code,
# 		video_description = video_description,
# 		.groups = 'drop'
# 	)