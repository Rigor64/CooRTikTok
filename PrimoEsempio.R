library(CooRTweet)
library(igraph)

#seed per la replicazione di un esperimento
set.seed(123)
russian_coord_tweets

length(russian_coord_tweets$content_id) == nrow(russian_coord_tweets)

result <- detect_groups(russian_coord_tweets,
                        min_participation = 2,
                        time_window = 600)

#account_id = account che ha posta il contenuto per primo
#account_id_y = account che ha posta il contenuto per ultimo

combined_accounts <- c(result$account_id, result$account_id_y)
combined_accounts_dt <- data.table::data.table(account_id = combined_accounts)
account_counts <- combined_accounts_dt[, .N, by = account_id]

russian_coord_tweets <- data.table::as.data.table(russian_coord_tweets)
raw_counts <- russian_coord_tweets[, .N, by = account_id]
raw_counts_included <- raw_counts[account_id %in% combined_accounts]

# min_participation
min(raw_counts_included$N)

coord_graph <- generate_coordinated_network(result, edge_weight = 0.99, objects = TRUE)

min(E(coord_graph)$weight[E(coord_graph)$weight_threshold == 1])

#poichè usiamo l'edge_weight, che non è una metrica adeguata. Allora, calcoliamo un edge_simmetry_score, che calcola da quale due due account c'è maggiore coinvolgimento nell'attività di coordinamento

summary_groups <- group_stats(coord_graph = coord_graph, weight_threshold = "full")

summary_accounts <- account_stats(coord_graph = coord_graph, result = result, weight_threshold = "full")

result_update <- flag_speed_share(russian_coord_tweets, result, min_participation = 2, time_window = 120)

coord_graph_fast <-
  generate_coordinated_network(
    result_update,
    fast_net = TRUE,
    edge_weight = 0.99,
    subgraph = 2
  )

#creazione di un dataset passando i parametri, funzione customizzabile per la preparazione del dataframe
prep_data <-
  function(x,
           object_id = NULL,
           account_id = NULL,
           content_id = NULL,
           timestamp_share = NULL
  )




