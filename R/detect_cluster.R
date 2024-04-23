# Definizione funzione detect_cluster() 
# Attraverso la scelta di un algoritmo di community detection, 
# permette il rilevamento di cluster
detect_cluster <- function(graph, mode) {
	if (is_connected(graph)) {
		clusters <- switch(
			mode,
			1 = cluster_edge_betweenness(
			  graph,
			  weights = NULL,
			  directed = TRUE,
			  edge.betweenness = TRUE,
			  merges = TRUE,
			  bridges = TRUE,
			  modularity = TRUE,
			  membership = TRUE
			),
			2 = cluster_fast_greedy(
			  graph,
			  merges = TRUE,
			  modularity = TRUE,
			  membership = TRUE,
			  weights = NULL
			),
			3 = cluster_label_prop(
				graph, 
				weights = NULL, 
				initial = NULL, 
				fixed = NULL
			),
			4 = cluster_leading_eigen(
			  graph,
			  steps = -1,
			  weights = NULL,
			  start = NULL,
			  options = arpack_defaults(),
			  callback = NULL,
			  extra = NULL,
			  env = parent.frame()
			),
			5 = cluster_leiden(
			  graph,
			  objective_function = c("CPM", "modularity"),
			  weights = NULL,
			  resolution_parameter = 1,
			  beta = 0.01,
			  initial_membership = NULL,
			  n_iterations = 2,
			  vertex_weights = NULL
			),
			6 = cluster_louvain(
				graph, 
				weights = NULL, 
				resolution = 1
			)
		)
		# Per grafi connessi
	} else if (!is_connected(graph)) {
		cluster_spinglass(
		  graph,
		  weights = NULL,
		  vertex = NULL,
		  spins = 25,
		  parupdate = FALSE,
		  start.temp = 1,
		  stop.temp = 0.01,
		  cool.fact = 0.99,
		  update.rule = c("config", "random", "simple"),
		  gamma = 1,
		  implementation = c("orig", "neg"),
		  gamma.minus = 1
		)
	} else {
		# Genero messaggio di errore
		stop("Invalid input.\n")
	}
}