# Definizione funzione detect_cluster()
# Attraverso la scelta di un algoritmo di community detection,
# permette il rilevamento di cluster
detect_cluster <- function(graph) {
  clusters <- cluster_louvain(
    graph,
    weights = NULL,
    resolution = 1
  )

  return (clusters)

}

