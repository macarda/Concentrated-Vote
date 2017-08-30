## https://stackoverflow.com/questions/8124626/finding-connected-components-of-adjacency-matrix-graph

connected_graph <- function(M) {
  components <- 0
  marks <- rep(0, nrow(M))
  queue <- c()
  ## Enumerate vertices
  for (i in 1:nrow(M)) {
    ## If vertex not visited
    if (marks[i] == 0) {
      components <- components + 1

      ## Put this vertex into queue
      queue <- i

      ## while queue is not empty
      while(length(queue) > 0) {
        ## pop vertex v from q
        v <- queue[1]
        queue <- queue[-1]
        marks[v] <- components
        ## Put all adjacent vertices with marks equal to zero into queue.
        queue <- c(queue, which((M[v,] == 1) & (marks == 0)))
      }
    }
  }
  return(marks)
}

connected_bipartite_graph <- function(R) {
  M <- rbind(cbind(matrix(0, nrow(R), nrow(R)), R),
             cbind(t(R), matrix(0, ncol(R), ncol(R))))
  marks <- connected_graph(M)
  return(list(rows=marks[1:nrow(R)], cols=marks[(nrow(R)+1):(nrow(R)+ncol(R))]))
}

R <- rbind(cbind(matrix(1, 5, 3), matrix(0, 5, 2)),
           cbind(matrix(0, 4, 3), matrix(1, 4, 2)))

message("Unconnected graph...")
print(R)
message("...two groups")
print(connected_bipartite_graph(R))

message("Connected graph...")
R[6, 3] <- 1
print(R)
message("...one group")
print(connected_bipartite_graph(R))
