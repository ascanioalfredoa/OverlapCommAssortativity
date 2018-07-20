graph.strength.distribution <- function (graph, cumulative = FALSE, ...)
{
if (!is.igraph(graph)) {
stop("Not a graph object")
}
# graph.strength() instead of degree()
cs <- graph.strength(graph, ...)
hi <- hist(cs, -1:max(cs), plot = FALSE)$density
if (!cumulative) {
res <- hi
}
else {
res <- rev(cumsum(rev(hi)))
}
res
}