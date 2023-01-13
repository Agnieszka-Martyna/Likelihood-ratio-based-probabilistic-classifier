KS = function(X,k)
{
  # print(X)
  metric <- "euclidean"
  if (is.data.frame(X)) 
  x <- X <- as.matrix(X)
  m <- nrow(X)
  n <- 1:nrow(X)

	D <- as.matrix(dist(X, method=metric))
	id <- c(arrayInd(which.max(D), rep(m, 2)))
	model <- n[id]
	n <- n[-id]
	
	while (length(model) < k) {
	  d <- D[model, -c(model)]
	  mins_cal <- do.call(pmin.int, lapply(1:nrow(d), function(i) d[i,]))
	  id <- which.max(mins_cal)
	  model <- c(model, n[id])
	  n <- n[-id]} 

	return(list(model=model))
}
