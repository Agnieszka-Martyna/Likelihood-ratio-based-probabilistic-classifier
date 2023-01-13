duplex = function(X,Sets)
{
  metric <- "euclidean"
  if (is.data.frame(X)) 
  x <- X <- as.matrix(X)
  m <- nrow(X)
  n <- 1:nrow(X)
  # half <- floor(m/2)
  # if (k > half) 
  # k <- half
  
  k = m/Sets
  
  if (Sets==2)
  {
    D <- as.matrix(dist(X, method=metric))
    id <- c(arrayInd(which.max(D), rep(m, 2)))
    model <- n[id]
    n <- n[-id]
    id <- c(arrayInd(which.max(D[n,n]), rep(m - 2, 2)))
    test1 <- n[id]
    n <- n[-id]
    
    while (length(model) < k) {
      if (length(n) != 1)
      {d <- D[model, -c(model, test1)]
      mins_cal <- do.call(pmin.int, lapply(1:nrow(d), function(i) d[i,]))
      id <- which.max(mins_cal)
      model <- c(model, n[id])
      n <- n[-id]} else {model = c(model,n);n = NULL}
      
      if (length(n) > 1)
      {d <- D[test1, -c(model, test1)]
      mins_cal <- do.call(pmin.int, lapply(1:nrow(d), function(i) d[i,]))
      id <- which.max(mins_cal)
      test1 <- c(test1, n[id])
      n <- n[-id]} else {test1 = c(test1,n);n = NULL}
      
      test2=NULL
    }
  } else
  {
    D <- as.matrix(dist(X, method=metric))
    id <- c(arrayInd(which.max(D), rep(m, 2)))
    model <- n[id]
    n <- n[-id]
    
    id <- c(arrayInd(which.max(D[n,n]), rep(m - 2, 2)))
    test1 <- n[id]
    n <- n[-id]
    
    id <- c(arrayInd(which.max(D[n,n]), rep(m - 4, 2)))
    test2 <- n[id]
    n <- n[-id]
    
    while (length(model) < k) {
      if (length(n) != 1)
      {d <- D[model, -c(model, test1,test2)]
      mins_cal <- do.call(pmin.int, lapply(1:nrow(d), function(i) d[i,]))
      id <- which.max(mins_cal)
      model <- c(model, n[id])
      n <- n[-id]} else {model = c(model,n);n = NULL}
      
      if (length(n) > 1)
      {d <- D[test1, -c(model, test1,test2)]
      mins_cal <- do.call(pmin.int, lapply(1:nrow(d), function(i) d[i,]))
      id <- which.max(mins_cal)
      test1 <- c(test1, n[id])
      n <- n[-id]} else {test1 = c(test1,n);n = NULL}
    
      if (length(n) > 1)
      {d <- D[test2, -c(model, test1,test2)]
      mins_cal <- do.call(pmin.int, lapply(1:nrow(d), function(i) d[i,]))
      id <- which.max(mins_cal)
      test2 <- c(test2, n[id])
      n <- n[-id]} else {test2 = c(test2,n);n = NULL}
    }
  }
  
  return(list(model=model,test1=test1,test2=test2))
}

    
