## Computing the inverse of a square matrix


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){ 
  I <- NULL
  set <- function(y){
    x <<- y
    Inv <<- NULL
  }
  
  get <- function() {x}
  set_I <- function(k) {I <<- k}
  get_I <- function() {I}
  list(get = get , set = set, get_I = get_I, set_I = set_I)
  
}
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated , then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  I <- x$get_I()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data <- x$get() 
  I <- solve(data, ...)
  x$set_I(I)
  I
}