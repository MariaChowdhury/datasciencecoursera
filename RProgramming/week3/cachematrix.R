#The first function, makeCacheMatrix creates a special "vector", 
#which is really a list containing a function to
# - set the value of the vector
# - get the value of the vector
# - set the value of the inverse
# - get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


##cacheSolve computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}

#test function tests the above method
test<-function(){
  set.seed(123)
  mat<-matrix(rnorm(1000),nrow=5,ncol = 5)
  res<-makeCacheMatrix(mat)
  cacheSolve(res)
}



