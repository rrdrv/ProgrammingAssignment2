#The first function, makeCacheMatrix creates a special "matrix".

##makeVector changed to makeCacheMatrix; list changed to matrix; x = numeric changed to x = matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  matrix(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

#The following function calculates the inverse of the special "matrix" created with the above function.
#However, it first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
  m <- x$solve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$solve(m)
  m
}
