#The first function, makeCacheMatrix creates a special "matrix", containing a function to
#set the value of the matrix
#get the value of the matirx
#set the value of the mean
#get the value of the mean

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

#The following function calculates the mean of the special "matrix" created with the above function.
#However, it first checks to see if the mean has already been calculated.
#If so, it gets the mean from the cache and skips the computation.
#Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

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
