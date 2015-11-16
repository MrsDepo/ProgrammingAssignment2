######################################################
# These fuctions are designed to compute the inverse #
# of a matrix and then cache the result in order to  #
# save computing time when the inverse is needed     #
# again.                                             #
######################################################


######################################################
# The fuction makeCacheMatrix() takes a square       #
# matrix input and creates a list containing the     #
# fuctions to get the matrix (get), set the inverse  #
# of the matrix (setinverse), and get the inverse of #
# the matrix.                                        #
######################################################
makeCacheMatrix <- function(x = matrix()) {
  ##set inv to NULL, then set up internal functions for caching
  ##and retrieving inverse matrix.
  inv <- NULL 
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


######################################################
# The function cacheSolve() first checks to see if   #
# the matrix inverse has already been cached and, if #
# so, returns the cached result. If not, it will use #
# solve to calculate the inverse and store it using  #
# setinverse(inv) then return the inverse.           #
######################################################
cacheSolve <- function(x, ...) {
  ##First check to see if inv has already been cached.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##If inv has not been cached, get the inverse and cache it.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}