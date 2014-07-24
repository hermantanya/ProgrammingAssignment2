## makeCacheMartix function can cache an inverse of a given matrix
## Returns functions: set - sets a martix, get - gets a cashed matrix, setinverse - sets an inversed matrix, getinverse - gets a cashed inverse matrix 
## casheSolve function returns a matrix that is an inverse of a given matrix. 
## If the result is already cashed - returns the caches inverse, otherwise - calculates the inverse of a given martix

## The function caches an inverse of a matrix x, and returns fnctions: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  
{
  #x and m are internal variables of the returned object, they will not be accessed directly, only through the internal functions
  # set, get, setinverse, getinverse
  
  m <- NULL    # saves ("caches") the inverse of x
  
  # set internal matrix x, to input value of y
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get internal matrix x
  get <- function() {
    return(x)
  }
  
  
  setinverse <- function(solve) {
    m <<- solve
  }
  
  getinverse <- function() {
    return(m)
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  

}


## The function returns a matrix that is an inverse of a given matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}
