## Below 2 functions calculate inverse of a matrix
## if inverse exist it uses cache value otherwise it calculate


## This  function includes get and set methods for given matrix, 
## It is return a list containing functions
## - set the value of matrix
## - get the value of matrix
## - set the value of inverse matrix
## - get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {  
  m <- NULL
  ## Store matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  ## Set inverse matrix
  setinv <- function(solve) m <<- solve
  
  ## Get stored inverse 
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function calculates the inverse of a matrix 
## which is created by the makeCacheMatrix function. 
## It set this inverse via makeCacheMatrix
## It checks this inverse before calculation if it is exist
## it returns existing inverse

cacheSolve <- function(x, ...) {
        
  ## get inverse matrix 
  m <- x$getinv()
  
  ## if the inverse matrix available return it with a message  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if inverse not available get original matrix 
  data <- x$get()
  
  ## calculate inverse
  m <- solve(data, ...)  
  
  ##  store inverse matrix
  x$setinv(m)
  
  ## return inverse matrix
  m
}





