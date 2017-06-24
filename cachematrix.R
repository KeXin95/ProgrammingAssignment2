## Put comments here that give an overall description of what your
## functions do

##This function creates a special "matrix" object that 
#can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #A function "set" that will create a matrix object 
  set <- function(y){
    x <<- y 
    inv <<- NULL
  }
  
  #A function "get" that will return the cached matrix
  get <- function() x
  
  #A function "setinv" will set the inverse of the matrix with the given inverse
  setinv <- function(newinv) inv <<- newinv
  
  #A function "getinv" will return the cached inverse 
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. If the
#inverse has already been calculated (and the matrix
# has not changed), then the cachesolve will retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if ( !is.null(inv) ){
      message("getting cached data")
      return(inv)
    }
    data  <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}

#Testing the functions
cat("Testing the functions...")
testmat <- matrix( c(5, 1, 0,
                     3,-1, 2,
                     4, 0,-1), nrow=3, byrow=TRUE) 
specialmat = makeCacheMatrix(testmat)

specialmat$get()

specialmat$getinv()    

cacheSolve(specialmat)
specialmat$getinv()

