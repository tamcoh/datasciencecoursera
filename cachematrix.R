
## The makeCacheMatrix funciton accepts a matrix object and produces an object that is
## a *special* matrix, to be used in conjunction with the cacheSolve function to cache its own inverse. 


makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {
                  x <<- y
                  m <<- NULL
              }
              get <- function() x
              setinverse <- function(solve) m <<- solve
              getinverse <- function() m
              list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)

}


## The cacheSolve function takes a matrix as its argument and then calls on the 
## getinverse function (that's part of the makeCacheMatrix function) to check if 
## that matrix's inversion already exists in cache, and if so retrieve it.
## If the matrix isn't found in cache, the function will perform the matrix inversion
## using solve() and then insert that value into cache (using setinverse, also a 
## part of the makeCacheMatrix function), so it can be called directly from cache
## in the future. 

cacheSolve <- function(x, ...) {
          m <- x$getinverse()
          if(!is.null(m)) {
              message("getting cached data")
              return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setinverse(m)
          m
}  
        ## Return a matrix that is the inverse of 'x'

