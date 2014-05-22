## The Cache Matrix package has two functions that are used to cashe 
## the computed (inverse) matrix and use it, when appropriate, to solve 
## a linear equation. When the inverse matrix is used repeatedly in
## certain situations, using the cached matrix, rather computing it 
## again and again, saves total computation time.

## makeCasheMatrix() can set or get a matrix in the parent environment
## It can also set or get the Inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of x, and put it in cache
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(invMX) inv <<- invMX
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve() returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  ## First, check if the inverse is chached. If so, return it
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  ## If the inverse matrix is not chached, create 
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

############# Test run results #################
##
##> x = rbind(c(3, 5), c(7, 2))
##> m<-makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    3    5
##[2,]    7    2
##> cacheSolve(m)
##[,1]       [,2]
##[1,] -0.06896552  0.1724138
##[2,]  0.24137931 -0.1034483
##> cacheSolve(m)
##getting cached inverse matrix
##[,1]       [,2]
##[1,] -0.06896552  0.1724138
##[2,]  0.24137931 -0.1034483
##> 
##
##
##
##
