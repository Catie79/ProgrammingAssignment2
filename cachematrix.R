## These functions create an invertible matrix and then solve for
## the inverse. The solutions is cached for future use, saving
## computational time. It checks for changes in the matrix and,
## if the matrix has not changed, returns the cached solution. If
## the matrix has changed or the inverse has not been found before,
## it solves for the inverse and returns the value as well as caching
## it.
## Creates an invertible matrix for use in the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
## Create cache matrix
m <- NULL
set <- function(y) {
x <<- y
m <<- NULL
}
## Set up holding spot for inverse of matrix
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)
}
## Function to solve a matrix for it's inverse and return that matrix
cacheSolve <- function(x, ...) {
#check to see if the inverse has already been cached
m <- x$getinverse()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
# If the solution does not already exist, solve for the inverse
data <- x$get()
m <- solve(x)
# Cache the inverse for future use
x$setinverse(m)
# Return the inverse
m
}
