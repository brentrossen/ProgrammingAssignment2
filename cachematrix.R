## makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(newMatrix) {
        mat <<- newMatrix
        inv <<- NULL
    }
    get <- function() mat
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Takes a special matrix object created using makeCacheMatrix and either
## computes the inverse then caches it in the object, or returns a previously
## computed inverse

cacheSolve <- function(mat, ...) {
    ## Return a matrix that is the inverse of 'mat'
    inverse <- mat$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- mat$get()
    inverse <- solve(data, ...)
    mat$setinverse(inverse)
    inverse
}
