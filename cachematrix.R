## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
#       set the value of the MATRIX
#       get the value of the MATRIX
#       set the INVERSE of the MATRIX
#       get the INVERSE of the MATRIX
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(s){
                x <<- s
                inver <<- NULL 
        }
        get <- function() x
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function() inver
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special
#       "matrix" returned by makeCacheMatrix above. 
#       If the inverse has already been calculated 
#       (and the matrix has not changed), then 
#       cacheSolve should retrieve the inverse from 
#       the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        if (!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        matric <- x$get()
        inver <- solve(matric, ...)
        x$setInverse(inver)
        inver
}
