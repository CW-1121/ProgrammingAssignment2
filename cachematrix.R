## Together, these functions calculate the inverse of a matrix or retrieve
## the cached inverse from memory

## This function takes a matrix as input and returns a list that contains
## functions to: set or get the existing matrix, and set or get the matrice's 
## inverse
## This function returns a list that can be passed as an argument to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the inverse of a matrix 
## This matrix is stored in the list that is passed as input (a makeCacheMatrix object)
## If the inverse has already been calculated, the function retrieves it from the cache
## Otherwise, the function calculates the inverse (assuming matrix is invertible)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
        
}
