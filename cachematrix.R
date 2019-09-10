## makeCacheMatrix initializes x and s. It also defines multiple functions to be used by 
## cacheSolves and stores all this in a list.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve uses makeCacheMatrix for inputs and the functions defined to compute the 
## inverse of the matrix supplied. First, it tries to call s from a function supplied 
## through makeCacheMatrix. If it is not null, then we retrieve s from memory. If it 
## is null, then we move down to calculate s through solve() and then use setinverse() 
## through makeCacheMatrix to cache s for quick use later.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
