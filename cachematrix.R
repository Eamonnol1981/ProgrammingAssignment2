## The functions below are used to compute and cache the inverse of a matrix

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrix_inv <<- inverse
        getinverse <- function() matrix_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function returns the inverse of the matrix that was 
## created with the function above. However, it first checks to see 
## if the inverse of the matrix has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse matrix and sets this to be the value of the inverse in the cache  
## using the setinverse function.

cacheSolve <- function(x, ...) {
        matrix_inv<- x$getinverse()
        if(!is.null(matrix_inv)) {
                message("getting cached data")
                return(matrix_inv)
        }
        data <- x$get()
        matrix_inv <- solve(data, ...)
        x$setinverse(matrix_inv)
        matrix_inv
}

