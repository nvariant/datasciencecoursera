# makeCacheMatrix() and cacheSolve() create and cache an inverse to a matrix so the inverse does 
# not have to be recalculated, which can be a time consuming operation for large matrices.



# makeCacheMatrix takes a matrix and stores it so it can be retrieved by cacheSOlve(). 
# The '<<' operator allows the matrix to persist and be used outside of the function scope.

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(mean) inv <<- solve
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## cacheSolve performs the inversion of the cached matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

mat <-  matrix(c(1,0,0,-2,1,0,0,0,1), ncol=3)
y <- makeCacheMatrix(mat)
z <- cacheSolve(y)
z
m <- makeCacheMatrix(mat)
n <- cacheSolve(m)
n
