## The makeCacheMatrix function can store a matrix  and its inverse using 
## set and setinverse "subfunctions". Then we can call a matrix or its inverse,
## if stored in the makeCacheMatrix function, with get and getinverse
## "subfunctions".
## Finally, the cacheSolve function check if there is already
## an inverse matrix stored in makeCacheMatrix function ; if there isn't, they
## create the inverse matrix and store it in the makeCacheMatrix function.

## Calling makeCacheMatrix for a matrix store it in the function. We also
## can store the inverse of the matrix obtained by Solve() function with
## makeCacheMatrix(x)$setinverse("inverse of the matrix").

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                im <<- inverse
        }
        getinverse <- function() {
                im
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Check if the inverse of the matrix is already stored in makeCacheMatrix
## function. In that case, the message "getting cached data" and the inverse
## matrix appear. If there is no inverse matrix stored, cacheSolve create
## the inverse matrix and store it in makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        im <- x$getinverse()
        im
}
