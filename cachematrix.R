## "CacheMatrix" is pair of functions that compute and
## cache the inverse of a matrix

## This function "makeCacheMatrix" creates a special "matrix" object 
## that can cache its inverse.

## solve() is function that compute inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invMat <<- solve
        getinverse <- function() invMat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function "cacheSolve" computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above

## if  !is.null(invMat) = TRUE ( inverse has already 
## been calculated), inverse of matrix will be returned
## else, it will be computed

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        invMat <- x$getinverse()                
        if(!is.null(invMat)) {
                message("getting cached inverse")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data, ...)
        x$setinverse(invMat)
        invMat        
}
