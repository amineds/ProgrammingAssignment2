## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly.

## makeCacheMatrix creates creates a special "matrix" object 
## that can cache its inverse. 
## A list of 4 functions is retured
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x
    setinver <- function(inversion) inv_m <<- inversion
    getinver <- function() inv_m
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)
    
    
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. 
## If the inverse has already been calculated 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inv_m <- x$getinver()
    if(!is.null(inv_m)) {
        message("getting cached data")
        return(inv_m)
    }
    matrix <- x$get()
    inv_m <- solve(matrix)
    x$setinver(inv_m)
    inv_m
}

## one way of testing
## > m = cbind(c(1,3),c(2,3))
# > inv = makeCacheMatrix(m)
# > inv$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    3    3
# > inv$getinver()
# NULL
# > cacheSolve(inv)
# [,1]       [,2]
# [1,]   -1  0.6666667
# [2,]    1 -0.3333333
# > cacheSolve(inv)
# getting cached data
# [,1]       [,2]
# [1,]   -1  0.6666667
# [2,]    1 -0.3333333
# > 