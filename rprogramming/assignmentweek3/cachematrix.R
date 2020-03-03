## This function is used to calculate the inverse matrix and store the inversed
## matrix for later use

## This part of function creates a new kind of special "matrix" where we can
## 1. set the matrix
## 2. get the matrix
## 3. set the inversed matrix
## 4. get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y){
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inver <<- solve
    getinverse <- function() inver
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## This function is used to first check if there is an inversed matrix stored
## If yes, pull the data
## If not, caculate the inversed matrix and store it

cacheSolve <- function(x, ...) {
    inver <- x$getinverse()
    if(!is.null(inver)) {
        message("Retreiving cached data")
        return(inver)
    }
    matr <- x$get()
    inver <- solve(matr, ...)
    x$setinverse(inver)
    inver
        ## Return a matrix that is the inverse of 'x'
}
