##makeCacheMatrix and cacheSolve functions are used for calculating saving and 
##retriving matrix and its associated inverse

## makeCacheMatrix functions are performing four operations:
##  set the value of the matrix
##  get the value of the matrix
##  set the inverse of the matrix
##  get the inverset of the matrix

##  This function assume that the inverse of the matrix always exist, 
##  thus no error handling here
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m<<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set= set, get=get, setinverse=setinverse, getinverse=getinverse)

}



## cacheSolve will first check whether the inverse of the matrix
## exists in the cache.  If it does, then retrieve from the cache,
## otherwise the solve function is called, the result is save in the cache 
## as well as returning to calling function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
