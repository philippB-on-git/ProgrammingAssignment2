## makeCacheMatrix:
##      initializes an object in memory that stores a matrix as well as its inverted matrix
##      the object provides the methods
##          get()       : returns the matrix stored in "x"
##          set(x)      : sets a new matrix x and cleares the inverted matrix (inv <- NULL)
##          getinv()    : returns the matrix stored in "inv" 
##                          (or NULL if inverted matrix not yet set)
##          setinv(inv) : sets the inverted matrix "inv"
## cacheSolve:
##      checks if the inverse matrix ("inv") of an object created by "makeCacheMatrix" is set
##          if so, the inverse matrix is returned
##          else, the matrix is inverted and stored in the object as "inv", inv is then returned


## makeCacheMatrix
## creates object comprising a matrix x provided via the function argument as well as a list of 
## methods to access both the matrix x and inv, which is used to store its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverted) inv <<- inverted
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve
## checks if "inv" of an object created via "makeCacheMatrix" is still not set
##      if so, the inverse matrix of the matrix stored in the object is calculated and 
##          stored in "inv", inv is then returned
##      otherwise "inv" (stored in the object) is returned 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # check if inv is still NULL
    if (is.null(x$getinv())) {
        message("inverting matrix...")
        mat <- x$get()
        mat_inv <- solve(mat)
        x$setinv(mat_inv)
        return(mat_inv)
    } 
    
    message("getting inverted matrix...")
    x$getinv()
}
