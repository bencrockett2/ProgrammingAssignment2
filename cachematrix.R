## These functions create and utilize a list of functions used in verifying
## existence of an inverse in the parent environment and calculating that 
## inverse if it is not found

## This function creates the list of functions used to set and calculate the
## inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #initialize inverse as NULL w/in execution environment
    
    ## next four lines used to manually reset matrix if desired
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x # used to return matrix in main function
    setinv <- function(inv) i <<- inv # stores inverse matrix in parent env
    getinv <- function() i # returns stored inverse matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)    
}


## This function searches environment for matrix inverse and 
## calculates it if it does not exist

cacheSolve <- function(x, ...) {
    ptm <- proc.time() # start timer for function
    i <- x$getinv() # get stored inverse value from pvs func if it exists
    ## next 4 lines return inverse if it exists
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## next lines are default if inverse doesn't exist in environment
    data <- x$get() # pulls actual matrix from pvs func
    i <- solve(data, ...) # calculates inverse of matrix
    x$setinv(i) # sets matrix inverse 
    i # returns inverse
}
