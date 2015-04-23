## The functions here take an invertable matrix and caches it for 
## future use. To test, load this file into memory and run test()

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## x is a square invertible matrix
    ## returns a list containing functions to
    ##  1. set the matrix
    ##  2. get the matrix
    ##  3. set the inverse
    ##  4. get the inverse
    ## this list is used as the input to cacheSolve()
    
    inv <- NULL
    set <- function(y) {
        ## use `<<-` to assign a value to an object in an environment 
        ## different from the current environment. 
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse already exists and has not changed, the function retrieves the inverse from the cache.
##  
cacheSolve <- function(x, ...) {
    ## x is the output of makeCacheMatrix()
    ## returns inverse of the original matrix passed to makeCacheMatrix()
    inv = x$getinv()
    ## if the inverse has already been calculated
    if (!is.null(inv)){
        ## get it from the cache and skips the computation using solve(). 
        message("using cached matrix")
        ## exit the function here returning the already cached inversed matrix
        return(inv)  
    }
    
    ## if inverse does not exists, create it 
    m = x$get()
    inv <- solve(m, ...)
    
    ## sets the value of the inverse in the cache via the setinv function.
    x$setinv(inv)
    message("creating new cached matrix")
    inv
}


#####
# Extra function to run everything. At the prompt type:
# >test()
# or
# >test(y) # where y is an existing invertable matrix 
#####
test = function(y = matrix()){
    ## create a matrix if we don't already have one
    if(length(y) > 1){
        message("Good matrix exists, so we'll use it")
    } else{
        message("Matrix does not exist so lets create one")
        y <<- matrix(rnorm(10000), nrow=100, ncol=100)  #create a matrix first
    }
    ##  create cachable/invertible matrix
    cmatrix = makeCacheMatrix(y)
    
    ## Create a cached inverse of cmatrix, and measure how long it takes
    start.time = Sys.time()         ## set start time 
    cacheSolve(cmatrix)             ## inverse cmatrix
    dur = Sys.time() - start.time   ## subtract start time from the end time
    first <- dur
    print(dur)                      ## print out the time it took to run
    
    ## Do it all again to see if the processing time improves when retreived from the cache
    start.time = Sys.time()
    cacheSolve(cmatrix)
    dur = Sys.time() - start.time
    second <- dur
    print(dur)
    message("Performance improved by:")
    improvement <- (first - second)
    improvement
}

# To test makeCacheMatrix and cacheSolve pass an existing matrix
# to the test(y) function, or leave test() empty to go with the default

# then run everything
# > test()  # run the test function, pass in optional matrix
