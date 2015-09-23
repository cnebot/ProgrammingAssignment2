# To avoid  costly computation for Matrix inversion there may be 
# some benefit from caching the inverse of a matrix rather than compute 
# everytime.  
# The following functions are used to cache the inverse of a matrix.

# From x, a square invertible matrix, makeCacheMatrix creates 
# a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
## This list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        
        #  `<<-` allows us to assign a value to an object in a 
        # different environment from the current one. 
        
        x <<- y
        inv <<- NULL
    }
    
    get = function() x
    
    setinv = function(inverse) inv <<- inverse 
    
    getinv = function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


# The following function returns the inverse of the matrix. First, it checks if
# the inverse has already been computed. If so, it gets the result from cache 
# and skips the computation. If not, it computes the inverse, sets the value 
# in the cache with setinverse function.



cacheSolve <- function(x, ...) {
    
    # x comes from  makeCacheMatrix()
    # and give us the inverse of the original matrix
    # input to makeCacheMatrix()
    
    inv = x$getinv()
    
    # In case the inverse has already been calculated
    # get it from the cache and skips the computation.
    
    if (!is.null(inv)){
       
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, calculates the inverse 
    # sets the value of the inverse in the cache via the setinv function.
    
    data = x$get()
    inv = solve(data, ...)
    
    x$setinv(inv)
    
    return(inv)
}

# This is a proof
# x = rbind(c(3, 2), c(-1, 1))
# ma=makeCacheMatrix(x)
# ma$get()
#    [,1] [,2]
#    [1,]    3    2
#    [2,]   -1    1
# cacheSolve(ma)
#    [,1] [,2]
#    [1,]  0.2 -0.4
#    [2,]  0.2  0.6
# cacheSolve(ma)
#  getting cached data
#    [,1] [,2]
#    [1,]  0.2 -0.4
#    [2,]  0.2  0.6
#