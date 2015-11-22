##Note: My function stores the matrix under the name "mat1" instead of "x". 

makeCacheMatrix <- function(mat1=matrix()) { ## With the input you give makeCacheMatrix creates a matrix
    m <- NULL                                ## and you store it under a variable, e.g. like test<-makeCacheMatrix(matrix(...))
    set <- function(y) {                     
        mat1 <<- y                           ## The rest what this function does, is to store 4 functions as a list,
        m <<- NULL                           ## namely set, get, setinverse and getinverse. No calculation is performed.
    }
    get <- function() mat1
    setinverse <- function(inverse) m <<- inverse 
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the matrix created with makeCacheMatrix. 
## Furthermore, it uses the functions that are stored in makeCacheMatrix.

cacheSolve <- function(mat1,...) {           ## You need to run this function on the variable you named before, e.g. test (see above)
    m <- mat1$getinverse()                   ## Here it gets the inverse of the matrix that is calculated in this function.
    if(!is.null(m)) {                        ## If there is an inverse because it has been calculated already, this inverse
        message("getting cached data")       ## simply gets retrieved and it says "getting cached data" and there is no need to calculate it again.
        return(m)
    }
    data <- mat1$get()                      
    m <- solve(data, ...)                    ## If the inverse has not yet been calculated, here it gets calculated (with solve()).
    mat1$setinverse(m)
    m                                        ## Return a matrix that is the inverse of 'mat1'
}

