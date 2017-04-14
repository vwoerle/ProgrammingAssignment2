makeCacheMatrix <- function(X = matrix()) {     # function 'makeCacheMatrix' creates special matrix 'X'
        IN <- NULL                              # inverse matrix 'IN' is assigned to 0
        set <- function(Y) {                    # function 'set' 
                X <<- Y                          # 'X' is assigned to 'Y' (argument of the function makeCacheMatrix), no matter to which value 'X' is assigned in another environment
                IN <<- NULL                      # 'IN' is assigned to 0, no matter which value it is assigned to in another environment
        }
        get <- function() X                     # function 'get': get 'X'
        setinv <- function(inv) IN <<- inv      # function 'setinv': value of the inverse matrix 'IN' is changed to 'inv', although its value was assigned to NULL in the 'set' environment
        getinv <- function() IN                 # function 'getinv': get inverse matrix 'IN'
        list(set = set, get = get,              # list is created with elements 'set', 'get', 'setinv', 'getinv'; this list is the output of function 'makeCacheMatrix' 
             setinv = setinv,
             getinv = getinv)
}


cacheSolve <- function(X, ...) {                # function 'cacheSolve' uses special matrix 'X' to calculate inverse matrix 'IN', only if it has not been calculated already
        IN <- X$getinv()                        # inverse matrix 'IN' is assigned to 'getinv' element of list
        if(!is.null(IN)) {                      # if 'IN' has been calculated already,
                message("getting cached data")
                return(IN)                      # the inverse matrix 'IN' is returned
        }
        M <- X$get()                            # else: 'IN' has not been calculated and is still 0: matrix 'M' is created; its value is assigned to the matrix 'X' from the special matrix
        IN <- solve(M, ...)            # the inverse matrix of 'M' is calculated and assigns to 'IN'
        X$setinv(IN)                    # calculated inverse matrix 'IN' is cached in list element 'setinv'
        IN                              # Inverse matrix 'IN' is the output of function 'cacheSolve'
}


##function calling: cacheSolve(makeCacheMatrix(matrix(c(1,2,3,4),2,2)))

##problem if matrix is no square matrix or if its determinant is 0 (if matrix is not invertible). Then there is no inverse matrix.
## This is ignored in this code. The matrix is assumed to be invertible.
