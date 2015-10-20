# Functions that will calculate and cache the inverse of a matrix
#Usage: 

# function makeCacheMatrix 
# Creates a special matrix that ia a list containing 4 functions:
# 'set': sets the value of the matrix
# 'get': gets the value of the marix
# 'setinverse': sets the value of the inverse matrix
# 'getinverse': gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    # Define setter for matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # Returns matrix
    get <- function() x
    
    # Sets inverse once calculated
    setinverse <- function(inverse) i <<- inverse
    
    # Returns calculated inverse, else null
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# function cacheSolve
# Calculates the inverse of the matrix created in makeCacheMatrix
# Reuses calculated value if already set.
cacheSolve <- function(x, ...) {
    # Try to get inverse, null if not calculated
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # Retrieve matrix
    data <- x$get()
    
    # Perform ivnerse
    i <- solve(data, ...)
    
    # Set inverse
    x$setinverse(i)
    
    # Return newly calculated inverse
    i
}

#Tests:
#> source ('cachematrix.R')
#> m <- makeCacheMatrix(matrix(c(6,4,4,6)), c(2,2)))
#> m <- makeCacheMatrix(matrix(c(6,4,4,6), c(2,2)))
#> cacheSolve(m)
#[,1] [,2]
#[1,]  0.3 -0.2
#[2,] -0.2  0.3
#> cacheSolve(m)
#getting cached data
#[,1] [,2]
#[1,]  0.3 -0.2
#[2,] -0.2  0.3

