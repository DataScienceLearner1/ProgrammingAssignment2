## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

## define the argument "x" with "matrix" as default
makeCacheMatrix <- function(x = matrix()) { 
        
        ## initialize inverse to hold value of matrix inverse  
        inverse <- NULL                    
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
       
        ## defines get function and returns value of the matrix argument
        get <- function() x
        
        ## assigns value of inverse in parent environment
        setInverse <- function(solveMatrix) inverse <<- solveMatrix
        
        ## gets the value of inverse when called
        getInverse <- function() inverse
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.
## Assumes that the input matrix is always inversible
        
        
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        #if inverse matrix is not NULL 
        if(!is.null(inverse)){
        
                #Output: Getting Cached Invertible Matrix 
                message("getting cached matrix")
                
                #return the invertible matrix
                return(inverse)
        }
        
        #get the original matrix
        matrix <- x$get()
        
        #use solve function to inverse the matrix
        inverse <- solve(matrix)
        
        #set the invertible matrix
        x$setInverse(inverse)
        
        #return the invertible matrix
        inverse      
}
