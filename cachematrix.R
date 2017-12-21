# Cache Inverse Matrix

## makeCacheMatrix() Creates a special object of the type "makeCacheMatrix()" 
## to work with a matrix (input), providing some mutator and accessor methods.
## cacheSolve() solves a matrix and cache its inverse matrix, when called for
## matrix previously solved, return the inverse matrix from the cache.

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(z) {
                
                x <<- z
                inv <<- NULL
        }
        
        get <- function() x
        
        set_inverse <- function(inverse_matrix) inv <<- inverse_matrix
        
        get_inverse <- function() inv 
        
        list(set = set, get = get, 
             set_inverse = set_inverse, 
             get_inverse = get_inverse) 
}


## Computes the inverse matrix of makeCacheMatrix() object and caches its inverse
## or recover it from cache if it was previously calculated.

cacheSolve <- function(x, ...) {
        
        inv <- x$get_inverse()
        
        if (!is.null(inv)) {
                message("Getting cached inverse matrix")
                
                return(inv)
        }
        
        matrix_to_solve <- x$get()
        
        inv <- solve(matrix_to_solve, ...)
        
        x$set_inverse(inv)
        
        inv
}