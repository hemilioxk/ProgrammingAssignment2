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
                inv <<- NULL # inv is restarted when the function is called again
        }
        
        get <- function() x
        
        set_inverse <- function(inverse_matrix) inv <<- inverse_matrix
        
        get_inverse <- function() inv 
        
        list(set = set, get = get, 
             set_inverse = set_inverse, 
             get_inverse = get_inverse) 
        
        # Must make a list to access the functions. 
        # Must name the function so you can call them with $

}


## Computes the inverse matrix of makeCacheMatrix() object and caches its inverse
## or recover it from cache if it was previously calculated.

cacheSolve <- function(x, ...) {
        
        inv <- x$get_inverse()
        
        if (!is.null(inv)) {
                message("Getting cached inverse matrix")
                
                return(inv)
        }
        
        inv <- solve(x, ...)
        
        x$set_inverse(x) #cache the inverse matrix
        
        inv
        ## Return a matrix that is the inverse of 'x'
}

###Tests:

invertible_matrix <- matrix(c(2, 2, 3, 2), 2, 2)

invertible_matrix

solve(invertible_matrix)

my_matrix_1 <- makeCacheMatrix(invertible_matrix)

my_matrix_1$get()

my_matrix_1$get_inverse()

my_matrix_1$set(1)

my_matrix_1$get()

my_matrix_1$set(invertible_matrix)

my_matrix_1$get()

cacheSolve(my_matrix_1)
