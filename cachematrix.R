## This file was created by Daniel Bondeson on 27 September 2018 for the 
## Programming in R: Programming Assignment 2: Lexical Scoping. 


## The goal of this exercise was to generate two functions which receive a matrix, 
## check to see if that matrix has been solved before, and then either return 
## the previous solution, or compute a new solution and cache it. 

## makeCacheMatrix receives a passed matrix and creates a special matrix "spec" 
## which the function cacheSolve can read and use to cache the inverse of the 
## passed matrix. 
makeCacheMatrix <- function(x = matrix()) {
        
        #inv is the cached variable containing the final answer
        inv <<- NULL
        
        setdata <- function (y) {
                x <<- y
                inv <<- NULL
        }
        
        getdata <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        #These functions are stored in a 2x2 matrix with addressable coordinates. 
        mat_names <- list(c("Data","Inverse"),c("Set","Get"))
        
        matrix(c(setdata, getdata, setinv, getinv),
               nrow = 2,
               ncol = 2,
               dimnames = mat_names,
               byrow = T)
}


## The cacheSolve function is passed the special vector generated in 
## makeCacheMatrix. It first checks whether the special matrix has already been 
## evaluated, and if so, simply returns the inverse. If not, it retrieves, solves, 
## and stores the inverse of the original matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the original 'x'
        
        i <- x[["Inverse","Get"]]()
       
        #check to see if i has already been solved
        if (!is.null(i)) {
                message ("Getting cached answer")
                return(i)
        }
        
        #retrieve the original matrix and solve it
        data <- x[["Data","Get"]]()
        
        i <- solve(data)
        
        i <<- x[["Inverse","Set"]](data)
        i
}
