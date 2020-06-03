
## Defining two functions that cache the inverse of a matrix
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

## This function creates and fetches the user provided matrix and its inverse(computed in the 'cacheSolve' function)

makeCacheMatrix <- function(x = matrix()) {
  
                ## Initializing variable for inverse calculation
                inv <- NULL
                
                ## Initializing the matrix
                set <-function(matrix){
                  m <<- matrix
                  inv <<- NULL
                }
                
                ## Fetching the matrix
                get <- function(){
                  m ## Returning the matrix
                }
                
                ## Initializing the method for inverse of matrix
                setInverse <- function(inverse){
                  inv <<- inverse
                }
                
                ## Fetching the inverse of the matrix
                getInverse <- function(){
                  inv ## Returning the inverse of the matrix
                }
                
                ## Returning the list of all functions
                list(set = set, get = get, setInverse = setInverse,
                     getInverse = getInverse)
}


## Computes the inverse of the matrix returned by the 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  
           ## Return a matrix that is the inverse of 'x'
           m <- x$getInverse()
           
           ## First check to see if the inverse has already been calculated. 
           ## If so, it gets the inverse from the cache and skips the computation. 
           ## Otherwise, it calculates the inverse of the data and sets the value of 
           ## the inverse in the cache via the setInverse function.
           if(!is.null(m)){
             message("Getting cached data")
             return(m)
           }
           
           ## Fetching the matrix from the defined object
           m_data <- x$get()
           
           ## Calculating the inverse using solve() function
           m <- solve(m_data)
           
           ## Set the inverse to the defined object
           x$setInverse(m)
           
           retrun(m) ## Returning the final matrix
}