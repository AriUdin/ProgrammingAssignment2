
## Short comment of makeCacheMatrix() function :
## In the beginning the input data x should be written, but if it is not written the default value will be NULL matrix.
## Before it cached, the value of inverse.matrix and get.inverse will be NULL.
## the set.matrix function is applied whenever new matrix is observed,
## while the set.inverse is used to input the new matrix inverse after determined by cacheSolve function.
## The result of this function is data list.


makeCacheMatrix <- function(x = matrix()) {                      
    inverse.matrix <- NULL                                       ## Set Null as the initial value
    set.matrix <- function(y) {                                  ## Set the new matrix to the function
        x <<- y
        inverse.matrix <<- NULL
    }
    get.matrix <- function() x                                   ## Call the matrix x
    set.inverse <- function(inverse) inverse.matrix <<- inverse  ## Set the inverse matrix
    get.inverse <- function() inverse.matrix                     ## Call the inverse matrix
    list(set.matrix = set.matrix, get.matrix = get.matrix,       ## Create list 
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Short comment of cacheSolve() function :
## In the beginning the input data x is the variable that consisted of the result from makeCacheMatrix() function
## Initially it check the value of the inverse matrix, if it is exist it will print the value and close the function.
## If it is NULL, the calculation of the inverse will be conducted, by sole() function.
## After the inverse matrix is obtained, it will rewrite/re-set the inverse matrix in the variable x.
## Lastly, it will prints the inverse matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse.matrix <- x$get.inverse()                            ## Set the inverse value based on  previous list 
    if(!is.null(inverse.matrix)) {                               ## If the value of inverse.matrix is exist, it will stop the function, otherwise continue
        message("getting cached data")                              
        return(inverse.matrix)
    }
    input.matrix <- x$get.matrix()                               ## It will extract the matrix from the argument x
    inverse.matrix <- solve(input.matrix, ...)                   ## It creates the inverse of matrix 
    x$set.inverse(inverse.matrix)                                ## It is setting the new value of set.inverse of data list x 
    inverse.matrix                                               ## It prints the result of inverse matrix
}
