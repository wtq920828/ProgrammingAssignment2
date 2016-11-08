## Like the function in the example, my function includes two functions, the first
## function called "makeCasheMatrix" and the second function called "cashSolve",
## which is used to get the inversion of the matrix by using the son-function in
## "makeCashMatrix"

## The first function, makeVector creates a special "vector", which is really a list containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix
makeCasheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() x
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## The following function calculates the inverse of the special "matrix" created with the 
## the above function. However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
    cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)) {
            message("Caching the Inverse of a Matrix")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    }
}

 # MatrixInverse
# MatrixInverse
