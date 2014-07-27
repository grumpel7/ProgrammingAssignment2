## The following two functions allow the creation of a special object that stores an 
## invertible matrix and caches its inverse matrix in order to allow a quicker
## exection
## Assuming that the matrix invertible means that the matrix has the same row and column
## dimension (that is, the matrix is square). In this assignment, we are not testing that the
## row and column has the same dimension.
## To run this program, first we need to create an object that is a square matrix.
## Example: a <- matrix(c(0,2,1,4),2,2) which is a 2x2 matrix.
## Next, we can create a cache version of a matrix. for instance, b <- makeCacheMatrix(a).
## To compute the inverse matrix, call cacheSolve for the matrix, for instance cacheSolve(b).
## Now you can call the matrix or inverse of the matrix as needed. For instance, b$get() or b$getinverse().\
## Please note that if you use set() or setmean(), it will reset (corrupt?) the values.

## makeCacheMatrix: takes a square matrix and allows getting the matrix or its inverse (if already computed).
## also allows resetting of matrix element values with set and setinverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## cacheSolve: returns a matrix that is the inverse of matrix x and saves it

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
