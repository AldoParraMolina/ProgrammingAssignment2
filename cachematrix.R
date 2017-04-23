# The next pair of functions creates and cumputes the inverse matrix of a square matrix. This functions are designed to
# don't misspend resourses and time on computing an operation on the same object many times.

# makeCacheMatrix -------------------------------------------------------------------------------------------------

# This function creates a special "matrix" object that can cache its inverse. Note that the argument x is validated to
# ensure that is a square matrix, allowing to compute it their inverse. If not, the function ends 
# asking for a square matrix

makeCacheMatrix <- function(x = matrix()) {
    
    if(dim(x)[1] != dim(x)[2]){
        massage <- "Insert a square matrix"
        return(massage)
    }
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}



# cacheSolve ------------------------------------------------------------------------------------------------------

# This function use a special "matrix" object created with makeCacheMatrix. cacheSolve uses a list of functions in
# makeCacheMatrix to compute the inverse matrix, ensuring to don't repeat a process if it is not neccesary when the 
# function is called in the same matrix twice.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

# Example ---------------------------------------------------------------------------------------------------------

# To ilustrate the functionality of this functions you can run the next code

my_matrix <- makeCacheMatrix(matrix(rnorm(810000),900,900))

cacheSolve(my_matrix)
