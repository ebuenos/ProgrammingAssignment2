## This code implements two functions makeCacheMatrix and cacheSolve
## makeCacheMatrix is a function to cache the inverse matrix so we do not need to calculate
## it everytime we call the function without changing the matrix
## cacheSolve first check if the inverse was already calculated and if it was then return the
## inverse matrix, if not it will calculate the inverse and return its value and cache the
## inverse matrix to avoid extra calculations in the future

## This function receives a given matrix and cache the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        #Initialize the inverse matrix as a NULL matrix.
        m_inverse <-NULL
        #Set the value of the matrix and clear the inverse matrix in case it has changed.
        set <- function(y){
                x <<- y
                m_inverse <<- NULL
        }
        #Get the value of the matrix
        get <- function() x
        #Set the inverse_matrix
        setinverse <- function(solve) m_inverse <<- solve
        #Get the value of the inverse matrix
        getinverse <- function() m_inverse
        #Return the values of the function which will be used for another function
        #to check if the inverse was calculated before.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## First check if the inverse matrix are cached and return the inverse of the given matrix
## if not cached will calculated the inverse, cache and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #Gets the value of the inverse matrix
        m_inverse <- x$getinverse()
        #Check if the inverse matrix was already solved, if it was
        #prints a message that is getting data from cache and return its value
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }
        #Get the matrix to be inversed
        data <- x$get()
        #Calculate the inverse of the matrix
        m_inverse <- solve(data, ...)
        #Set the value of the inverse matrix to the setinverse variable
        x$setinverse(m_inverse)
        #Return the inverse matrix
        m_inverse
}

