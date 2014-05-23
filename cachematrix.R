## The two functions demonstrate the utilization of the <<- super assignment
## operator and lexical scoping. makeCacheMatrix() manages the storage and
## retrieval of a inverse matrix. cacheSolve() computes the inverse of a
## matrix or retrieves a cache of that inverse matrix from makeCacheMatrix().

## Creates and stores an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #Cache
        
        # Methods/Subfunctions
        set <- function(y) {
                x <<- y #Super assigns inputed vector to x
                m <<- NULL #Clears cache of super assigned cache m
        }
        get <- function() x #Returns value of x
        savecache <- function(solve) m <<- solve #Saves the cache
        getcache <- function() m #Retrieves the cache
        
        # Sets values
        list(
                 set = set
                ,get = get
                ,savecache = savecache
                ,getcache = getcache
        )
}


## Calculates an inverse matrix or retrieves it's pre-calculated cache from
## makeCacheMatrix()

cacheSolve <- function(x, ...) {
        # Returns cache from makeCacheMatrix() if present
        m <- x$getcache() #Retrieves cache as m
        
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        
        # Returns inversed matrix if cache not present
        data <- x$get()
        m <- solve(data, ...)
        x$savecache(m) #Saves inversed matrix to cache
        m
}