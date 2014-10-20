## makeCacheMatrix creates a special vector, which is really contains functions:
## 1. set the value of the matrix ($set)
## 2. get the value of the matrix ($get)
## 3. set the inverse value of the matrix ($setsolve)
## 4. get the inverse value of the matrix ($getsolve)
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function (y) {
        s <<- NULL
        x <<- y
    }
    get <- function () x
    setsolve <- function (solve) {
        s <<- solve
    }
    getsolve <- function () s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve calculates the inverse matrix for x.
## At first it checks to see if cache value has been caclulated. If so, 
## it gets the cache value and skip the rest caclulation
cacheSolve <- function(x, ...) {
    cache =  x$getsolve()
    if (!is.null(cache)) {
        message("cache hit")
        return(cache)
    }
    inverse = solve(x$get())
    x$setsolve(inverse)
    inverse
}
