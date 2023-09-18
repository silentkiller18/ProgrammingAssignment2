# Set the working directory
setwd('C:/Users/rubind1/Documents/Coursera-R')

# Define a function to create a cacheable matrix
makeCacheMatrix <- function(data = matrix(sample(1:100, 9), 3, 3)) {
  cache <- list(data = data, solve = NULL)

  set <- function(x) {
    cache$data <- x
    cache$solve <- NULL
  }

  get <- function() {
    cache$data
  }

  setsolve <- function(solve) {
    cache$solve <- solve
  }

  getsolve <- function() {
    cache$solve
  }

  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# Define a function to compute and cache the inverse of a matrix
cacheSolve <- function(x, ...) {
  cached_solve <- x$getsolve()
  if (!is.null(cached_solve)) {
    message("Getting cached inverse matrix")
    return(cached_solve)
  }

  data <- x$get()
  inverted_matrix <- solve(data, ...)

  x$setsolve(inverted_matrix)
  inverted_matrix
}
