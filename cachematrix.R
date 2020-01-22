  # Create a special matrix and return its inverse
  
  # makeCacheMatrix - Creates a special matrix object
  makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      insert_matrix <- function(new_data) {
          x <<- new_data
          inv <<- NULL
      }
      get_data <- function() {
          return(as.matrix(x))
      }
      set_inv <- function(inverse) {
          inv <<- inverse
      }
      get_inv <- function() {
          return(inv)
      }
      list(
          insert_matrix = insert_matrix,
          get_data = get_data,
          set_inv = set_inv,
          get_inv = get_inv
      )
  }
  
  # cacheSolve - Returns inverse of the matrix created by makeCacheMatrix
  cacheSolve <- function(x, ...) {
      inv <- x$get_inv()
      if (!is.null(inv)) {
          message("returning inverse from cache...")
          return(inv)
      }
      data <- x$get_data()
      inv <- solve(data, ...)
      x$set_inv(inv)
      return(inv)
  }
