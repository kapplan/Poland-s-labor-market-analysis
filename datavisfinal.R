
functionTree <- function(expr, eframe = globalenv(), maxDepth = 3, closureRecursive = FALSE, depth = 0) {
  # Helper function to determine the type description
  getTypeDescription <- function(expr) {
    if (is.symbol(expr)) {
      return("symbol")
    } else if (is.call(expr)) {
      funcName <- as.character(expr[[1]])
      if (funcName %in% c("<-", "=")) {
        return("function:special")
      } else if (funcName == "function") {
        return("function:closure")
      } else {
        return("function:call")
      }
    } else if (is.atomic(expr) && length(expr) == 1) {
      return("atom")
    } else {
      return("unknown")
    }
  }
  
  # Function to print the current expression and its type
  printExpression <- function(expr, depth) {
    indent <- paste(rep(" ", depth * 2), collapse = "")  # Create indentation based on depth
    typeDescription <- getTypeDescription(expr)
    exprDescription <- deparse(expr)
    if (length(exprDescription) > 1) {
      exprDescription <- exprDescription[1]
    }
    cat(indent, depth, " (", typeDescription, ") -> ", exprDescription, "\n", sep = "")
  }
  
  # Base case: Stop the recursion if the maximum depth is exceeded
  if (depth > maxDepth) {
    return()
  }
  
  # Print the current expression
  printExpression(expr, depth)
  
  # If the expression is a call, recursively analyze its arguments
  if (is.call(expr)) {
    lapply(expr[-1], function(sub_expr) functionTree(sub_expr, eframe, maxDepth, closureRecursive, depth + 1))
  }
}

# Define a simple function f for demonstration
f <- function(n = 0) { n ^ 2 }

# Create an expression using the function f
expr <- quote(x <- f(5))

# Print the structured tree-like view using functionTree
functionTree(expr)



