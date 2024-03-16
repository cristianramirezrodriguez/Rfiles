# Define the delta y function
deltay <- function(y1, y2) {
  yf <- as.numeric(y2)
  yi <- as.numeric(y1)
  return(yf - yi)
}

# Define the delta x function
deltax <- function(x1, x2) {
  xf <- as.numeric(x2)
  xi <- as.numeric(x1)
  return(xf - xi)
}

# Define the find slope function
findm <- function(x1, y1, x2, y2) {
  m <- deltay(y1, y2) / deltax(x1, x2)
  return(m)
}

# Define the point-slope equation function
pointslopeequation <- function(x1, y1, x2, y2) {
  m <- findm(x1, y1, x2, y2)
  return(paste0("y - ", y1, " = ", m, "(x - ", x1, ")"))
}

# Define the find intercept function
findb <- function(x1, y1, m) {
  b <- y1 - m * x1
  return(b)
}

# Define the slope-intercept equation function
slopeinterceptequation <- function(x1, y1, x2, y2) {
  m <- findm(x1, y1, x2, y2)
  b <- findb(x1, y1, m)
  return(paste0("y = ", m, "x + ", b))
}

# Define the standard form equation function
standardformequation <- function(x1, y1, x2, y2) {
  A <- -deltay(y1, y2)
  B <- deltax(x1, x2)
  C <- B * y1 + A * x1
  return(paste0(A, "x + ", B, "y = ", C))
}

# Define the plotline function
plotline <- function(x1, y1, x2, y2) {
  x <- seq(-10, 10, length.out = 100)
  m <- findm(x1, y1, x2, y2)
  b <- findb(x1, y1, m)
  y <- m * x + b
  
  plot(x, y, type = "l", col = "blue", main = "y vs x", xlab = "x", ylab = "y")
  points(c(x1, x2), c(y1, y2), col = "red", pch = 16)
  legend("topleft", legend = c("Line", "Given Points"), col = c("blue", "red"), pch = c(NA, 16))
  grid()
}

# Example use
x1 <- 1
y1 <- 2
x2 <- 3
y2 <- 4

plotline(x1, y1, x2, y2)
