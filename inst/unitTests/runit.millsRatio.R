# Testing millsR
test.millsR <- function() {
  testmillsR <- sapply(1:5, millsR)
  values <- c(0.65568, 0.42137, 0.30459, 0.23665, 0.19281)
  tol <- 0.001
  checkEquals(testmillsR, values, tol = tol)
  
  # Checking that e^log(millsR) = millsR.
  checkEquals(sapply(1:5, millsR), exp(sapply(1:5, millsR, log = TRUE)))
}
