# Testing Q-Q and P-P plots
graphicstest.gnlFit <- function() {
  param <- c(0, 1, 2, 3, 1)
  names(param) <- c("mu", "sigma", "alpha", "beta", "rho")

  # RUnit uses kind = "M-M", normal.kind = "K-R" for RNG. See ?RNGkind
  set.seed(2242, kind = "Marsaglia-Multicarry")
  dataVector <- rgnl(1000, param = param) 
  testgnlFit <- gnlFit(dataVector)
  
  pdf("Q-Q Plot of dataVector.pdf")
  plot.gnlFit(testgnlFit, which = 3)
  dev.off()

  pdf("P-P Plot of dataVector.pdf")
  plot.gnlFit(testgnlFit, which = 4)
  dev.off()
}
