## Create parameter sets for the Normal Laplace Distribution

## nlLargeParam
mus <- c(-5, -2, -1, 0, 1, 2, 5)
sigmas <- c(1, 2, 3, 5, 7, 8, 10)
alphas <- c(0.5, 1, 1.5, 2, 3, 4, 5, 7, 10)
betas <- c(0.5, 1, 1.5, 2, 3, 4, 5, 7, 10)
totalrows <- length(mus) * length(sigmas) * length(alphas) * length(betas)
nlLargeParam <- matrix(nrow = totalrows, ncol = 4)
rownum <- 1

for (i in 1:length(mus)) {
  for (j in 1:length(sigmas)) {
    for (k in 1:length(alphas)) {
      for (l in 1:length(betas)) {
        param <- c(mus[i], sigmas[j], alphas[k], betas[l])
        nlLargeParam[rownum, ] <- param
        rownum <- rownum + 1
      }
    }
  }
}


## nlSmallParam
mus <- c(0, 1)
sigmas <- c(1, 5)
alphas <- c(0.5, 1.5, 4)
betas <- c(0.5, 1.5, 4)
totalrows <- length(mus) * length(sigmas) * length(alphas) * length(betas)
nlSmallParam <- matrix(nrow = totalrows, ncol = 4)
rownum <- 1

for (i in 1:length(mus)) {
  for (j in 1:length(sigmas)) {
    for (k in 1:length(alphas)) {
      for (l in 1:length(betas)) {
        param <- c(mus[i], sigmas[j], alphas[k], betas[l])
        nlSmallParam[rownum, ] <- param
        rownum <- rownum + 1
      }
    }
  }
}


## nlLargeShape
mus <- 0
sigmas <- c(1, 2, 3, 5, 7, 8, 10)
alphas <- c(0.5, 1, 1.5, 2, 3, 4, 5, 7, 10)
betas <- c(0.5, 1, 1.5, 2, 3, 4, 5, 7, 10)
totalrows <- length(mus) * length(sigmas) * length(alphas) * length(betas)
nlLargeShape <- matrix(nrow = totalrows, ncol = 4)
rownum <- 1

for (i in 1:length(mus)) {
  for (j in 1:length(sigmas)) {
    for (k in 1:length(alphas)) {
      for (l in 1:length(betas)) {
        param <- c(mus[i], sigmas[j], alphas[k], betas[l])
        nlLargeShape[rownum, ] <- param
        rownum <- rownum + 1
      }
    }
  }
}


## nlSmallShape
mus <- 0
sigmas <- c(1, 5)
alphas <- c(0.5, 1.5, 4)
betas <- c(0.5, 1.5, 4)
totalrows <- length(mus) * length(sigmas) * length(alphas) * length(betas)
nlSmallShape <- matrix(nrow = totalrows, ncol = 4)
rownum <- 1

for (i in 1:length(mus)) {
  for (j in 1:length(sigmas)) {
    for (k in 1:length(alphas)) {
      for (l in 1:length(betas)) {
        param <- c(mus[i], sigmas[j], alphas[k], betas[l])
        nlSmallShape[rownum, ] <- param
        rownum <- rownum + 1
      }
    }
  }
}
