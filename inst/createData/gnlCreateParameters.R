## Create parameter sets for the Generalised Normal Laplace Distribution

## gnlLargeParam
mus <- c(-5, -2, -1, 0, 1, 2, 5)
sigmas <- c(1, 2, 3, 5, 7, 8, 10)
alphas <- c(0.5, 1, 1.5, 2, 3, 4, 5, 7, 10)
betas <- c(0.5, 1, 1.5, 2, 3, 4, 5, 7, 10)
rhos <- c(0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4)
totalrows <- length(mus) * length(sigmas) * length(alphas) * length(betas) * length(rhos)
gnlLargeParam <- matrix(nrow = totalrows, ncol = 5)
rownum <- 1

for (i in 1:length(mus)) {
  for (j in 1:length(sigmas)) {
    for (k in 1:length(alphas)) {
      for (l in 1:length(betas)) {
        for (m in 1:length(rhos)) {
          param <- c(mus[i], sigmas[j], alphas[k], betas[l], rhos[m])
          gnlLargeParam[rownum, ] <- param
          rownum <- rownum + 1
        }
      }
    }
  }
}


## gnlSmallParam
mus <- c(0, 1)
sigmas <- c(1, 5)
alphas <- c(0.5, 1.5, 4)
betas <- c(0.5, 1.5, 4)
rhos <- c(0.5, 1, 2)
totalrows <- length(mus) * length(sigmas) * length(alphas) * length(betas) * length(rhos)
gnlSmallParam <- matrix(nrow = totalrows, ncol = 5)
rownum <- 1

for (i in 1:length(mus)) {
  for (j in 1:length(sigmas)) {
    for (k in 1:length(alphas)) {
      for (l in 1:length(betas)) {
        for (m in 1:length(rhos)) {
          param <- c(mus[i], sigmas[j], alphas[k], betas[l], rhos[m])
          gnlSmallParam[rownum, ] <- param
          rownum <- rownum + 1
        }
      }
    }
  }
}


## gnlLargeShape
mus <- 0
sigmas <- c(1, 2, 3, 5, 7, 8, 10)
alphas <- c(0.5, 1, 1.5, 2, 3, 4, 5, 7, 10)
betas <- c(0.5, 1, 1.5, 2, 3, 4, 5, 7, 10)
rhos <- c(0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4)
totalrows <- length(mus) * length(sigmas) * length(alphas) * length(betas) * length(rhos)
gnlLargeShape <- matrix(nrow = totalrows, ncol = 5)
rownum <- 1

for (i in 1:length(mus)) {
  for (j in 1:length(sigmas)) {
    for (k in 1:length(alphas)) {
      for (l in 1:length(betas)) {
        for (m in 1:length(rhos)) {
          param <- c(mus[i], sigmas[j], alphas[k], betas[l], rhos[m])
          gnlLargeShape[rownum, ] <- param
          rownum <- rownum + 1
        }
      }
    }
  }
}


## gnlSmallShape
mus <- 0
sigmas <- c(1, 5)
alphas <- c(0.5, 1.5, 4)
betas <- c(0.5, 1.5, 4)
rhos <- c(0.5, 1, 2)
totalrows <- length(mus) * length(sigmas) * length(alphas) * length(betas) * length(rhos)
gnlSmallShape <- matrix(nrow = totalrows, ncol = 5)
rownum <- 1

for (i in 1:length(mus)) {
  for (j in 1:length(sigmas)) {
    for (k in 1:length(alphas)) {
      for (l in 1:length(betas)) {
        for (m in 1:length(rhos)) {
          param <- c(mus[i], sigmas[j], alphas[k], betas[l], rhos[m])
          gnlSmallShape[rownum, ] <- param
          rownum <- rownum + 1
        }
      }
    }
  }
}
