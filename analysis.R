
dat <- read.csv("paired_distances.csv", stringsAsFactors = FALSE)
parks <- unique(c(dat$Park1, dat$Park2))
dat.mat <- matrix(0, nrow = 68, ncol = 68, dimnames = list(parks, parks))
for(i in parks)
{
  for(j in parks)
  {
    if(i == j) next
    idx <- (dat$Park1 == i & dat$Park2 == j) | (dat$Park1 == j & dat$Park2 == i)
    stopifnot(sum(idx) == 1)
    dat.mat[i, j] <- dat$distance[idx]
  }
}

