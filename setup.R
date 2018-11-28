library(tidyverse)
dat <- read.csv("paired_distances.csv", stringsAsFactors = FALSE)
parks <- sort(unique(c(dat$Park1, dat$Park2)))
dat.mat <- matrix(0, nrow = 73, ncol = 73, dimnames = list(parks, parks))
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

stopifnot(isSymmetric(dat.mat))
Rcpp::sourceCpp("three_opt.cpp")
three.opt <- function(distances, tour = 1:ncol(distances), max.swaps = 20000, max.loops = 100)
{
  stopifnot(all(length(tour) == dim(distances)))
  three_opt(tour - 1L, distances, max.swaps, max.loops) + 1L
}
tour.dist <- function(distances, tour) tour_dist(tour - 1L, distances = distances)

park.info <- read_tsv("parks.tsv", col_names = TRUE, col_types = cols())
mn <- filter(map_data("state"), region == "minnesota")
