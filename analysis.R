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

generate_tour <- function(what = c("all", "sp.only", "hc.only"))
{
  what <- match.arg(what)
  stopifnot(all(names(park.info$Park) == colnames(dat.mat)))
  if(what == "hc.only")
  {
    hc <- colnames(dat.mat) %in% park.info$Park[park.info$Hiking.Club]
    dat.mat.tmp <- dat.mat[hc, hc]
  } else if(what == "sp.only")
  {
    sp <- grepl("State Park MN$", colnames(dat.mat))
    dat.mat.tmp <- dat.mat[sp, sp]
  } else dat.mat.tmp <- dat.mat

  N <- ncol(dat.mat.tmp)
  set.seed(88)
  tour <- tibble(
    init = c(list(1:N), replicate(500, sample(1:N, N, replace = FALSE), simplify = FALSE)),
    tours = lapply(init, three.opt, distances = dat.mat.tmp),
    tour_dist = map_dbl(tours, tour.dist, distances = dat.mat.tmp)
  ) %>%
    arrange(tour_dist)

  tour.info <- slice(park.info, match(colnames(dat.mat.tmp)[tour$tours[[1]][c(1:N, 1)]], Park))

  p <- ggplot(mn, aes(x = long, y = lat)) +
    geom_polygon(fill = NA, color = "black") +
    geom_path(data = tour.info, color = "red") +
    geom_point(data = park.info, aes(color = Hiking.Club)) +
    scale_color_manual(values = c("red", "black")) +
    coord_equal() +
    theme_classic()
  plot(p)
  tour
}

out <- generate_tour("all")

cat(paste0(colnames(dat.mat)[out$tours[[1]][c(1:73, 1)]], collapse = "\n"),
    "\n", sep = "", file = "optimal_map.csv")
