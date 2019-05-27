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
  stopifnot(all(length(tour) <= dim(distances)))
  stopifnot(all(max(tour) <= dim(distances)))
  stopifnot(nrow(distances) == ncol(distances))
  stopifnot(length(tour) >= 6)
  three_opt(tour - 1L, distances, max.swaps, max.loops) + 1L
}
tour.dist <- function(distances, tour) tour_dist(tour - 1L, distances = distances)
brute.force <- function(distances, tour)
{
  stopifnot(all(max(tour) <= dim(distances)))
  stopifnot(nrow(distances) == ncol(distances))
  stopifnot(length(tour) <= 10 && length(tour) >= 2)
  brute_force(tour - 1L, distances = distances) + 1L
}
tours.equal <- function(tour1, tour2)
{
  if(!setequal(tour1, tour2)) return(FALSE)
  tour1 <- paste0(tour1, collapse = ',')
  tour2.rev <- paste0(rev(c(tour2, tour2)), collapse = ',')
  tour2 <- paste0(c(tour2, tour2), collapse = ',')
  grepl(tour1, tour2) || grepl(tour1, tour2.rev)
}

parkinfo <- "parks.tsv" %>%
  read_tsv(col_names = TRUE, col_types = cols()) %>%
  arrange(Park)
park.info <- filter(parkinfo, !grepl("^Garden Island", Park))

mn <- filter(map_data("state"), region == "minnesota")

plot_tour <- function(tour)
{
  tour.info <- park.info[tour[c(1:length(tour), 1)], ]

  p <- ggplot(mn, aes(x = long, y = lat)) +
    geom_polygon(fill = NA, color = "black") +
    geom_path(data = tour.info, color = "red") +
    geom_point(data = parkinfo, aes(color = Hiking.Club)) +
    scale_color_manual(values = c("red", "black")) +
    coord_equal() +
    theme_classic()
  plot(p)
  invisible(p)
}
