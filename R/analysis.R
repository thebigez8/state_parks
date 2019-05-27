source("setup.R")

generate_tour <- function(what = c("all", "sp.only", "hc.only"))
{
  what <- match.arg(what)
  stopifnot(all(park.info$Park == colnames(dat.mat)))
  if(what == "hc.only")
  {
    whch <- which(parkinfo$Hiking.Club)
  } else if(what == "sp.only")
  {
    whch <- grep("State Park MN$", colnames(dat.mat))
  } else whch <- 1:ncol(dat.mat)
  N <- length(whch)
  set.seed(88)
  tour <- tibble(
    init = c(list(whch), replicate(500, sample(whch, N, replace = FALSE), simplify = FALSE)),
    tours = lapply(init, three.opt, distances = dat.mat),
    tour_dist = map_dbl(tours, tour.dist, distances = dat.mat)
  ) %>%
    arrange(tour_dist)

  plot_tour(tour$tours[[1]])
  tour
}

out <- generate_tour("all")

cat(paste0(colnames(dat.mat)[out$tours[[1]][c(1:73, 1)]], collapse = "\n"),
    "\n", sep = "", file = "optimal_map.csv")
