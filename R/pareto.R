source("R/setup.R")

Mate <- function(tour1, tour2)
{
  tmp <- intersect(tour1, tour2)
  extras <- setdiff(union(tour1, tour2), tmp)
  c(tmp, sample(extras, length(tour1) - length(tmp)))
}

Mutate <- function(tour, allparks, cutoff = 0.05)
{
  if(runif(1) < cutoff) tour[sample(seq_along(tour), 1)] <- sample(setdiff(allparks, tour), 1)
  tour
}

opt <- function(distances, tour)
  if(length(tour) <= 8) brute.force(distances, tour) else three.opt(distances, tour)

find_shortest <- function(nparks = 10, nper = 100, ngen = 100, mutation = 0.05)
{
  stopifnot(nper > 4)
  allparks <- seq_len(N <- ncol(dat.mat))
  set.seed(88)

  if(nparks <= 4)
  {
    init <- combn(allparks, nparks)
    tours <- apply(init, 2, opt, distances = dat.mat)
    tour_dist <- apply(tours, 2, tour.dist, distances = dat.mat)
    wm <- which.min(tour_dist)
    return(list(init = init[, wm], tour = tours[, wm], dists = tour_dist[wm],
                parks = colnames(dat.mat)[tours[, wm]]))
  }

  out <- numeric(ngen)
  aver <- numeric(ngen)
  pop <- tibble(
    init = replicate(nper, sample(allparks, nparks, replace = FALSE), simplify = FALSE)
  )
  for(i in seq_len(ngen))
  {
    pop <- pop %>%
      mutate(
        tours = lapply(init, opt, distances = dat.mat),
        tour_dist = map_dbl(tours, tour.dist, distances = dat.mat)
      ) %>%
      arrange(tour_dist) %>%
      mutate(
        p = sqrt(n():1)/sum(sqrt(1:n()))
      )
    out[i] <- min(pop$tour_dist)
    aver[i] <- mean(pop$tour_dist)
    if(i != ngen)
    {
      pop$init <- c(
        list(pop$init[[1]]),
        replicate(
          floor(nper*0.75),
          Mate(pop$init[[sample(1:nper, 1, prob = pop$p)]],
               pop$init[[sample(1:nper, 1, prob = pop$p)]]),
          simplify = FALSE
        ),
        replicate(
          nper - 1 - floor(nper*0.75),
          sample(allparks, nparks, replace = FALSE),
          simplify = FALSE
        )
      )
      if(mutation > 0) pop$init <- map2(pop$init, c(0, rep(mutation, nper-1)),
                                        Mutate, allparks = allparks)
    }
  }
  list(init = pop$init[[1]], tour = pop$tours[[1]], dists = out,
       parks = colnames(dat.mat)[pop$tours[[1]]])
}

pareto <- tibble(
  tmp = lapply(2:10, find_shortest, ngen = 25),
  tour = purrr::map(tmp, "tour")
)
