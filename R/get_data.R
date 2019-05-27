library(tidyverse)
library(xml2)
library(rvest)
source("API_KEY.R")

sra <- c("Big Bog", "La Salle Lake", "Minnesota Valley", "Red River", "Garden Island",
         "Cuyuna Country", "Iron Range Off-Highway Vehicle", "Greenleaf Lake")
non.hiking.club <- c("Cuyuna Country", "Garden Island", "Greenleaf Lake",
                     "Hill Annex Mine", "Iron Range Off-Highway Vehicle", "John A. Latsch")

parks <- "parks.tsv" %>%
  read_tsv(col_names = TRUE, col_types = cols()) %>%
  mutate(
    Hiking.Club = !(Park %in% non.hiking.club),
    Park = paste(Park, "State", if_else(Park %in% sra, "Recreation Area", "Park"), "MN")
  )

pairs <- parks$Park %>%
  setdiff("Garden Island State Recreation Area MN") %>%
  combn(2) %>%
  t() %>%
  as.data.frame() %>%
  set_names(paste0("Park", 1:2)) %>%
  as_tibble() %>%
  filter(!(Park1 %in% parks$Park[parks$Hiking.Club]) | !(Park2 %in% parks$Park[parks$Hiking.Club])) %>%
  mutate(
    results = map(Park1, ~ list())
  )

to_park <- function(x) paste0(gsub("'", "%27", gsub(" ", "+", x)), collapse = "|")
get_park <- function(p1, p2)
{
  api <- "https://maps.googleapis.com/maps/api/distancematrix/xml?"
  url <- paste0(api, "origins=", to_park(p1),
                "&destinations=", to_park(p2),
                "&key=", API_KEY, "&mode=driving&units=metric")
  read_xml(url)
}

for(i in 1:nrow(pairs))
{
  pairs$results[[i]] <- get_park(pairs$Park1[i], pairs$Park2[i])
  if(i %% 10 == 0) print(i)
}

parse <- function(x, what)
{
  if(identical(x, list())) return(NA_character_)
  x %>%
    html_nodes(what) %>%
    html_nodes("value") %>%
    html_text()
}
parse2 <- function(x, what) html_text(html_nodes(x, what))

pairs$distance <- map_chr(pairs$results, parse, what = "distance")
pairs$duration <- map_chr(pairs$results, parse, what = "duration")
pairs$origin <- map_chr(pairs$results, parse2, what = "origin_address")
pairs$destination <- map_chr(pairs$results, parse2, what = "destination_address")
pairs$status <- map_lgl(pairs$results, ~ identical(c("OK", "OK"), html_text(html_nodes(.x, "status"))))
View(head(pairs))
anyNA(pairs$distance)
anyNA(pairs$duration)
anyNA(pairs$origin)
anyNA(pairs$destination)
table(pairs$status)
pairs$results <- NULL
write.table(pairs, "rawdata/paired_distances.csv", sep = ",", row.names = FALSE, append = TRUE,
            col.names = FALSE)


get_lat_long <- function(p)
{
  api <- "https://maps.googleapis.com/maps/api/geocode/xml?"
  url <- paste0(api, "address=", to_park(p), "&key=", API_KEY)
  read_xml(url)
}

parks$latlong <- map(parks$Park, get_lat_long)
parks$address <- map_chr(parks$latlong, ~ html_text(html_node(.x, "formatted_address")))
parks$lat <- map_chr(parks$latlong, ~ html_text(html_node(.x, "location lat")))
parks$long <- map_chr(parks$latlong, ~ html_text(html_node(.x, "location lng")))
parks$place_id <- map_chr(parks$latlong, ~ html_text(html_node(.x, "place_id")))
parks$status <- map_chr(parks$latlong, ~ html_text(html_node(.x, "status")))

anyNA(parks$address)
anyNA(parks$lat)
anyNA(parks$long)
anyNA(parks$place_id)
table(parks$status)
parks$latlong <- NULL
write.table(parks, "parks.tsv", sep = "\t", row.names = FALSE, col.names = TRUE)
