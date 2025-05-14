#install.packages("auk")
#largely taken from the auk quickstart guide

library(tidyverse)
library(auk)
# path to the ebird data file
species <- c("Northern Shrike", "Trumpeter Swan", "American Woodcock",
             "Red-headed Woodpecker", "Hammond's Flycatcher", "Cassin's Vireo",
             "Broad-tailed Hummingbird", "Gray Flycatcher", "Wilson's Warbler",
             "Clay-colored Sparrow", "Hermit Thrush", "Northern Harrier",
             "Hooded Merganser", "Sandhill Crane", "Black-billed Cuckoo",
             "Palm Warbler", "Summer Tanager", "American Tree Sparrow")

setwd("D:/myFiles/ebd_US_smp_relMar-2025/ebd_US_relMar-2025/ebd_US_relMar-2025.txt")

#f_in <- system.file("initial_overview")
file.access("ebd_US_relMar-2025.txt", mode = 0)
file.exists("ebd_US_relMar-2025.txt")
file.info("ebd_US_relMar-2025.txt")
f_in <- Sys.glob("D:/myFiles/ebd_US_smp_relMar-2025/ebd_US_relMar-2025/ebd_US_relMar-2025.txt")
f_in <- system.file("D:/myFiles/ebd_US_smp_relMar-2025/ebd_US_relMar-2025/ebd_US_relMar-2025.txt", package = "auk", mustWork = TRUE)

# output text file
f_out <- "ebd_US_relMar-2025_filtered.txt"

ebird_data <- f_in %>%
  # 1. reference file
  auk_ebd() %>%
  # 2. define filters
  auk_species(species) %>%
  auk_country(country = "United States") %>%
  # 3. run filtering
  auk_filter(file = f_out, overwrite = TRUE) %>%
  # 4. read text file into r data frame

  read_ebd()


