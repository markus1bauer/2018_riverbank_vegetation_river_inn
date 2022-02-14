# Riverbank vegetation at River Inn
# Write metadata ####
# Markus Bauer
# 2022-02-14


### Packages ###
library(EML)

### Start ###
rm(list = ls())



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


creator <- eml$creator(
  individualName = eml$individualName(
    givenName = "Markus",
    surName = "Bauer"
  ),
  electronicMailAddress = "markusbauer@mailbox.org"
)

address <- list(
  deliveryPoint = "Emil-Ramann-Strasse 6",
  city = "Freising",
  administrativeArea = "Bayern",
  postalCode = "85354",
  country = "Germany"
)

contact <-
  list(
    individualName = creator$individualName,
    electronicMailAddress = creator$electronicMailAddress,
    address = address,
    organizationName = "Technical University of Munich",
    phone = "0049-152-56391781"
  )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


abstract <- "Empty"

keywordSet <- list(
  keywordThesaurus = "LTER controlled vocabulary",
  keyword = list(
    "rivers",
    "monitoring",
    "restoration",
    "riparian",
    "vegetation dynamics"
  )
)

geographicDescription <- "River Inn near Wasserburg"

coverage <- set_coverage(
  begin = "2014-06-01", end = "2016-07-31",
  sci_names = list(list(
    Kingdom = "Plantae",
    Division = "Tracheophyta",
    Subdivision = "Spermatophytina"
  )),
  geographicDescription = geographicDescription,
  west = 12.15060, east = 12.26716,
  north = 47.98487, south = 48.13731,
  altitudeMin = 413, altitudeMaximum = 433,
  altitudeUnits = "meter"
)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Finalize EML#############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


dataset <- list(
  title = "Resilience of riparian vegetation after restoration measures on River Inn",
  creator = creator,
  pubDate = "2018",
  language = "English",
  intellectualRights = "CC BY 4.0",
  abstract = abstract,
  keywordSet = keywordSet,
  coverage = coverage,
  contact = contact
)

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset
)

write_eml(eml, "METADATA.xml")
eml_validate("METADATA.xml")
