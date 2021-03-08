# Prepare Metadata ####
# Markus Bauer


### Packages ###
library(EML)

### Start ###
rm(list = ls())



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Create infos about persons ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


creator <- eml$creator(
  individualName = eml$individualName(
    givenName = "Markus", 
    surName = "Bauer"),
  electronicMailAddress = "markusbauer@mailbox.org"
)

address <- list(
  deliveryPoint = "Emil-Ramann-Strasse 6",
  city = "Freising",
  administrativeArea = "Bayern",
  postalCode = "85354",
  country = "Germany")

contact <- 
  list(
    individualName = creator$individualName,
    electronicMailAddress = creator$electronicMailAddress,
    address = address,
    organizationName = "Technical University of Munich",
    phone = "0049-152-56391781"
    )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create site and experiment infos ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


abstract <- "River restoration is widely applied, although its effects are poorly understood, and degraded hab-
itats might be difficult to improve. Moreover, there is a lack of monitoring as well as few system-
atic comparisons of restoration methods. This study presents results of a 4 - year monitoring on
River Inn (southern Germany) investigating restoration by gravel or sand addition or embankment
removal. The results were compared with reference sites that represent the pre - restoration
conditions. At the landscape scale, we analysed vegetation types based on aerial photographs,
whereas at a smaller scale, we undertook vegetation surveys and evaluated species composition,
growth, and life form, as well as the proportion of the target vegetation. After 4 years, the data
indicated a 'negative resilience' of the vegetation back to the state prior to restoration. The struc-
tural analysis revealed an extensive spread of reed at expense of bare soil. Thus, the species com-
position largely regressed to the pre - restoration conditions, and neither annuals nor other
pioneer species showed a long - term benefit of river restoration. There were differences among
the three restoration treatments after 2 years, but no longer after 4 years. However, the river res-
toration had three positive outcomes: (a) There was a temporary benefit for pioneer vegetation
that most likely replenished the seed bank of the respective species, (b) the valuable reed commu-
nities showed resilience, and (c) the measures allowed some practical learning as expected for
adaptive restoration."

keywordSet <- list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list(
      "rivers",
      "monitoring",
      "restoration",
      "riparian",
      "vegetation dynamics")
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
  west = 12.15060 , east = 12.26716,
  north = 47.98487, south = 48.13731,
  altitudeMin = 413, altitudeMaximum = 433,
  altitudeUnits = "meter"
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C finalize EML ##############################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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

setwd("Z:/Documents/0_Uni/2016_Projekt_7_Inn_Bachelorthesis/3_Aufnahmen_und_Ergebnisse/2018_monitoring_River_Inn")
write_eml(eml, "METADATA.xml")
eml_validate("METADATA.xml")
