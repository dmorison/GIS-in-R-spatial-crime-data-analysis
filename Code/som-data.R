setwd("C:/Users/davem/Projects/201703_crime-data/")
libs <- c("reshape2")
lapply(libs, library, character.only = TRUE)

importdata <- function(dirs) {
  all <- NULL
  ### Variables to keep
  retain <- c("Month", "Longitude", "Latitude", "LSOA.code", "LSOA.name", "Crime.type")
  for(d in dirs) {
    filename <- NULL
    switch(d,
           "city-of-london-police-coord-data" = {filename <- "city-of-london-street"},
           "met-police-coord-data" = {filename <- "metropolitan-street"}
           )
    currDir <- dir(file.path("Data", d))
    for(i in currDir) {
      currFilePath <- file.path("Data", d, i, paste0(i, "-", filename, ".csv"))
      # print(currFilePath)
      currFile <- read.csv(currFilePath)
      # print(dim(currFile))
      currFileSub <- currFile[retain]
      currFileSub <- currFileSub[which(complete.cases(currFileSub)), ]
      # print(dim(currFileSub))
      if(d == "met-police-coord-data") {
        currFileSub <- currFileSub[which(currFileSub$Latitude > 51.275), ]
        currFileSub <- currFileSub[which(currFileSub$Latitude < 51.7), ]
        currFileSub <- currFileSub[which(currFileSub$Longitude > -0.53), ]
        currFileSub <- currFileSub[which(currFileSub$Longitude < 0.35), ]
      }
      print(dim(currFileSub))
      all <- rbind(all, currFileSub)
    }
  }
  return(all)
}

crimes <- importdata(c("city-of-london-police-coord-data", "met-police-coord-data"))
crimeTypes <- dcast(crimes, LSOA.name ~ Crime.type, value.var = "Crime.type")
colnames(crimeTypes) <- c("LSOA_name", "anti_social_behaviour", "bicycle_theft", "burglary",
                          "criminal_damage_and_arson", "drugs", "other_crime", "other_theft",
                          "possession_of_weapons", "public_order", "robbery", "shoplifting",
                          "theft_from_the_person", "vehicle_crime", "violence_and_sexual_offences")

write.csv(crimeTypes, file = "Data/crime-types.csv", row.names = FALSE)
# ggplot(crimeTypes, aes(x = drugs, y = bicycle_theft)) + geom_point()

