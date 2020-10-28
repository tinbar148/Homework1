#Read in each file and clean

#Audubon data
audubondata <- read.csv("audubon_data.csv", na.strings = "NA")

#Change date to standard format
audubondata$Date <- as.Date(audubondata$Date, format = "%m/%d/%y")

#Remove N/W and change to +/-
audubondata$Longitude <- substr(audubondata$Longitude, 2, nchar(audubondata$Longitude))
audubondata$Longitude <- -1*as.numeric(audubondata$Longitude)
audubondata$Latitude <- substr(audubondata$Latitude, 2, nchar(audubondata$Latitude))

#GW data
gwdata <- read.csv("gw_data_mac.csv", na.strings = "NA")

#Change date to standard format
gwdata$Date <- as.Date(gwdata$Date, format = "%d-%b-%y")

#Change longitude to decimal degrees
gwdata$Longitude <- strsplit(gwdata$Longitude, "°")
front <- sapply(gwdata$Longitude, "[[", 1)
back <- sapply(gwdata$Longitude, "[[", 2)
back <- substr(back, 1, nchar(back)-2) 
gwdata$Longitude <- as.numeric(front) + as.numeric(back)/60
gwdata$Longitude <- -1*as.numeric(gwdata$Longitude)

#Change latitude to decimal degrees
gwdata$Latitude <- strsplit(gwdata$Latitude, "°")
front2 <- sapply(gwdata$Latitude, "[[", 1)
back2 <- sapply(gwdata$Latitude, "[[", 2)
back2 <- substr(back2, 1, nchar(back2)-2) 
gwdata$Latitude <- as.numeric(front2) + as.numeric(back2)/60

#Nat geo data
natgeodata <- read.csv("nat_geo_data.csv", na.strings = "NA")

#Look at longitude values
summary(natgeodata$Longitude)
natgeodata[which(natgeodata$Longitude >= 0), ]

#Edit here, fixed three erroneous values
#Switch latitude and longitude for the three erroneous values
switch <- natgeodata[which(natgeodata$Longitude >= 0), ]
natgeodata$Latitude[which(natgeodata$Longitude >= 0)] <- switch$Longitude
natgeodata$Longitude[which(natgeodata$Longitude >= 0)] <- switch$Latitude

#Combine data
clean_data <- rbind(audubondata, gwdata, natgeodata)

#Edit here, changed to select transects from final file
#Remove nontransects and select only transects
clean_data <- clean_data[grepl("transect", clean_data$Survey_Type, ignore.case = TRUE), ]
clean_data <- clean_data[!grepl("nontransect", clean_data$Survey_Type), ]

#Select only data taken on or after Jan 1, 2010
clean_data <- subset(clean_data, Date>="2010-01-01")
clean_data <- subset(clean_data, Survey_Type=="transect")
row.names(clean_data) <- 1:nrow(clean_data)

#Export new spreadsheet
write.csv(clean_data, file = "my_clean_data.csv", row.names=FALSE)

##Assignment 1 Map Plotting Template

library(sp)
library(rgdal)

clean_data <- read.csv("my_clean_data.csv")

plotting_data <- SpatialPoints(clean_data[, c("Longitude", "Latitude")])

#Map of DC neighborhoods from maps2.dcgis.dc.gov
dc <- readOGR("Neighborhood_Clusters-shp", "Neighborhood_Clusters")
