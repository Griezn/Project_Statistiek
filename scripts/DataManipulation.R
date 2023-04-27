airbnb <- read.csv2("./data/airbnb.csv", sep=" ")

# lengtegraad en breedtegraad verwijderen
airbnb$lat <- NULL
airbnb$lng <- NULL

# categorien omzetten naar labels
airbnb$room <- factor(airbnb$room, levels = c(1,2,3), labels = c("volledig", "afzonderlijk", "gedeeld"))
airbnb$host <- factor(airbnb$host, levels = c(0, 1, 2), labels = c("enige", "2 tot 4", "meer dan 4"))

# herschaling van 1-10
airbnb$attr <- (airbnb$attr - min(airbnb$attr)) * (10 - 1) / (max(airbnb$attr) - min(airbnb$attr)) + 1
airbnb$rest <- (airbnb$rest - min(airbnb$rest)) * (10 - 1) / (max(airbnb$rest) - min(airbnb$rest)) + 1

# save to RData
save(airbnb, file= "./data/airbnb.RData")
