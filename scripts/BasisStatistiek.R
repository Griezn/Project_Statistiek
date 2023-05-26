load("./data/airbnb.RData")
attach(airbnb)

# calculate the mean and Standard Error of the Mean, SEM of the numeric variables

# mean
mean(realSum) # 604.828
mean(capacity) # 2.769703
mean(bedrooms) # 1.302968
mean(dist) # 2.806345
mean(metro) # 1.089285
mean(attr) # 2.100636
mean(rest) # 3.284797
mean(cleanliness) # 9.470829
mean(satisfaction) # 9.468577

# SEM
sd(realSum)/sqrt(length(realSum)) # 14.19467
sd(capacity)/sqrt(length(capacity)) # 0.03262872
sd(bedrooms)/sqrt(length(bedrooms)) # 0.02344912
sd(dist)/sqrt(length(dist)) # 0.06515667
sd(metro)/sqrt(length(metro)) # 0.02644384
sd(attr)/sqrt(length(attr)) # 0.0300981
sd(rest)/sqrt(length(rest)) # 0.05630767
sd(cleanliness)/sqrt(length(cleanliness)) # 0.02657482
sd(satisfaction)/sqrt(length(satisfaction)) # 0.02121881

# range
range(realSum) # 165.9129 8130.6681
range(capacity) # 2 6
range(bedrooms) # 0 5
range(dist) # 0.01504452 11.19593222
range(metro) # 0.03651741 4.41190504
range(attr) # 1 10
range(rest) # 1 10
range(cleanliness) # 2 10
range(satisfaction) # 2 10

# plot every variable so we can see the distribution
hist(realSum, main="Histogram of realSum", xlab="realSum") # rechtsscheef, met een zware staart
hist(capacity, main="Histogram of capacity", xlab="capacity")
hist(bedrooms, main="Histogram of bedrooms", xlab="bedrooms")
hist(dist, main="Histogram of dist", xlab="dist") # rechtsscheef, met een zware staart.
hist(metro, main="Histogram of metro", xlab="metro") # rechtsscheef, met een lichte staart.
hist(attr, main="Histogram of attr", xlab="attr") # rechtsscheef, met een lichte staart.
hist(rest, main="Histogram of rest", xlab="rest") # rechtsscheef, met een lichte staart.
hist(cleanliness, main="Histogram of cleanliness", xlab="cleanliness") # linksscheef met zware staart
hist(satisfaction, main="Histogram of satisfaction", xlab="satisfaction") # linksscheef met zware staart


