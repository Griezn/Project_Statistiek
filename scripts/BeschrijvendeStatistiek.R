# laad dataset
load("./data/airbnb.RData")
attach(airbnb)

# onderzoek van de veranderlijken

#============================#
# realSum -> continue
# rechtsscheef, met een zware staart. 8000 is een outlier
# Ook duidelijk te zien op de bloxplot
hist(realSum)
boxplot(realSum)
# zonder outliers, nog steeds rechtsscheef
realSum2 <- realSum[realSum < 3000]
hist(realSum2, breaks = 16)
boxplot(realSum2)

summary(realSum)
#   Min.   | 1st Qu. | Median | Mean   | 3rd Qu.  | Max.
#   165.9  | 362.8   | 491.6  | 604.8  | 716.6    | 8130.7
sd(realSum, na.rm = TRUE) # 443,6828


#============================#
#room -> nominale veranderlijke
t_r <- table(room)
barplot(t_r)
pie(t_r)

summary(room)
#     volledig | afzonderlijk  | gedeeld
#          588 |          385  |       4

# calculate proportions
prop.table(t_r)
#    volledig afzonderlijk      gedeeld
# 0.601842375  0.394063460  0.004094166


#============================#
# capacity -> discrete veranderlijke
t_c <- table(capacity)
barplot(t_c)
pie(t_c)

summary(capacity)
#   Min.   | 1st Qu. | Median | Mean   | 3rd Qu.  | Max.
#   2.00   | 2.00    | 2.00   | 2.77   | 4.00     | 6.00
sd(capacity, na.rm = TRUE) # 1,019876

#============================#
# bedrooms -> discrete veranderlijke
t_b <- table(bedrooms)
barplot(t_b)
pie(t_b)

summary(bedrooms)
#   Min.   | 1st Qu. | Median | Mean   | 3rd Qu.  | Max.
#   0.00   | 1.00    | 1.00   | 1.30   | 2.00     | 5.00
sd(bedrooms, na.rm = TRUE) # 0,7329

#============================#
# distance -> continue
# rechtsscheef, met een zware staart.
hist(dist)
boxplot(dist)

summary(dist)
#   Min.    | 1st Qu. | Median  | Mean    | 3rd Qu. | Max.
#   0.01504 | 1.41004 | 2.31455 | 2.80634 | 3.61017 | 11.19593
sd(dist, na.rm = TRUE) # 2,03

#============================#
# metro -> continue
# rechtsscheef, met een lichte staart.
hist(metro)
boxplot(metro)

summary(metro)
#   Min.    | 1st Qu. | Median  | Mean    | 3rd Qu. | Max.
#   0.03652 | 0.46714 | 0.87617 | 1.08929 | 1.50086 | 4.41190
sd(metro, na.rm = TRUE) # 0,82655

#============================#
# rest -> continue
# rechtsscheef, met een lichte staart.
hist(rest)
boxplot(rest)

summary(rest)
#   Min.  | 1st Qu. | Median | Mean  | 3rd Qu. | Max.
#   1.000 | 1.984   | 2.774  | 3.285 | 4.179   | 10.000
sd(rest, na.rm = TRUE) # 1,76

#============================#
# host -> ordinale veranderlijke
t_h <- table(host)
barplot(t_h)
pie(t_h)

summary(host)
# enige | 2 tot 4 | meer dan 4
#   636 |     249 |         92
prop.table(t_h)

#============================#
# cleanliness -> discrete veranderlijke
t_cl <- table(cleanliness)
# linksscheef met zware staart
barplot(t_cl)
pie(t_cl)

summary(cleanliness)
#   Min.  | 1st Qu. | Median | Mean  | 3rd Qu. | Max.
#   2.000 | 9.000   | 10.00  | 9.471 | 10.00   | 10.000
sd(cleanliness, na.rm = TRUE) # 0,83

#============================#
# satisfaction -> discrete veranderlijke
t_s <- table(satisfaction)
# linksscheef met zware staart
hist(satisfaction)
pie(t_s)

summary(satisfaction)
#   Min.  | 1st Qu. | Median | Mean  | 3rd Qu. | Max.
#   2.000 | 9.300   | 9.600  | 9.469 | 9.900   | 10.000
sd(satisfaction, na.rm = TRUE) # 0,663