# laad dataset
load("./data/airbnb.RData")
attach(airbnb)

# onderzoek van de veranderlijken

#============================#
# realSum -> continue
# rechtsscheef, met een zware staart. 8000 is een outlier
hist(realSum)
# zonder outliers, nog steeds rechtsscheef
hist(realSum, xlim = range(0, 3000), breaks = 16)

summary(realSum)
#   Min.   | 1st Qu. | Median | Mean   | 3rd Qu.  | Max.
#   165.9  | 362.8   | 491.6  | 604.8  | 716.6    | 8130.7


#============================#
#room -> nominale veranderlijke
t_r <- table(room)
barplot(t_r)
pie(t_r)

summary(room)
#     volledig | afzonderlijk  | gedeeld
#          588 |          385  |       4

#============================#
# capacity -> discrete veranderlijke
t_c <- table(capacity)
barplot(t_c)
pie(t_c)

summary(capacity)
#   Min.   | 1st Qu. | Median | Mean   | 3rd Qu.  | Max.
#   2.00   | 2.00    | 2.00   | 2.77   | 4.00     | 6.00

#============================#
# bedrooms -> discrete veranderlijke
t_b <- table(bedrooms)
barplot(t_b)
pie(t_b)

summary(bedrooms)
#   Min.   | 1st Qu. | Median | Mean   | 3rd Qu.  | Max.
#   0.00   | 1.00    | 1.00   | 1.30   | 2.00     | 5.00

#============================#
# distance -> continue
# rechtsscheef, met een zware staart.
hist(dist)

summary(dist)
#   Min.    | 1st Qu. | Median  | Mean    | 3rd Qu. | Max.
#   0.01504 | 1.41004 | 2.31455 | 2.80634 | 3.61017 | 11.19593

#============================#
# metro -> continue
# rechtsscheef, met een lichte staart.
hist(metro)

summary(metro)
#   Min.    | 1st Qu. | Median  | Mean    | 3rd Qu. | Max.
#   0.03652 | 0.46714 | 0.87617 | 1.08929 | 1.50086 | 4.41190

#============================#
# rest -> continue
# rechtsscheef, met een lichte staart.
hist(rest)

summary(rest)
#   Min.  | 1st Qu. | Median | Mean  | 3rd Qu. | Max.
#   1.000 | 1.984   | 2.774  | 3.285 | 4.179   | 10.000

#============================#
# host -> ordinale veranderlijke
t_h <- table(host)
barplot(t_h)
pie(t_h)

summary(host)
# enige | 2 tot 4 | meer dan 4
#   636 |     249 |         92

#============================#
# cleanliness -> discrete veranderlijke
t_cl <- table(cleanliness)
# linksscheef met zware staart
barplot(t_cl)
pie(t_cl)

summary(cleanliness)
#   Min.  | 1st Qu. | Median | Mean  | 3rd Qu. | Max.
#   2.000 | 9.000   | 10.00  | 9.471 | 10.00   | 10.000

#============================#
# satisfaction -> discrete veranderlijke
t_s <- table(satisfaction)
# linksscheef met zware staart
barplot(t_s)
pie(t_s)

summary(satisfaction)
#   Min.  | 1st Qu. | Median | Mean  | 3rd Qu. | Max.
#   2.000 | 9.300   | 9.600  | 9.469 | 9.900   | 10.000