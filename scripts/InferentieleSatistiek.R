# laad dataset
load("C:/Users/r0951309/IdeaProjects/Project_Statistiek/data/airbnb.RData")
load("./data/airbnb.RData")
attach(airbnb)

#==== KENMERKEN VD STEEKPROEF ====#

# Gemiddelde kost 2019 tov 2023
n <- length(realSum); n
# n = 977, en is dus groot genoeg -> CLS voldaan
ha <- mean(realSum)
h0 <- 620

# t = -1.0689, df = 976, p-value = 0.2854
# t_0.025 = -1.962398 < t -> niet significant meer
t.test(realSum, mu=h0, alternative = "two.sided")
qt(0.025, 976)


# particuliere vs professionele
host2 <- as.character(host)
host2[host2 == "enige"] <- "particulier"
host2[host2 != "particulier"] <- "professioneel"
t_h <- table(host2)
#  particulier professioneel
#          636           341
prop.test(t_h, alternative = "two.sided") # p-value < 2.2e-16


# controle als het aantal beschikbare kamers de poissonverdeling volgt
lambda <- median(bedrooms)
expected_freq <- dpois(0:5, lambda)
expected_freq <- expected_freq / sum(expected_freq)
observed_freq <- table(bedrooms)

chisq.test(observed_freq, p = expected_freq) # p-value < 2.2e-16
chisq.test(observed_freq, p = expected_freq)$expected
chisq.test(observed_freq, p=expected_freq)$residuals
# p-waarde is zeer klein dus de h0 wordt verworpen, het aantal beschikbare kamers volgt niet de poissonverdeling
# de expected values bij 0 zijn veel te hoog en bij 1 veel te laag, de andere waarden zijn ongeveer gelijk

#==== GEMIDDELDE KOST ====#

# verschilt de totale kost als het een maximum score heeft voor netheid
# dit zijn ongepaarde groepen
max <- realSum[cleanliness == 10]
rest <- realSum[cleanliness != 10]
mean(max)
mean(rest)
abs(mean(max) - mean(rest))
# controleren als de veranderlijken normaal verdeeld zijn
hist(max)
hist(rest)
# Uit de kwantielplotten kunnen we niet zeggen dat ze normaal verdeeld zijn
qqnorm(max); qqline(max)
qqnorm(rest); qqline(rest)
# zelfs zonder de outliers kunnen we nog niet spreken van normaal verdeeld
qqnorm(max, ylim = range(0, 2000)); qqline(max)
qqnorm(rest, ylim = range(0, 1500)); qqline(rest)
# we doen de SWT test om te testen op normaliteit
shapiro.test(max)
shapiro.test(rest)
# de shaprio test verwerpt de normaliteit in beide groepen p is ongeveer 0% in beide gevallen
length(max)
length(rest)
# n is in beide groepen groot genoeg dus uit de CLS volgt dat de gemiddelden ongeveer normaal verdeeld zijn
# daarom gebruiken we de t-test met verschillende varianties
t.test(max, rest, paired = FALSE, var.equal = FALSE) # p = 0.1513, t = 1.4362
qt(0.025, 959.48) # -1.96244
qt(0.975, 959.48) # 1.96244
# De p-waarde is groter dan 0.05 dus moeten we de h0 accepteren wat dus betekent dat
# Conclusie: We kunnen besluiten uit de steekproef dat een verblijf met maximum score netheid
# niet significant meer kost dan een kamer zonder maximum score netheid.


# verschilt de kost als de eigeneer meer dan één woning aanbied
enige <- realSum[host == "enige"]
meerdere <- realSum[host != "enige"]
mean(enige)
mean(meerdere)
abs(mean(enige) - mean(meerdere))
# het verschil in gemiddelden is significant anders van 0
qqnorm(enige); qqline(enige)
qqnorm(meerdere); qqline(meerdere)
# uit d e kwantielplotten leiden we eerder af dat de gegevens niet normaal verdeeld zijn
# maar we merken ook enkele outliers
qqnorm(enige, ylim = range(0, 2000)); qqline(enige)
qqnorm(meerdere, ylim = range(0, 2000)); qqline(meerdere)
# zonder outliers lijkt de normaliteit al iets meer in zicht, maar we kunnen het nog niet besluiten
# Daarom doen we de shapiro test op normaliteit
shapiro.test(enige)
shapiro.test(meerdere)
# de p-waarde van beide groepen is weer zeer klein, ongeveer 0, dus zijn de gegevens niet normaal verdeeld
# de n is wel groot genoeg in beide gevallen dus door de CLS zijn de gemiddelden ongeveer normaal verdeeld
length(enige)
length(meerdere)
# daarom doen we de t-test met verschillende varianties
t.test(enige, meerdere, paired = FALSE, var.equal = FALSE) # t = 2.2383, p = 0.0256
qt(0.025, 551.88) # -1.964272
qt(0.975, 551.88) # 1.964272
# de t-waarde is groter dan qt(0.0975, 551.88) en ligt dus in het verwerpingsgebied
# Conclusie: Uit de steekproef kunnne we afleiden dat de totale kost voor een verblijf significant verschilt
# als de eigenaar slechts één tov meerdere verblijven aanbied.


# Verschilt de totale prijs naargelang de volledegie woning verhuurd wordt of niet
table(room)
volledig <- realSum[room == "volledig"]
afzonderlijk <- realSum[room != "volledig"]
mean(volledig)
mean(afzonderlijk)
abs(mean(volledig) - mean(afzonderlijk))
# Het verschil in gemiddelden is significant verschillend van 0
qqnorm(volledig); qqline(volledig)
qqnorm(volledig, ylim = range(0, 2000)); qqline(volledig)
qqnorm(afzonderlijk); qqline(afzonderlijk)
qqnorm(afzonderlijk, ylim = range(0, 1000)); qqline(afzonderlijk)
# Uit de kwantielplotten lijkt het dat de gegevens niet normaal verdeeld zijn
# We besluiten de normaliteit aan de hand van een shaprio test
shapiro.test(volledig)
shapiro.test(afzonderlijk)
# uit de testen blijkt dat beide gegvens niet normaal verdeeld zijn
# de n is wel groot genoeg in beide gevallen dus door de CLS zijn de gemiddelden ongeveer normaal verdeeld
length(volledig)
length(afzonderlijk)
# daarom doen we de t-test met verschillende varianties
t.test(volledig, afzonderlijk, paired = FALSE, var.equal = FALSE) # t = 14.086, p ~ 0
qt(0.025, 850.2) # -1.962758
qt(0.975, 850.2) # 1.962758
# Een t-waarde van 14.086 ligt ver buiten het verwerpingsgebied
# dus doen we nog een extra controle, voor moesten de gemiddelden toch niet normaal verdeeld zijn
wilcox.test(volledig, afzonderlijk, paired = FALSE) # W = 1.9e+05, p = 2.2e-16
# Conclusie: Uit de steekproef kunnen we besluiten dat de totale kost voor een verblijf significant verschilt
# als de volledige woning verhuurd wordt tov afzonderlijke kamers.


#==== ASSOCIATIES ====#
# Nu gaan we na of er afhankelijkheid is tussen de totalen en de verschillende variabelen

# is er correlatie tussen de totale kost en de variable room
# realSum
# Room is een nominale veranderlijke dus met kruistabel
realSumKlassen <- cut(realSum, breaks = c(0, 200, 300, 400, 500, 600, 700, 800, 1000, 2000, 9000))
# Afzonderlijk en gedeeld moet worden samengenomen om te voldoen aan de cochran regel
room2 <- as.character(room)
room2[room2=="gedeeld"] <- "afzonderlijk"
table(realSumKlassen, room2)

chisq.test(realSumKlassen, room2) # p-value = 4.76e-12 -> p-waarde zeer klein , zeer afhankelijk
chisq.test(realSumKlassen, room2)$expected # laagste excpected value = 4.777 dus dicht genoeg bij de 5
chisq.test(realSumKlassen, room2)$residuals
# De gegevens tonen afhankelijkheid tussen beide veranderlijken (zeer significant)

plot(log10(realSum) ~ room)

# is er correlatie tussen de totale kost en de variable capacity
# capacity is een ordinale veranderlijke dus met kruistabel
realSumKlassen <- cut(realSum, breaks = c(0, 200, 300, 400, 500, 600, 700, 800, 1000, 2000, 9000))
capacity2 <- capacity
capacity2[capacity2==6] <- 4
capacity2[capacity2==5] <- 4


table(realSumKlassen, capacity2)

chisq.test(realSumKlassen, capacity2) # p-value < 2.2e-16 -> p-waarde zeer klein , zeer afhankelijk
chisq.test(realSumKlassen, capacity2)$expected
chisq.test(realSumKlassen, capacity2)$residuals

boxplot(log10(realSum) ~ capacity)


# is er correlatie tussen de totale kost en de variable bedrooms
# bedrooms is een ordinale veranderlijke en er zijn ties dus met kruistabel
realSumKlassen <- cut(realSum, breaks = c(0, 300, 400, 500, 600, 700, 800, 1000, 9000))

bedrooms2 <- bedrooms
bedrooms2[bedrooms2==5] <- 2
bedrooms2[bedrooms2==4] <- 2
bedrooms2[bedrooms2==3] <- 2

table(realSumKlassen, bedrooms2)

chisq.test(realSumKlassen, bedrooms2) # p-value < 2.2e-16 -> p-waarde zeer klein , zeer afhankelijk
chisq.test(realSumKlassen, bedrooms2)$expected
chisq.test(realSumKlassen, bedrooms2)$residuals

boxplot(log10(realSum) ~ bedrooms)


# is er correlatie tussen de totale kost en de variable distance
# distance is een continue veranderlijke dus eerst testen op normaliteit
shapiro.test(dist) # p-value < 2.2e-16 -> p-waarde zeer klein , niet normaal verdeeld
# dus gebruiken we de spearman correlatie
cor.test(realSum, dist, method = "spearman") # p-value < 2.2e-16 -> p-waarde zeer klein , zeer afhankelijk

plot(log10(realSum) ~ dist)
abline(lm(log10(realSum) ~ dist), col = "red")

# is er correlatie tussen de totale kost en de variable metro
# metro is een continue veranderlijke dus eerst testen op normaliteit
shapiro.test(metro) # p-value < 2.2e-16 -> p-waarde zeer klein , niet normaal verdeeld
# dus gebruiken we de spearman correlatie
cor.test(realSum, metro, method = "spearman") # p-value = 9.476e-10 -> p-waarde zeer klein , zeer afhankelijk

plot(log10(realSum) ~ metro)
abline(lm(log10(realSum) ~ metro), col = "red")


# is er correlatie tussen de totale kost en de variable attractions
# attractions is een continue veranderlijke dus eerst testen op normaliteit
shapiro.test(attr) # p-value < 2.2e-16 -> p-waarde zeer klein , niet normaal verdeeld
# dus gebruiken we de spearman correlatie
cor.test(realSum, attr, method = "spearman") # p-value < 2.2e-16 -> p-waarde zeer klein , zeer afhankelijk

plot(log10(realSum) ~ log10(attr))
abline(lm(log10(realSum) ~ log10(attr)), col = "red")


# is er correlatie tussen de totale kost en de variable restaurants
# restaurants is een continue veranderlijke dus eerst testen op normaliteit
shapiro.test(rest) # p-value < 2.2e-16 -> p-waarde zeer klein , niet normaal verdeeld
# dus gebruiken we de spearman correlatie
cor.test(realSum, rest, method = "spearman") # p-value < 2.2e-16 -> p-waarde zeer klein , zeer afhankelijk

plot(log10(realSum) ~ rest)
abline(lm(log10(realSum) ~ rest), col = "red")


# is er correlatie tussen de totale kost en de variable host
# host is een ordinale veranderlijke en er zijn ties dus met kruistabel
realSumKlassen <- cut(realSum, breaks = c(0, 200, 300, 400, 500, 600, 700, 800, 1000, 2000, 9000))
meerdere <- as.character(host)
meerdere[meerdere=="2 tot 4"] <- "meerdere"
meerdere[meerdere=="meer dan 4"] <- "meerdere"

table(realSumKlassen, meerdere)

chisq.test(realSumKlassen, meerdere) # p-value = 0.0023 -> p-waarde is niet zo klein , een beetje afhankelijk
chisq.test(realSumKlassen, meerdere)$expected
chisq.test(realSumKlassen, meerdere)$residuals

plot(log10(realSum) ~ host)


# is er correlatie tussen de totale kost en de variable cleanliness
# cleanlines is een ordinale veranderlijke en er zijn ties dus met kruistabel
realSumKlassen <- cut(realSum, breaks = c(0, 300, 400, 500, 600, 700, 800, 1000, 9000))
cleanliness2 <- cleanliness
cleanliness2[cleanliness2==2] <- 8
cleanliness2[cleanliness2==3] <- 8
cleanliness2[cleanliness2==4] <- 8
cleanliness2[cleanliness2==5] <- 8
cleanliness2[cleanliness2==6] <- 8
cleanliness2[cleanliness2==7] <- 8

table(realSumKlassen, cleanliness2)

chisq.test(realSumKlassen, cleanliness2) # p-value = 0.01148 -> p-waarde is ongeveer 1%, een beetje afhankelijk
chisq.test(realSumKlassen, cleanliness2)$expected
chisq.test(realSumKlassen, cleanliness2)$residuals

boxplot(log10(realSum) ~ cleanliness2)


# is er correlatie tussen de totale kost en de variable satisfaction
# satisfaction is een continue veranderlijke dus eerst testen op normaliteit
shapiro.test(satisfaction) # p-value < 2.2e-16 -> p-waarde zeer klein , niet normaal verdeeld
# dus gebruiken we de spearman correlatie
cor.test(realSum, satisfaction, method = "spearman") # p-value = 1.657e-07 -> p-waarde zeer klein , zeer afhankelijk

plot(log10(realSum) ~ exp(satisfaction))
abline(lm(log10(realSum) ~ exp(satisfaction)), col = "red")


#==== REGRESIE ====#
# kost in functie van de acctractie score
model <- lm(realSum ~ attr); summary(model)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)
#(Intercept)   338.93      33.49   10.12   <2e-16 ***
#attr          126.58      14.55    8.70   <2e-16 ***

#Residual standard error: 427.6 on 975 degrees of freedom
#Multiple R-squared:  0.07204,   Adjusted R-squared:  0.07109
#F-statistic: 75.69 on 1 and 975 DF,  p-value: < 2.2e-16
betrouwbh <- predict(model, interval = "confidence", level = 0.95)
predictie <- predict(model, interval = "prediction", level = 0.95)
plot(realSum ~ attr)
abline(model, col='red')
x_i <- model$model[, 2]
lines(sort(x_i), betrouwbh[order(x_i), 2], col='blue')
lines(sort(x_i), betrouwbh[order(x_i), 3], col='blue')
lines(sort(x_i), predictie[order(x_i), 2], col='green')
lines(sort(x_i), predictie[order(x_i), 3], col='green')

logmodel <- lm(log10(realSum) ~ log10(attr)); summary(logmodel)
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.55138    0.01312  194.44   <2e-16 ***
#log10(attr)  0.56912    0.03943   14.43   <2e-16 ***

#Multiple R-squared:  0.176,     Adjusted R-squared:  0.1752
#F-statistic: 208.3 on 1 and 975 DF,  p-value: < 2.2e-16
betrouwbh <- predict(logmodel, interval = "confidence", level = 0.95)
predictie <- predict(logmodel, interval = "prediction", level = 0.95)
plot(log10(realSum) ~ log10(attr))
abline(logmodel, col='red')
x_i <- logmodel$model[,2]
lines(sort(x_i), betrouwbh[order(x_i), 2], col='blue')
lines(sort(x_i), betrouwbh[order(x_i), 3], col='blue')
lines(sort(x_i), predictie[order(x_i), 2], col='green')
lines(sort(x_i), predictie[order(x_i), 3], col='green')


# meervoudig regressiemodel
model <- lm(realSum ~ dist + attr + metro + rest + satisfaction); summary(model)
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -186.99     211.08  -0.886 0.375899
#dist           -29.64      10.15  -2.919 0.003590 **
#attr            69.97      27.49   2.546 0.011063 *
#metro          -12.81      18.53  -0.691 0.489666    
#rest             5.08      16.57   0.307 0.759213
#satisfaction    76.60      20.45   3.745 0.000191 ***

model <- update(model, ~.-rest); summary(model)
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
#(Intercept)  -175.316    207.519  -0.845 0.398420
#dist          -31.019      9.103  -3.408 0.000682 ***
#attr           75.706     20.120   3.763 0.000178 ***
#metro         -13.668     18.310  -0.746 0.455561
#satisfaction   76.363     20.429   3.738 0.000196 ***

model <- update(model, ~.-metro); summary(model)
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -190.900    206.419  -0.925 0.355290
#dist          -31.985      9.008  -3.551 0.000402 ***
#attr           79.480     19.470   4.082 4.83e-05 ***
#satisfaction   75.886     20.415   3.717 0.000213 ***

plot(model)

logmodel <- lm(log10(realSum) ~ log10(dist) + log10(attr) + exp(satisfaction)); summary(logmodel)
#Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)
#(Intercept)        2.483e+00  4.151e-02  59.805  < 2e-16 ***
#log10(dist)       -4.546e-02  4.173e-02  -1.089    0.276
#log10(attr)        4.874e-01  8.583e-02   5.679 1.79e-08 ***
#exp(satisfaction)  7.289e-06  1.114e-06   6.544 9.69e-11 ***
plot(logmodel)


# nagaan of het zin heeft om afzonderlijke vergelijkingen te hanteren naargelang het een volledig verhuurde of niet volledig verhuurde woning betreft
logmodel.room <- update(logmodel, ~.*room); summary(logmodel.room)
#Coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)
#(Intercept)                         2.598e+00  4.365e-02  59.529  < 2e-16 ***
#log10(dist)                         2.121e-02  4.359e-02   0.487   0.6267
#log10(attr)                         5.625e-01  8.838e-02   6.365 3.02e-10 ***
#exp(satisfaction)                   2.627e-06  1.276e-06   2.059   0.0398 *
#roomafzonderlijk                   -8.238e-02  7.463e-02  -1.104   0.2699
#roomgedeeld                        -1.016e+00  8.947e-01  -1.135   0.2565
#log10(dist):roomafzonderlijk       -1.884e-01  7.488e-02  -2.516   0.0120 *
#log10(dist):roomgedeeld             6.301e-01  9.230e-01   0.683   0.4950
#log10(attr):roomafzonderlijk       -3.624e-01  1.552e-01  -2.336   0.0197 *
#log10(attr):roomgedeeld             9.858e-01  1.385e+00   0.712   0.4766
#exp(satisfaction):roomafzonderlijk  1.683e-06  1.936e-06   0.869   0.3850
#exp(satisfaction):roomgedeeld       8.861e-06  1.826e-05   0.485   0.6277

anova(logmodel.room)
#Response: log10(realSum)
#                        Df  Sum Sq Mean Sq  F value    Pr(>F)
#log10(dist)              1  7.5018  7.5018 262.0481 < 2.2e-16 ***
#log10(attr)              1  1.3144  1.3144  45.9131 2.148e-11 ***
#exp(satisfaction)        1  1.7291  1.7291  60.3998 1.977e-14 ***
#room                     2 11.4378  5.7189 199.7708 < 2.2e-16 ***
#log10(dist):room         2  0.0252  0.0126   0.4395   0.64447
#log10(attr):room         2  0.1753  0.0877   3.0624   0.04723 * -> significant
#exp(satisfaction):room   2  0.0273  0.0137   0.4769   0.62082
#Residuals              965 27.6255  0.0286

betas <- logmodel.room$coefficients; betas
#(Intercept)                        log10(dist)                        log10(attr)
#2.598150e+00                       2.120829e-02                       5.625165e-01
#exp(satisfaction)                   roomafzonderlijk                        roomgedeeld
#2.627196e-06                      -8.237942e-02                      -1.015786e+00
#log10(dist):roomafzonderlijk            log10(dist):roomgedeeld       log10(attr):roomafzonderlijk
#-1.883811e-01                       6.301221e-01                      -3.624138e-01
#log10(attr):roomgedeeld exp(satisfaction):roomafzonderlijk      exp(satisfaction):roomgedeeld
#9.858018e-01                       1.682917e-06                       8.860690e-06


intercept <- c(betas[1], betas[1] + betas[5:6]); intercept

slope_dist <- c(betas[2], betas[2] + betas[7:8]); slope_dist
slope_attr <- c(betas[3], betas[3] + betas[9:10]); slope_attr
slope_satisfaction <- c(betas[4], betas[4] + betas[11:12]); slope_satisfaction

plot(log10(realSum) ~ log10(attr), col = room)
for (k in 1:3) {
  abline(intercept[k], slope_attr[k], col = k)
}
legend("topleft", legend = c("afzonderlijk", "gedeeld", "volledig"), col = 1:3, lty = 1, bty = "n")

plot(log10(realSum) ~ log10(dist), col = room)
for (k in 1:3) {
  abline(intercept[k], slope_dist[k], col = k)
}
legend("topleft", legend = c("afzonderlijk", "gedeeld", "volledig"), col = 1:3, lty = 1, bty = "n")

plot(log10(realSum) ~ exp(satisfaction), col = room)
for (k in 1:3) {
  abline(intercept[k], slope_satisfaction[k], col = k)
}
legend("topleft", legend = c("afzonderlijk", "gedeeld", "volledig"), col = 1:3, lty = 1, bty = "n")

