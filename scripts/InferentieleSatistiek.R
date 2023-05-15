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
table(host)
part <- length(realSum[host == "enige"])

# p is zeer klein (p-value < 2.2e-16) dus significant meer dan de helft is particulier
binom.test(part, length(realSum), 0.5)


# controle als het aantal beschikbare kamers de poissonverdeling volgt
lambda <- median(bedrooms)
expected_freq <- dpois(0:5, lambda) / sum(expected_freq)
observed_freq <- table(bedrooms)

chisq.test(observed_freq, p = expected_freq) # p-value < 2.2e-16
chisq.test(observed_freq, p = expected_freq)$expected
# p-waarde is zeer klein dus de h0 wordt verworpen, het aantal beschikbare kamers volgt niet de poissonverdeling
# de expected values bij 0 zijn veel te hoog en bij 1 veel te laag, de andere waarden zijn ongeveer gelijk

#==== GEMIDDELDE KOST ====#

# verschilt de totale kost als het een maximum score heeft voor netheid
# dit zijn ongepaarde groepen
max <- realSum[cleanliness == 10]
rest <- realSum[cleanliness != 10]
mean(max) - mean(rest)
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
mean(enige) - mean(meerdere)
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
mean(volledig) - mean(afzonderlijk)
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


# is er correlatie tussen de totale kost en de variable capacity
# capacity is een ordinale veranderlijke dus met kruistabel
realSumKlassen <- cut(realSum, breaks = c(0, 200, 300, 400, 500, 600, 700, 800, 1000, 2000, 9000))
capacity2 = capacity
capacity2[capacity2==6] = 4
capacity2[capacity2==5] = 4


table(realSumKlassen, capacity2)

chisq.test(realSumKlassen, capacity2) # p-value < 2.2e-16 -> p-waarde zeer klein , zeer afhankelijk
chisq.test(realSumKlassen, capacity2)$expected
chisq.test(realSumKlassen, capacity2)$residuals # nolint


# is er correlatie tussen de totale kost en de variable bedrooms
# bedrooms is een ordinale veranderlijke en er zijn ties dus met kruistabel
realSumKlassen <- cut(realSum, breaks = c(0, 300, 400, 500, 600, 700, 800, 1000, 9000))

bedrooms2 = bedrooms
bedrooms2[bedrooms2==5] = 2
bedrooms2[bedrooms2==4] = 2
bedrooms2[bedrooms2==3] = 2

table(realSumKlassen, bedrooms2)

chisq.test(realSumKlassen, bedrooms2) # p-value < 2.2e-16 -> p-waarde zeer klein , zeer afhankelijk
chisq.test(realSumKlassen, bedrooms2)$expected
chisq.test(realSumKlassen, bedrooms2)$residuals


# is er correlatie tussen de totale kost en de variable distance
# distance is een continue veranderlijke dus eerst testen op normaliteit
shapiro.test(dist) # p-value < 2.2e-16 -> p-waarde zeer klein , niet normaal verdeeld
# dus gebruiken we de spearman correlatie
cor.test(realSum, dist, method = "spearman") # p-value < 2.2e-16 -> p-waarde zeer klein , zeer afhankelijk


# is er correlatie tussen de totale kost en de variable metro
# metro is een continue veranderlijke dus eerst testen op normaliteit
shapiro.test(metro) # p-value < 2.2e-16 -> p-waarde zeer klein , niet normaal verdeeld
# dus gebruiken we de spearman correlatie
cor.test(realSum, metro, method = "spearman") # p-value = 9.476e-10 -> p-waarde zeer klein , zeer afhankelijk


# is er correlatie tussen de totale kost en de variable attractions
# attractions is een continue veranderlijke dus eerst testen op normaliteit
shapiro.test(attr) # p-value < 2.2e-16 -> p-waarde zeer klein , niet normaal verdeeld
# dus gebruiken we de spearman correlatie
cor.test(realSum, attr, method = "spearman") # p-value < 2.2e-16 -> p-waarde zeer klein , zeer afhankelijk


# is er correlatie tussen de totale kost en de variable restaurants
# restaurants is een continue veranderlijke dus eerst testen op normaliteit
shapiro.test(rest) # p-value < 2.2e-16 -> p-waarde zeer klein , niet normaal verdeeld
# dus gebruiken we de spearman correlatie
cor.test(realSum, rest, method = "spearman") # p-value < 2.2e-16 -> p-waarde zeer klein , zeer afhankelijk


# is er correlatie tussen de totale kost en de variable host
# host is een ordinale veranderlijke en er zijn ties dus met kruistabel
realSumKlassen <- cut(realSum, breaks = c(0, 200, 300, 400, 500, 600, 700, 800, 1000, 2000, 9000))
meerdere <- as.character(host)
meerdere[meerdere=="2 tot 4"] = "meerdere"
meerdere[meerdere=="meer dan 4"] = "meerdere"

table(realSumKlassen, meerdere)

chisq.test(realSumKlassen, meerdere) # p-value = 0.0023 -> p-waarde is niet zo klein , een beetje afhankelijk
chisq.test(realSumKlassen, meerdere)$expected
chisq.test(realSumKlassen, meerdere)$residuals


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


# is er correlatie tussen de totale kost en de variable satisfaction
# satisfaction is een continue veranderlijke dus eerst testen op normaliteit
shapiro.test(satisfaction) # p-value < 2.2e-16 -> p-waarde zeer klein , niet normaal verdeeld
# dus gebruiken we de spearman correlatie
cor.test(realSum, satisfaction, method = "spearman") # p-value = 1.657e-07 -> p-waarde zeer klein , zeer afhankelijk
