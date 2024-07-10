library(MASS)
library(DescTools)
library(effsize)


## vysledky jazykovych testu zavisi na socioekonomickem statusu ?
# U socioekonomickeho statusu porovnavejte pouze 5 skupin vytvorenych podle pocatecni cislice promenne SES.
# Ohodnotte jak statistickou, tak vecnou vyznamnost, a to nejen celkove pro promennou SES, ale i pro rozdily 
# vsech dvojic socioekonomickych statusu.


jazykovy_test <- nlschools$lang
socioeko_status <- nlschools$SES
# vykreslení prvních deseti hodnot ať vím s čím pracuji
head(jazykovy_test, 10)
head(socioeko_status, 20)
# socioeko na kategorie
prvni_cislice <- as.integer(substr(socioeko_status, 1, 1))
kategorie <- factor(prvni_cislice, levels = 1:5)


# Chci porovnat statisticky významný rozdíl skupin = ANOVA nebo něco podobného
# Potřebuju zjistit - normalita reziduí, shodnost rozptylů

# normalita reziduí?
res<-residuals(lm(jazykovy_test~kategorie))
PlotQQ(res) # podle QQ je šikmý doleva
shapiro.test(res) # test souhlasí že není v žádném případě normální rozdělení (p = 1.294e-15)
# Normalita = NE => Kruskal

boxplot(jazykovy_test~kategorie,main="vysledky testu podle socioeko statusu",col="orange")

kruskal.test(jazykovy_test ~ kategorie) # průměry se liší
# Které skupiny se liší?
DunnTest(jazykovy_test~kategorie)
# Liší se všechny skupiny

# Kruskal nepotřebuje test rozptylu, ale protože je žádán tak ho sem dám
bartlett.test(jazykovy_test~kategorie)
#  p-value = 1.03e-05 => rozptyl nejsou stejné

# Věcná významnost
# Budu kontrolovat všechny páry cohenovým D
cohen.d(jazykovy_test ~ factor(kategorie, levels = c(levels(kategorie)[1], levels(kategorie)[2]))) # mid
cohen.d(jazykovy_test ~ factor(kategorie, levels = c(levels(kategorie)[1], levels(kategorie)[3]))) # mid
cohen.d(jazykovy_test ~ factor(kategorie, levels = c(levels(kategorie)[1], levels(kategorie)[4]))) # large
cohen.d(jazykovy_test ~ factor(kategorie, levels = c(levels(kategorie)[1], levels(kategorie)[5]))) # large
cohen.d(jazykovy_test ~ factor(kategorie, levels = c(levels(kategorie)[2], levels(kategorie)[3]))) # small
cohen.d(jazykovy_test ~ factor(kategorie, levels = c(levels(kategorie)[2], levels(kategorie)[4]))) # mid
cohen.d(jazykovy_test ~ factor(kategorie, levels = c(levels(kategorie)[2], levels(kategorie)[5]))) # mid
cohen.d(jazykovy_test ~ factor(kategorie, levels = c(levels(kategorie)[3], levels(kategorie)[4]))) # small
cohen.d(jazykovy_test ~ factor(kategorie, levels = c(levels(kategorie)[3], levels(kategorie)[5]))) # mid
cohen.d(jazykovy_test ~ factor(kategorie, levels = c(levels(kategorie)[4], levels(kategorie)[5]))) # small



## cerealni vyrobky hodnocene v datovem sobouru UScereal rozdelit do skupin podle meritelnych parametru.
# Vyzkousejte vice deleni a vyberte to, ktere Vam prijde optimalni.
UScereal

cereal_data <- UScereal
cereal_data <- subset(cereal_data, select = c("calories", "protein", "fat", "sodium", "fibre", "carbo", "sugars", "potassium"))
cereal_data

hc <- hclust(dist(cereal_data))
plot(hc, hang = -1)
# vidím outliery který jsou specialozovany na nějakou proměnnou a rozhodnul jsem se jich zbavit pro tuto analýzu
cereal_data <- cereal_data[!(rownames(cereal_data) %in% c("Grape-Nuts", "All-Bran", "100% Bran", "All-Bran with Extra Fiber")), ]
hc <- hclust(dist(cereal_data))
plot(hc, hang = -1)

# Dává mi smysl 5 shluků podle dendodramu po odstraneni outlieru
k <- 5
plot(hc)
rect.hclust(hc, k)




cereal_data$cluster <- cutree(hc, k)
cluster_means <- aggregate(. ~ cluster, data = cereal_data, FUN = mean)
print(cluster_means)
# Skupina 1 = 
# Skupina 2 = 
# Skupina 3 = nejvíc kalorií, proteinu, tuku, fibre, carbo, sugars and potassium
# Skupina 4 = nejvíc sodium
# Skupina 5 = 




# Protože jde o dataset kde má vliv hodně proměnných najednou mi nedává smysl použít k-means nebo jiný dvoudimensionální
# přístup.





## Vysledky ciselnych testu namerenych v datovem souboru UScereals znazornete vhodnym grafem. 
# K jeho vykresleni vyuzijte faktorovou analyzu, pripadne hlavni komponenty, pokud vhodne faktory nebude mozno vytvorit.
fact_anal <- factanal(~., data = cereal_data, factors = 4,scores = "Bartlett")
fact_anal
# Závěr
#Faktor 1: Nutriční hodnoty

#Tento faktor má vysoké váhy pro vlákninu (fibre), draslík (potassium) a bílkoviny (protein).
#Může představovat zdravé složky v cereáliích, které jsou bohaté na vlákninu, bílkoviny a minerály.
#Faktor 2: Energetické složky

#Tento faktor má vysoké váhy pro kalorie (calories) a tuky (fat).
#Může indikovat energetickou hustotu cereálií, kde vyšší hodnoty mohou znamenat větší kalorickou hodnotu a obsah tuků.
#Faktor 3: Složení uhlohydrátů

#Tento faktor má vysoké váhy pro sacharidy (carbo) a v mírnější míře i pro sodík (sodium).
#Naznačuje, že cereálie s vyšším obsahem sacharidů mohou být spojeny s vyšším obsahem sodíku.
#Faktor 4: Obsah cukru

#Tento faktor má vysokou váhu pro cukry (sugars).
#Ukazuje, že cukry jsou samostatnou složkou, která může být v cereáliích přítomna nezávisle na ostatních nutričních hodnotách.



par(mfrow = c(1, 3))  # Rozmístění grafů do jednoho řádku a tří sloupců

# Graf Faktor 1 vs. Faktor 2
plot(fact_anal$scores[,1], fact_anal$scores[,2], 
     xlab = "Faktor 1 (Nutriční hodnoty)", ylab = "Faktor 2 (Energetické složky)",
     main = "Faktorové skóry cereálií - F1 vs. F2")

# Graf Faktor 1 vs. Faktor 3
plot(fact_anal$scores[,1], fact_anal$scores[,3], 
     xlab = "Faktor 1 (Nutriční hodnoty)", ylab = "Faktor 3 (Složení uhlohydrátů)",
     main = "Faktorové skóry cereálií - F1 vs. F3")

# Graf Faktor 2 vs. Faktor 3
plot(fact_anal$scores[,2], fact_anal$scores[,3], 
     xlab = "Faktor 2 (Energetické složky)", ylab = "Faktor 3 (Složení uhlohydrátů)",
     main = "Faktorové skóry cereálií - F2 vs. F3")