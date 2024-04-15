library(MASS)
library(DescTools)

## vysledky jazykovych testu zavisi na socioekonomickem statusu ?
# U socioekonomickeho statusu porovnavejte pouze 5 skupin vytvorenych podle pocatecni cislice promenne SES.
# Ohodnotte jak statistickou, tak vecnou vyznamnost, a to nejen celkove pro promennou SES, ale i pro rozdily 
# vsech dvojic socioekonomickych statusu.


jazykovy_test <- nlschools$lang
socioeko_status <- nlschools$SES
# vykreslení prvních deseti hodnot ať vím s čím pracuji
head(jazykovy_test, 10)
head(socioeko_status, 10)
# socioeko na kategorie
kategorie <- factor(socioeko_status, levels = levels(kategorie)[1:5])
# Kategorie jsou 10, 13, 15, 17, 18
# normalita?
res<-residuals(lm(jazykovy_test~kategorie))
PlotQQ(res) # podle QQ je šikmý doleva
shapiro.test(res) # test souhlasí že není normální rozdělení (p = 0.02)
boxplot(jazykovy_test~kategorie,main="vysledky testu podle socioeko statusu",col="orange")
# test variability
oneway.test(jazykovy_test~kategorie, var.equal = FALSE)
# rozptyly jsou různé (p = 0.0007558) => Existuje minimálně jedna dvojice se statisticky výzmannou rozdílností
TukeyHSD(aov(jazykovy_test~factor(kategorie), var.equal = FALSE))
plot(TukeyHSD(aov(jazykovy_test~factor(kategorie))))
# věcná významnost
# věcně významné jsou rozdíly mezi skupinami 10 - 15, 10 - 18



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

