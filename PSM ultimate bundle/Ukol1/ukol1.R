library(MASS)
library(DescTools)
library(dunn.test)
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
# normalita?
res<-residuals(lm(jazykovy_test~kategorie))
PlotQQ(res) # podle QQ je šikmý doleva
shapiro.test(res) # test souhlasí že není normální rozdělení (p = 0.02)
boxplot(jazykovy_test~kategorie,main="vysledky testu podle socioeko statusu",col="orange")
# test variability
oneway.test(jazykovy_test~kategorie, var.equal = FALSE)
# rozptyly jsou různé (p = 0.0007558)
# nemám normální + mám víc než 2 kategorie => kruskal
kruskal.test(jazykovy_test ~ kategorie)
# Kruskal-Wallis chi-squared = 18.791, df = 4, p-value = 0.0008638
# p < alfa takže existuje rozdíl mezi alespoň dvěma skupinami




## cerealni vyrobky hodnocene v datovem sobouru UScereal rozdelit do skupin podle meritelnych parametru.
# Vyzkousejte vice deleni a vyberte to, ktere Vam prijde optimalni.
UScereal


## Vysledky ciselnych testu namerenych v datovem souboru UScereals znazornete vhodnym grafem. 
# K jeho vykresleni vyuzijte faktorovou analyzu, pripadne hlavni komponenty, pokud vhodne faktory nebude mozno vytvorit.
