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
# Určení počtu shluků 
k <- 4
plot(hc)
rect.hclust(hc, k)



truncated_row_names <- substr(rownames(cereal), 1, 7)
cereal.hc <- hclust(dist(cereal), "ave")
plot(cereal.hc, hang = -1, labels = truncated_row_names, cex = 0.5)
rect.hclust(cereal.hc, k=8)
seg <- cutree(cereal.hc, k=8)
plot(cereal$calories, cereal$sugars, col=seg, pch=19, main="Kalorie vs Cukry")
plot(cereal$calories, cereal$carbo, col=seg, pch=19, main="Kalorie vs Sacharidy")
plot(cereal$calories, cereal$fiber, col=seg, pch=19, main="Kalorie vs Vláknina")
plot(cereal$calories, cereal$fat, col=seg, pch=19, main="Kalorie vs Tuky")
pc<-prcomp(cereal,scale=T)$x
plot(pc[,1],pc[,2],col=seg,pch=19, main="Rozdělení podle dvou hlavních komponent") 
cereal.sc <- scale(cereal)
cereal.hc.sc <- hclust(dist(cereal.sc), "ave")
plot(cereal.hc.sc, hang = -1, labels = truncated_row_names, cex = 0.5)
rect.hclust(cereal.hc.sc,k=8)
seg.sc <- cutree(cereal.hc.sc, k=8)
plot(cereal$calories, cereal$sugars, col=seg.sc, pch=19, main="Kalorie vs Cukry")
plot(cereal$calories, cereal$carbo, col=seg.sc, pch=19, main="Kalorie vs Sacharidy")
plot(cereal$calories, cereal$fiber, col=seg.sc, pch=19, main="Kalorie vs Vláknina")
plot(cereal$calories, cereal$fat, col=seg.sc, pch=19, main="Kalorie vs Tuky")

## Vysledky ciselnych testu namerenych v datovem souboru UScereals znazornete vhodnym grafem. 
# K jeho vykresleni vyuzijte faktorovou analyzu, pripadne hlavni komponenty, pokud vhodne faktory nebude mozno vytvorit.
fact_anal <- factanal(~., data = cereal, factors = 3)
fact_anal
sc <- factanal(~., data = cereal, factors = 3, scores="Bartlett")$scores
plot(sc[,1],sc[,2],pch=19, main="První 2 faktory")
## vizualizace při diskretizaci proměnných (hrozný způsob diskretizace)
cereal.short <- cereal_data[, c("calories", "protein", "fat", "carbo","sugars")]
sc.sh <- factanal(~., data = cereal.short, factors = 2, scores="Bartlett")$scores
plot(sc.sh[,1],sc.sh[,2],pch=19, main="První 2 faktory")
loading_plot <- biplot(sc, sc, cex = 0.7)
library(gplots)
heatmap.2(abs(fact_anal$loadings), 
          Colv = NA, Rowv = NA, 
          col = colorRampPalette(c("white", "blue"))(100), 
          scale = "none",
          margins = c(5,10),
          main = "Heatmap zatížení proměnných na faktory",
          cexCol = 1)
