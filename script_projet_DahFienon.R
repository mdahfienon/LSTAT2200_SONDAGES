# libraries

require(samplingbook)
library(sampling)
library(survey)
library(ggplot2)
theme_set(new = theme_bw())
library(patchwork)
require(latex2exp)

# Data loading

data <- read.csv2("graines.csv", header = TRUE, sep = ",")

data <- transform(data, 
                  X = NULL,  
                  Poids = as.numeric(Poids), 
                  Couleur = factor(Couleur),
                  Intensite = factor(IntensitÃ.),
                  Regularity = factor(RÃ.gularitÃ.)
)

data$IntensitÃ. <- NULL
data$RÃ.gularitÃ. <- NULL

data |> head() |> knitr::kable(caption = "Extrait de la base de sondage")

# Description de la base de sondage
summary(data[,2:5]) |> knitr::kable(caption = "Statistique descriptive de la base")

# Figure 

m1 <- ggplot(data)+
  geom_bar(aes(x = Intensite, fill = Couleur), position = "fill")+
  theme(legend.position = "top")+
  labs(y = "", x = "",title = "Répartion des couleurs entre les intensités")

m2 <- ggplot(data)+
  geom_bar(aes(x = Regularity, fill = Couleur), position = "fill")+
  theme(legend.position = "top")+
  labs(y="", x = "", title = "Répartition des couleurs entre les formes")
m3 <- ggplot(data)+
  geom_bar(aes(x = Intensite, fill =Regularity ), position = "fill")+
  theme(legend.position = "top")+
  labs(y="", x = "", subtitle = "Répartition des intensités entre les formes")

p1 <- ggplot(data)+
  geom_violin(aes(x=Couleur, y = Poids, fill = Couleur, col = Couleur))+
  geom_boxplot(aes(x = Couleur, y= Poids),width = 0.25, varwidth = TRUE, outlier.shape = NA, fill = NA)+
  theme(legend.position = "top")+
  labs(x = " ", subtitle = "Densité des poids en focntion des couleurs")

p2 <- ggplot(data)+
  geom_violin(aes(x = Intensite, y= Poids, fill = Intensite, col = Intensite))+
  geom_boxplot(aes(x = Intensite, y= Poids),width = 0.25, varwidth = TRUE, outlier.shape = NA, fill = NA)+
  theme(legend.position = "top")+
  labs(subtitle = "Densité des poids en fonction de l'intensité des couleurs", x = "", y ="")

p3 <- ggplot(data, aes(x = Regularity, y = Poids))+
  geom_violin(aes(x = Regularity, y = Poids, fill = Regularity, col = Regularity ))+
  geom_boxplot(width = 0.25, varwidth = TRUE, outlier.shape = NA, fill = NA) +
  #geom_jitter(width = 0.1, height = 0, alpha = 0.2)+
  theme(legend.position = "top")+
  labs(subtitle = "Densité des poids en fonction de la forme des graines", x = "", y = "")


m1+m2+m3+p1+p2+p3 +plot_layout(nrow = 3)

# anova

v <- lm(Poids~Intensite, data = data)
anova(v) |> knitr::kable(caption = "Analyse de la variance du poids au sein de la caractéristiques Intensité")

# Echantillon_1

n <- 400
N <- dim(data)[1]
nh <- n/N *table(data$Couleur)
nh <- round(nh, 0)

set.seed(2022)
Sample <- sampling::strata(data, stratanames = c("Couleur"), size = nh, method = "srswor")
Echant1 <- sampling::getdata(data, Sample)
View(Echant1)

write.csv2(Echant1, file = "echantillon_1.csv")

# Etude échantillon_1

# Load E1

sampl1 <- read.csv("E1_Mathias_Dah_Fienon.csv", header = TRUE, sep = ",")
sampl1 <- transform(sampl1, 
                    X = NULL,  
                    Poids = as.numeric(Poids), 
                    Couleur = factor(Couleur),
                    Intensité = factor(Intensité),
                    Régularité = factor(Régularité)
)

View(sampl1)
str(sampl1)

# Répartition du prix 

summary(sampl1$Prix)

# Figure 
see <- shapiro.test(sampl1$Prix)
de <- ggplot(sampl1, aes(x = Prix))+
  geom_histogram(aes(y = ..density..) ,bins = 9, fill = "gray", col = "white")+
  geom_density(col = "red")+
  labs(subtitle = "Histogram of flowers Price\n and density curves")

qq <- ggplot(data = sampl1, aes(sample = Prix))+
  geom_qq(col = "gray")+
  geom_qq_line(col = "red")+
  labs(title = "Normality Test (Shapiro-Wilk)",
       subtitle = paste0("p-value:",round(see$p.value,11),"\nstatistics : ", 
                         round(see$statistic,3)) , x = "Theorical sample", y = "Sample quantile")

de + qq +plot_layout(nrow = 1, ncol = 2)


# Etude de la couleur
cou_fit <- lm(Prix~Couleur, sampl1)
bn <- anova(cou_fit)
cou <- ggplot(data = sampl1)+
  geom_boxplot(aes(y =Prix ,x = Couleur), varwidth = TRUE, col= c("blue", "yellow", "red"))+
  stat_summary(aes(y =Prix ,x = Couleur), fun ="mean", col= c("blue", "yellow", "red"), shape = 2)+
  labs(subtitle = paste("Egalité des moyennes :\np-value : ", round(bn$`Pr(>F)`[1],3), "\nNon Rejet égalité de moyennes "))


# Etude de l'intensité
Int_fit <- lm(Prix~Intensité, sampl1)
bn1 <- anova(Int_fit)
inte <- ggplot(data = sampl1)+
  geom_boxplot(aes(y =Prix ,x = Intensité, col = Intensité), varwidth = TRUE, show.legend = FALSE)+
  stat_summary(aes(y =Prix ,x = Intensité, col = Intensité), fun = "mean", shape = 2, show.legend = FALSE)+
  labs(subtitle = paste("Egalité des moyennes :\np-value : ", round(bn1$`Pr(>F)`[1],3), "\nRejet égalité de moyennes"), y = "")



# Etude de la régularité
reg_fit <- lm(Prix~Régularité, sampl1)
bn2 <- anova(reg_fit)
reg <- ggplot(data = sampl1)+
  geom_boxplot(aes(y =Prix ,x = Régularité, col = Régularité), varwidth = TRUE, show.legend = FALSE)+
  stat_summary(aes(y =Prix ,x = Régularité, col = Régularité), fun = "mean", shape = 2, show.legend = FALSE)+
  labs(subtitle = paste("Egalité des moyennes :\np-value : ", round(bn2$`Pr(>F)`[1],3), "\nNon Rejet égalité de moyennes"),  y = "")



cou + inte + reg + plot_layout(nrow = 2, ncol =2)

#lien entre le prix de la fleur et le poids de la graine

ok <- summary(lm(Prix~Poids, sampl1))
poi <- ggplot(sampl1, aes(x = Poids, y = Prix))+
  geom_point(col = "gray")+
  geom_smooth(aes(x = Poids, y = Prix), formula = y~x,method = "lm", se = FALSE, col = 4)+
  #facet_wrap(vars (Régularité))
  labs(subtitle = paste("R-squared =", round(ok$r.squared,3), "& corr =", round(cor(sampl1$Prix, sampl1$Poids),3)),
       title = "overall Prix vs overall Poids")

poi_in1 <- ggplot(subset(sampl1, Intensité ==  "1"), aes(x = Poids, y = Prix))+
  geom_point(alpha = 0.5, show.legend = FALSE, col = 1)+
  geom_smooth(aes(x = Poids, y = Prix), formula = y~x,method = "lm", se = FALSE, show.legend = FALSE, col = "gray")+
  #facet_wrap(vars (Intensité), labeller = "label_both")+
  labs(title = "Intensité = 1", 
       subtitle = paste("corr =", round(cor(subset(sampl1, Intensité ==  "1")$Prix, subset(sampl1, Intensité ==  "1")$Poids),3)),y="")


poi_in2 <- ggplot(subset(sampl1, Intensité ==  "2"), aes(x = Poids, y = Prix))+
  geom_point(alpha = 0.5, show.legend = FALSE, col = 2)+
  geom_smooth(aes(x = Poids, y = Prix), formula = y~x,method = "lm", se = FALSE, show.legend = FALSE, col = "gray")+
  #facet_wrap(vars (Intensité), labeller = "label_both")+
  labs(title = "Intensité = 2", 
       subtitle = paste("corr =", round(cor(subset(sampl1, Intensité ==  "2")$Prix, subset(sampl1, Intensité ==  "2")$Poids),3)),y="")

poi_in3 <- ggplot(subset(sampl1, Intensité ==  "3"), aes(x = Poids, y = Prix))+
  geom_point(alpha = 0.5, show.legend = FALSE, col = 3)+
  geom_smooth(aes(x = Poids, y = Prix), formula = y~x,method = "lm", se = FALSE, show.legend = FALSE, col = "gray")+
  #facet_wrap(vars (Intensité), labeller = "label_both")+
  labs(title = "Intensité = 3", 
       subtitle = paste("corr =", round(cor(subset(sampl1, Intensité ==  "3")$Prix, subset(sampl1, Intensité ==  "3")$Poids),3)),y="")

poi_in4 <- ggplot(subset(sampl1, Intensité ==  "4"), aes(x = Poids, y = Prix))+
  geom_point(alpha = 0.5, show.legend = FALSE, col = 4)+
  geom_smooth(aes(x = Poids, y = Prix), formula = y~x,method = "lm", se = FALSE, show.legend = FALSE, col = "gray")+
  #facet_wrap(vars (Intensité), labeller = "label_both")+
  labs(title = "Intensité = 4", 
       subtitle = paste("corr =", round(cor(subset(sampl1, Intensité ==  "4")$Prix, subset(sampl1, Intensité ==  "4")$Poids),3)),y="")

poi_in5 <- ggplot(subset(sampl1, Intensité ==  "5"), aes(x = Poids, y = Prix))+
  geom_point(alpha = 0.5, show.legend = FALSE, col = 5)+
  geom_smooth(aes(x = Poids, y = Prix), formula = y~x,method = "lm", se = FALSE, show.legend = FALSE, col = "gray")+
  #facet_wrap(vars (Intensité), labeller = "label_both")+
  labs(title = "Intensité = 5", 
       subtitle = paste("corr =", round(cor(subset(sampl1, Intensité ==  "5")$Prix, subset(sampl1, Intensité ==  "5")$Poids),3)),y="")

poi+poi_in1+poi_in2+poi_in3+poi_in4+poi_in5+plot_layout(nrow = 3, ncol = 2)

# Estimation ecart-type de l'intensité

aggregate(Prix~Intensité, sampl1, FUN = sd) |> 
  t() |> 
  knitr::kable(caption = "Ecart-type estimé du prix au sein des categories de l'intensité des graines ", align = "c")


# graines restantes après extraction de E1

data_reste <- data[!(data$Id %in% sampl1$Id),]


# Sélection Echantillon_2

set.seed(2022)
data_reste2 <- data_reste[order(data_reste$Intensité),]
e2_Nh <- as.numeric(table(data$Intensité))
e2_sh <- aggregate(Prix~Intensité, sampl1, FUN = sd)[,2]
#e2_Nh <- c(3621,10980,27308,31057,48380)
ToSample <- stratasamp(1750, e2_Nh, e2_sh, type = "opt")
Sample2_2 <- sampling::strata(data_reste2,
                              stratanames = c("Intensité"),
                              size = as.vector(ToSample[2, ]),
                              method = "srswor")
sample2 <- getdata(data_reste2, Sample2_2)
write.csv2(sample2, "echantillon_2.csv")


#Loading E2

sampl2 <- read.csv("E2_Mathias_Dah_Fienon.csv", header = TRUE, sep = ",")
sampl2 <- transform(sampl2, 
                    X = NULL,  
                    Poids = as.numeric(Poids), 
                    Couleur = factor(Couleur),
                    Intensité = factor(Intensité),
                    Régularité = factor(Régularité)
)

# subsets of sampl2

fleur_bleue <- subset(sampl2, Couleur == "Bleu")
fleur_jaune <- subset(sampl2, Couleur == "Jaune")
fleur_rouge <- subset(sampl2, Couleur == "Rouge")
intensity1 <- subset(sampl2, Intensité == 1)
intensity2 <- subset(sampl2, Intensité == 2)
intensity3 <- subset(sampl2, Intensité == 3)
intensity4 <- subset(sampl2, Intensité == 4)
intensity5 <- subset(sampl2, Intensité == 5)
regularity1 <- subset(sampl2, Régularité == 1)
regularity2 <- subset(sampl2, Régularité == 2)
regularity3 <- subset(sampl2, Régularité == 3)


# Etude E2

# Analyse descriptive

options(knitr.kable.NA = '')
summary(subset(sampl2, select = -Id)) |> 
  #round(2) |> 
  knitr::kable(caption = "Satistique description de Echantillon_2")

# Comparaison Moy_E1 et Moy_E2

round(t.test(sampl1$Prix, sampl2$Prix)$p.value,3)



# Distribution du prix des fleurs en fonction des caractéristiques des graines

voyon <- anova(lm(Prix~Couleur, sampl2))
voyon1 <- anova(lm(Prix~Régularité, sampl2))
voyon2 <- anova(lm(Prix~Intensité, sampl2))

s0 <- ggplot(sampl2)+
  geom_density(aes(x = Prix), col = 2, fill = "orange", alpha = 0.1, size = 0.3)+
  theme(legend.position = c(0.78, 0.65))+
  labs(subtitle = "Densité du prix au sein de Echantillon_2",
       y = "")



s1 <- ggplot(sampl2)+
  geom_density(aes(x = Prix, col = Intensité),  alpha = 0.1, size = 0.3)+
  theme(legend.position = c(0.78, 0.65))+
  labs(subtitle = "Densité du prix des fleurs en fonction de l'intensité\n de la graine",
       y = "")

s2 <- ggplot(sampl2)+
  geom_density(aes( x = Prix, col = Couleur), alpha = 0.1, size = 0.3)+
  theme(legend.position = c(0.78, 0.65))+
  labs(title = "Densité du prix des fleurs en fonction\n des couleurs de la graine",
       y = "")

s3 <- ggplot(sampl2)+
  geom_density(aes( x = Prix, col = Régularité), alpha = 0.1, size = 0.3)+
  theme(legend.position = c(0.78, 0.65))+
  labs(title = "Densité du prix des fleurs en fonction\n de la régularité de la graine",
       y = "")


a1 <- ggplot(sampl2)+
  geom_boxplot(aes(x = Couleur,y = Prix, col = Couleur), varwidth = TRUE, show.legend = FALSE)+
  stat_summary(aes(x = Couleur,y = Prix, col = Couleur), fun = "mean", shape = 1, show.legend = FALSE)+
  annotate("text", label = TeX(paste0("$mu_b$ = ", round(mean(fleur_bleue$Prix),2))), x = 1 , y =3)+
  annotate("text", label = TeX(paste0("$mu_j$ = ", round(mean(fleur_jaune$Prix),2))), x = 2 , y =3)+
  annotate("text", label = TeX(paste0("$mu_r$ = ", round(mean(fleur_rouge$Prix),2))), x = 3 , y =3)+
  labs(title = TeX(paste0("$mu_b=mu_j=mu_r$ ?")),
       subtitle = paste0("p-value of anova = ", round(voyon$`Pr(>F)`[1],7), "\nDécision : Rejet de l'hypothèse d'égalité\n des moyennes"))


a2 <- ggplot(sampl2)+
  geom_boxplot(aes(x = Régularité,y = Prix, col =Régularité), varwidth = TRUE, show.legend = FALSE, xlim = c(0, 5))+
  stat_summary(aes(x = Régularité,y = Prix, col =Régularité), fun = "mean", shape = 1, show.legend = FALSE)+
  annotate("text", label = TeX(paste0("$mu_1$ = ", round(mean(regularity1$Prix),2))), x = 1 , y =3)+
  annotate("text", label = TeX(paste0("$mu_2$ = ", round(mean(regularity2$Prix),2))), x = 2 , y =3)+
  annotate("text", label = TeX(paste0("$mu_3$ = ", round(mean(regularity3$Prix),2))), x = 3 , y =3)+
  #annotate("text", x = 3.5, y = 125, label = )+
  labs(title = TeX(paste0("$mu_1=mu_2=mu_3$ ?")),
       subtitle = paste0("p-value of anova = ", round(voyon1$`Pr(>F)`[1],3), "\nDécision : Non rejet de l'hypothèse d'égalité\n des moyennes"))

a3 <- ggplot(sampl2)+
  geom_boxplot(aes(x = Intensité,y = Prix, col =Intensité), varwidth = TRUE, show.legend = FALSE, xlim = c(0, 5))+
  stat_summary(aes(x = Intensité,y = Prix, col =Intensité), fun = "mean", shape = 1, show.legend = FALSE)+
  annotate("text", label = TeX(paste0("$mu_1$ = ", round(mean(intensity1$Prix),2))), x = 1 , y =3)+
  annotate("text", label = TeX(paste0("$mu_2$ = ", round(mean(intensity2$Prix),2))), x = 2 , y =3)+
  annotate("text", label = TeX(paste0("$mu_3$ = ", round(mean(intensity3$Prix),2))), x = 3 , y =3)+
  annotate("text", label = TeX(paste0("$mu_4$ = ", round(mean(intensity4$Prix),2))), x = 4 , y =3)+
  annotate("text", label = TeX(paste0("$mu_5$ = ", round(mean(intensity5$Prix),2))), x = 5 , y =3)+
  labs(title = TeX(paste0("$mu_1=mu_2=mu_3=mu_4=mu_5$ ?")),
       subtitle = paste0("p-value of anova = ", round(voyon2$`Pr(>F)`[1],7), "\nDécision : Rejet de l'hypothèse d'égalité\n des moyennes"))

s0+s1+s2+s3+a1+a2+plot_layout(nrow = 3)


a5 <- ggplot(sampl2)+
  geom_point(aes(x = Poids, y = Prix, col = Intensité, shape = Couleur), size = 3)+
  labs(subtitle = "Lien entre Prix de la fleur et Poids de la graine")+
  annotate("text", 
           label = "italic(R^2-adj)== 0.781",
           parse = TRUE,
           x = 0.5, y = 150)+
  annotate("text", 
           label = "italic(corr)== 0.883",
           parse = TRUE,
           x = 0.57, y = 140)


a3+a5+plot_layout(nrow = 2, ncol = 1)

linear2 <- summary(lm(Prix~Poids, sampl2))


# Estimation des paramètres 

plan_sonda <- svydesign(ids = ~1, strata = ~Intensité, fpc =~sample2$Prob , data = sampl2)

svymean(~Prix, design = plan_sonda2, deff = FALSE)


# Redressement 

Effectifs <- data.frame(
  "Intensité" = 1:5,
  "Freq" = as.numeric(table(data_reste2$Intensité))
)


poststrata <- postStratify(
  design = plan_sonda,
  strata = ~Intensité, population = Effectifs
)

# Sous-groupe - graines jaunes & intensité in 4,5

graines_jaunes <- subset(sampl2, Couleur == "Jaune" & (Intensité == "4" | Intensité == "5") )


mean(graines_jaunes$Prix)

sd(graines_jaunes$Prix)
























