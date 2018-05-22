base <- read.table("http://freakonometrics.free.fr/base-glm-act2040.txt", header=TRUE)
base2 <- read.table("http://freakonometrics.free.fr/base-pratique-act2040.txt", header=TRUE)


# Expliquer le lien entre le nombre de pages vu par un lecteur sur un service d'information en ligne et sa volont� � renouveller son abonnement

donnees <- read.table(file = 'demoLOGf.csv', sep=",",  header=F)
# X1 = nombre de pages vu par semaine en moyenne sur les 10 derni�res semaines
X1 <- donnees[,3]
# X2 = nombre de pages vu au cours de la derni�re semaine
X2 <- donnees[,4]
# Y = (1 si volont� de renouvellement de l'abonnement) (0 sinon)
Y <- donnees[,5]