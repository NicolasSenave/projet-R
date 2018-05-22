
# Expliquer le lien entre le nombre de pages vu par un lecteur sur un service d'information en ligne et sa volonté à renouveller son abonnement

donnees <- read.table(file = 'demoLOGf.csv', sep=",",  header=F)
# X1 = nombre de pages vu par semaine en moyenne sur les 10 dernières semaines
X1 <- donnees[,3]
# X2 = nombre de pages vu au cours de la dernière semaine
X2 <- donnees[,4]
# Y = (1 si volonté de renouvellement de l'abonnement) (0 sinon)
Y <- donnees[,5]
