# Data frame � partir d'un fichier csv
ozone=read.csv2("ozone.csv")
# v�rification
summary(ozone)
# Supprimer la variable inutile "obs"
ozone=ozone[,-1]
sapply(ozone, mean) # moyennes
sapply(ozone, sd) # �carts-types
boxplot(ozone[,2:4]) # bo�tes par groupe
boxplot(ozone[,5:7])
boxplot(ozone[,8:10])
boxplot(ozone[,c(1,11)])
hist(ozone$maxO3)
hist(ozone$maxO3v)
hist(log(ozone$maxO3))
hist(log(ozone$maxO3v))
boxplot(log(ozone[,c(1,11)]))
hist(ozone$maxO3)
hist(ozone$maxO3v)
hist(log(ozone$maxO3))
hist(log(ozone$maxO3v))
boxplot(log(ozone[,c(1,11)]))
ozone=data.frame(ozone,LmaxO3=log(ozone$maxO3),
                 LmaxO3v=log(ozone$maxO3v))
summary(ozone)
# estimation
res2.reg=lm(LmaxO3 ~ LmaxO3v+T9+T12+T15+Ne9+Ne12+
              Ne15+Vx9+Vx12+Vx15, data = ozone)
# diagnostics
plot(res2.reg)
# r�sultats
summary(res2.reg)


