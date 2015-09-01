# on compare, pour différentes valeurs de n, les IC donnés par 2 approximations asymptotiques (2 TCL):
# - la transformée en arccosinus (de l'exercice)
# - le TCL "classique" pour \overline{X} en remplaçant la variance de X_i par \overline{X}(1-\overline{X})


xb=0.2  # moyenne observée
alpha=0.05

za=qnorm(1-alpha/2)

for (n in c(10,100,1000,10000) )
{
  cat(c(n,'\n'))
cat(c(0.5*(1+cos(acos(2*xb-1)+za/sqrt(n))), 0.5*(1+cos(acos(2*xb-1)-za/sqrt(n))) ),'\n') 
cat(c(xb-za*sqrt(xb*(1-xb)/n) , xb+za*sqrt(xb*(1-xb)/n)),'\n\n')
}

