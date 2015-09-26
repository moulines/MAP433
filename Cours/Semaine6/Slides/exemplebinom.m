%
%..
%
n= 10;
theta= 0.5;
pd= makedist('Binomial',n,theta);
t0= 0:0.001:0.5;
err1esp= 1-(cdf(pd,n*(t0+0.5))-cdf(pd,n*(0.5-t0)));
plot(t0,err1esp);
xlabel('valeur critique: t0')
ylabel('Erreur Première Espèce')
t0= 0.3;
err1esp= 1-(cdf(pd,n*(t0+0.5))-cdf(pd,n*(0.5-t0)));
theta= 0:0.01:1.0;
err2esp= zeros(length(theta),1);
for itheta= 1:length(theta),
    curtheta= theta(itheta);
    pd= makedist('Binomial',n,curtheta);
    err2esp(itheta)= cdf(pd,n*(t0+0.5))-cdf(pd,n*(0.5-t0));
end
t0= 0.2;
pd= makedist('Binomial',n,0.5);
err1esp1= 1-(cdf(pd,n*(t0+0.5))-cdf(pd,n*(0.5-t0)));
err2esp1= zeros(length(theta),1);
for itheta= 1:length(theta),
    curtheta= theta(itheta);
    pd= makedist('Binomial',n,curtheta);
    err2esp1(itheta)= cdf(pd,n*(t0+0.5))-cdf(pd,n*(0.5-t0));
end
plot(theta,err2esp,'r-',theta,err2esp1,'g--')
xlabel('paramètre \theta')
ylabel('Erreur 2nde Espèce')
plot(theta,1-err2esp,'r-',theta,1-err2esp1,'g--')
xlabel('paramètre \theta')
ylabel('Puissance du test')
