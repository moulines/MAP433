%
%..
%
n= 10;
theta= 0.5;
pd= makedist('Binomial',n,theta);
t0= 0:0.001:0.5;
err1esp= 1-(cdf(pd,n*(t0+0.5))-cdf(pd,n*(0.5-t0)));
figure(1) % erreur de première espèce
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
% erreur de 2nde espèce
figure(2)
plot(theta,err2esp,'r-',theta,err2esp1,'g--')
xlabel('paramètre \theta')
ylabel('Erreur 2nde Espèce')
figure(3)
plot(theta,1-err2esp,'r-',theta,1-err2esp1,'g--')
xlabel('paramètre \theta')
ylabel('Puissance du test')
%
%... Erreur 1ere Espèce / Puissance
%
theta0= 0.5;
theta1= 0.75;
pd0= makedist('Binomial',n,theta0);
pd1= makedist('Binomial',n,theta1);
t0= 0:0.001:0.5;
err1= zeros(length(t0),1);
powt= zeros(length(t0),1);
for it0= 1:length(t0),
    t0cur= t0(it0);
    err1(it0)= 1-(cdf(pd0,n*(t0cur+0.5))-cdf(pd0,n*(0.5-t0cur)));
    powt(it0)= 1-(cdf(pd1,n*(t0cur+0.5))-cdf(pd1,n*(0.5-t0cur)));
end
figure(4)
plot(err1,powt,'o');
xlabel('Erreur Première Espèce')
ylabel('Puissance')