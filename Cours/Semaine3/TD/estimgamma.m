%
%... generation des données
%
n= 1000;
nMC= 500;
%
%... estimateur de moments
%
halpha= zeros(nMC,1);
hbeta= zeros(nMC,1);
hmlea= zeros(nMC,1);
hmleb= zeros(nMC,1);

for i=1:nMC,
    X= gamrnd(3,0.5,n,1);
    barX= mean(X);
    barlX= mean(log(X));
    s2= var(X);
    halpha(i)= barX^2/s2;
    hbeta(i)= barX/s2;
%
%... estimateur du maximum de vraisemblance
%
    profile= @(a) -n*a^2*log(a^2)+n*a^2*log(barX)+n*log(gamma(a^2))-n*(a^2-1)*barlX +n*a^2;
    ha2= fminsearch(profile, halpha(i)^2);
    hmlea(i)= ha2^2;
    hmleb(i)= hmlea(i)/barX;
end
figure(1)
boxplot([halpha,hmlea]);
title('Paramètre \alpha')
figure(2)
boxplot([hbeta,hmleb]);
title('Paramètre \beta')
