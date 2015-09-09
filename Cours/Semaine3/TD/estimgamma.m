%
%... generation des données
%
n= 100;
nMC= 5000;
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
%    profile= @(a) -n*a^2*log(a.^2)+n*a^2*log(barX)+n*log(gamma(a.^2))-n*(a.^2-1)*barlX +n*a^2;
%    ha2= fminsearch(profile, sqrt(halpha(i))); ha=ha2^2;
     profile= @(a) -n*a*log(a)+n*a*log(barX)+n*log(gamma(a))-n*(a-1)*barlX +n*a;
     ha= fminsearch(profile, halpha(i)); 
    hmlea(i)= ha;
    hmleb(i)= hmlea(i)/barX;
end

figure(1)
subplot(121)
boxplot([halpha,hmlea]);
title('Paramètre \alpha')
subplot(122)
boxplot([hbeta,hmleb]);
title('Paramètre \beta')
