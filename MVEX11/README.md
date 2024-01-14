# MVEX11-VT23-09
Kod som användes i kandidatarbetet, kurs MVEX11-VT23-09. Vissa av filerna 
är givna och enbart modifierade medans andra är skapade i projektet. 
Se README för ytterligare instuktioner. 

Nedan kommer en beskrivning för test-filerna, vilka är de som ska köras.
Resterande filer är funktionsfiler som används i testfilerna. I filerna 
motsvarar alpha_unknown=1 d_m1, alpha_unknown=2 d_m2, alpha_unknown=3 a_t1, 
alpha_unknown=4 a_t2 och alpha_unknown=5 k_12. 

test_variation_colour.m bygger på given kod men delen %% Variations in 
observations är skapat i projektet. Inskrivna max och min punkter för 
respektive parameter ytterpunkterna på dess estimerade parameterintervall. 

test_alpha_exp.m är de explicita beräkningarna av en parameter med den
ursprungliga modellen och inte den förensklade. Det går att välja om 
resultatet ska presenteras i vanlig eller logaritmerad skala. Det går också
att välja vilken lösare som ska användas. Antingen ode23s, ode45 eller Newton. 
Första delen av projektet är skapad i projektet medan den nedre delen, 
skalningen, är tagen från given kod. 

test3.m är given och visar lösningen av modellen med och utan tillagt
brus i figur 1. Figur 2 visar lösningen för de olika densiteterna. Koden är given och har inte skapats i projektet. 

test2.m var redan given jämför lösningen av ode45 och Newton, bakåt och framåt. Koden är given och har inte skapats i projektet.

test2_Chebyshew.m jämför lösningen av ode45 och Newton, bakåt och framåt, 
efter tillämpning av Chebyshew noder. Grunden är test2.m som sedan 
modifierades i projektet. 

test.m är given men modifierad då filen inte fungerade och genererade error. 
Figur 1 visar lösningen med och utan  brus. Figur 2 visar framåtlösningen av ode45 
och Newton. Figur 3 visar bakåtlösningen av ode45 och Newton. 

mod_test_alpha_exp_delar.m jämför lösningen av den explicita beräkningen för 
när delar av den förenklade modellen tagits bort. Detta gjordes för att 
undersöka om avvikeler av modellen orsakar i synnerhet av en viss del av 
ekvationerna. I fugur 2 visar genererade denisiteterna som också använts för de explicita 
beräkningarna.  Förutom skaliningen som är tagen från givet script har all
kod i filen skapats i projektet. 
 
mod_test_alpha_exp_Chebyshew.m genererar plottar för de explicita 
beräkningarna efter tillämpning av Chebyshew noder. Plottarna visar resultatet 
av olika lösare, skillnaden mellan förenklade verionen av modellen och orginal 
modellen i vanlig samt logaritmisk skala. I fugur 2 visar genererade 
denisiteterna som också använts för de explicita beräkningarna.Förutom 
skaliningen som är tagen från givet script har all kod i filen skapats i projektet.

mod_test_alpha_exp_Chebyshew_plot.m är en anpassad verion för att användas 
i rapporten av mod_test_alpha_exp_Chebyshew.m. 

mod_test_alpha_exp.m genererar plottar för explicita beräkningen för 
orginalmodellen samt förenklade modellen, löst med ode45,ode23s och Newton. 
I fugur 2 visar genererade denisiteterna som också använts för de explicita 
beräkningarna.Förutom skaliningen som är tagen från givet script har all 
kod i filen skapats i projektet.

mod_test1_alpha_exp_ode23s.m är en anpassad version av mod_test1_alpha_exp.m 
för att användas i rapporten. 

Create_adjoint_figure.m gernererar figurerna för Newtons metod i rapporten.
Filen bygger på given kod som modifierats. Om denna ändras glöm inte att 
ändra observationsintervallet så det matchar! För att generera samma plottar som i rapporten sätt:
obs_start = 2.1 för tidsintervall 0 till 20.
obs_start = 5 och intervallet är 5 till 20. 

leastsquare.m är polynomregression vilket används för att skapa ett polynom
 från diskreta datapunkter. Koden är skapad i projektet. 