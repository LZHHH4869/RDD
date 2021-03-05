capture log close
set obs 50000
set seed 55

* Generate running variables.
gen x = 10 + rnormal(10,5)
gen x2 = x*x
gen x3 = x*x*x

* Treatment
gen d = 0
replace d = 1 if x>=20

* Outcome
gen quantity = 5005 + 750*d + 2*x + 0.75*x2 - 0.05*x3 + rnormal(0,1000)

* Visualizing conditional means using cmogram
cmogram quantity x, cut(20) scatter line(20) lfit
cmogram quantity x, cut(20) scatter line(20) qfit

* Using different ranges
reg quantity d if x>=15 & x<=25, robust
reg quantity d if x>=17 & x<=23, robust
reg quantity d if x>=18 & x<=22, robust

gen x_c = x-20
gen x_c2=x_c^2
gen x_c3=x_c*x_c*x_c

* Interact the treatment with the re-centered running variable.
xi: reg quantity i.d*x_c, robust
xi: reg quantity d##(c.x_c c.x_c2), robust
xi: reg quantity d##(c.x_c c.x_c2 c.x_c3), robust

rddensity x, c(20)
rdrobust quantity x, c(20)


