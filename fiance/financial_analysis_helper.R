##_Solar power and financial analysis

#1.Solar radiation to power======
# convert joule to kmh (1/3600000)

radiation_to_power <- function(radiation, area, yield_r=0.175, pr=0.6, hours=8)
{ kWh <- radiation * area * yield_r * pr * hours * (1/3600000)
return(kWh) } 

#suppose we want to calculate how much power a 10m2 tiles could generate in one hour.
#and we know solar radiance 15000000

rad=18279207
area=10000
power_kWh <- radiation_to_power(rad, area) 
power_kWh

#EU defines poor regions as the the areas with  yearly solar radiance yields lower than 900 kwh
#Can you calculate if ssrd with value above is a poor area?
year_kmh <- radiation_to_power(rad, area=40000, hours=365*24) 
year_kmh

#2 Net Present Value (NPV)======
rep(10,4) #create a vector by repeating 10 4 times
# output of the function above is: 10 10 10 10
seq( 1, 11, 2) #create a sequence of data start from 1 and end at 11. 2 is the increment of the sequence.
#outout will be: 1 3 5 7 9 11

calc_NPV <- function(annual_revenue, i=0.05, lifetime_yrs, CAPEX, OPEX){
  costs_op <- rep(OPEX, lifetime_yrs) #operating cost
  revenue <- rep(annual_revenue, lifetime_yrs) 
  t <- seq(1, lifetime_yrs, 1) #output: 1, 2, 3, ...25
  gg<-sum( (revenue - costs_op)/(1 + i)**t )
  NPV <- sum( (revenue - costs_op)/(1 + i)**t ) - CAPEX
  return(round(NPV, 0))
}
#Exercise: if annual revenue is 14000000, and Capital expenditure is 150000000, then please calculate Net present value. Should we invest this project?

#103 USD/MWh (0.103USD/kWh -0.08 GBP/kWh)
4810447.422

#7.67p/kWh (£0.0767/kWh - 0.098 usd/kWh)
0.098*46703374

25*3*1.16e6
87236000

1160000*24*2 + 590*10*8*5

npv=calc_NPV(annual_revenue = 0.098*214405000000 ,OPEX= 0.098*214405000000*0.014,lifetime_yrs=25, CAPEX=(1160000*30000*8+590*30000*8*5))
npv



# annual_revenue=4576930.55726666
# i=0.05
# lifetime_yrs=25
# CAPEX=55916000
# OPEX=0
# 
# costs_op <- rep(OPEX, lifetime_yrs) #operating cost
# costs_op
# 
# t <- seq(1, lifetime_yrs, 1) #output: 1, 2, 3, ...25
# t
# revenue <- rep(annual_revenue, lifetime_yrs) 
# revenue
# gg<-(revenue - costs_op)/(1 + i)**t 
# gg
# NPV <- sum( (revenue - costs_op)/(1 + i)**t ) - CAPEX
# NPV
# 
# sum(gg)
# -CAPEX

ifelse(npv>0, "Support","obeject" )

#3 Levelized cost of electricity (LCOE)=====
#Life_span_generation_kWH is one of the required inputs to estimate the Levelized
#cost of electricity (following function)
Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.03, lifetime_yrs = 25){
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(yearly_generation_kWH/(1+discount)**t)
  return (round(L_S_G,0))
}

lsg=Life_span_generation_kWH(yearly_generation_kWH=214405000000)
lsg
LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

lcoe = LCOE(NPV=npv, Life_span_generation=lsg)
lcoe 

npv/lsg
