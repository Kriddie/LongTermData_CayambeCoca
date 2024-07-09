#k600 shouldn't change too much from day to day
#even if you do not pool and the k600 seems stable, then maybe you are ok! (and not pooling is faster)
#but if you are not pooling and k600 is all over the place, you may need to pool 
#you can do full pooling, but maye partial pooling is better because that allows for some variability between 

#model names

"b_kb_oipi_tr_plrckm"

#1st, b = baysian
#second refers to k600, lots of paper use "kb" or partial pooling
#third is errors; use both process and observational errors: "oipi"
#fourth is is the way that they calculate the metabolism, maite uses trapazoidal: "tr"
#fith has to do with 

# also do you want constant respiration or to vary with temperature
      #maite has constant respiration throughout the day


#bernharts 2022, allison appling
#look at methods in maite's paper to see what to use for your models and what to play around with 
    #"metabolis regime...."



# maybe maniptulate how many node sensor in k600/discharge relationship
#there are default priors for this but maite changes for her models
#there is an equation that bob hall has given maite that she uses



#Sensativity test? 
    #then you can play with priors


###how to evaluate?
  #R hat associated with entire model - R hat for process error (you want lower than 1.1)

#check relationship between k600 and ER -- you don't want a strong relationship!!

#range of variability in k600 (shouldn't be too variable!)
#reltationship between k600 and discharge (shouldn't be too strong!)
