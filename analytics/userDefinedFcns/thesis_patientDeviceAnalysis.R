# Test if proportion of compliant patients is different than original Medly Study (doi: 10.2196/jmir.1909)

#pkg <- c(pkg,"httr")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)

library(pwr)

## START

# 7 months

totalSeto <- 50 # (all patients in seto study)
setoCompliance <- c(42,33,16) # compliance @ 50, 80, 95% respectively

total <- 44 # (all patients)
compliance <- c(22,17,7) # compliance @ 50, 80, 95% respectively

mdat <- matrix(c(compliance, 
                 setoCompliance), # seto
              byrow=FALSE,
              ncol=2)
n <- c(total,totalSeto)

# Chi-squared test
apply(mdat, MARGIN=1, prop.test, n=n, p=NULL, alternative = "two.sided", conf.level = 0.95)

# 3 months
total <- 26
compliance <- c(13,10,7)
mdat <- matrix(c(compliance, 
                 setoCompliance), # seto
               byrow=FALSE,
               ncol=2)
n <- c(total,totalSeto)

# Chi-squared test
apply(mdat, MARGIN=1, prop.test, n=n, p=NULL, alternative = "two.sided", conf.level = 0.95)