# Test if proportion of Android vs iPhone patients is different than expected

pkg <- c("pwr")
#pkg <- c(pkg,"httr")
new.pkg <- pkg[!(pkg %in% installed.packages())]

if (length(new.pkg)) {
  install.packages(new.pkg)
}
rm(pkg,new.pkg)

library(pwr)

## START

total <- 37 # (only new patients)
total <- 44 # (all patients)
countOfiPhones <- 12
expectedProportionOfiPhones <- 0.438
expectediPhones <- round(total*expectedProportionOfiPhones)

x <-  c(countOfiPhones,expectediPhones) # count of successes iPhone
n <- c(total,total)

# Chi-squared test
res <- prop.test(x, n, p = NULL, alternative = "two.sided", conf.level = 0.95)
res


# Fisher Test
mat <- matrix(c(countOfIPhones,
                total-countOfIPhones,
                expectediPhones,
                total-expectediPhones),
              ncol=2)

res.fisher <- fisher.test(mat)
res.fisher

# Power analysis

effectSize <- c(0.2,0.5,0.8)
lapply(effectSize, FUN=function(x) {pwr.p.test(h = x, n=total, sig.level = 0.95, alternative='two.sided')})
