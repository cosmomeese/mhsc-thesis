

# Euler Project 1
mults <- vector()
for(n in 1:999)
{
  if((0 == n %% 5) || (0 == n %% 3))
  {
    mults <- c(mults, n)
  }
}
result <- sum(mults)

# Advent of Code 1
captcha <- c(1,1,2,2)
captcha <- c(1,1,1,1)
captcha <- c(1,2,3,4)
captcha <- c(9,1,2,1,2,1,2,9)

## start
lastDigit <- captcha[length(captcha)]
sum <- 0
for(n in captcha)
{
  if(lastDigit == n)
  {
    sum <- sum + n
  }
  lastDigit <- n
}
sum

# Advent of Code 2

mat <- matrix(c(5, 1, 9, 5, 7, 5, 3, NA, 2, 4, 6, 8),
              nrow=3,
              ncol=4,
              byrow=TRUE)

# can't do if entire row is empty
mat <- matrix(c(5, 1, 9, 5, 1, 1, 1, 1, 2, 4, 6, 8),
              nrow=3,
              ncol=4,
              byrow=TRUE)

perRow <- apply(mat,
                MARGIN=1,
                function(x)
                {
                  diff = max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
                })
checksum <- sum(perRow)


# Advent of Code 3
inputNumber <- 1
# couldn't get it: https://blog.balthazar-rouberol.com/solution-to-advent-of-code-day-3-spiral-memory.html


# Advent of Code 4
passphrase <- "aa bb cc dd ee"
passphrase <- "aa bb cc dd aa"
passphrase <- "aa bb cc dd aaa"

s <- unlist(strsplit(passphrase, " ", fixed = TRUE))
if(identical(s,unique(s)))
{
  print('valid')
  # valid passphrase
}

# Advent of Code 5

instructions <- c(0, 3, 0, 1, -3)

steps <- 0
idx <- 1
while(idx >= 1 && idx <= length(instructions))
{
  nextIdx <- idx + instructions[[idx]]
  instructions[[idx]] <- instructions[[idx]] + 1
  idx <- nextIdx
  steps <- steps + 1
}
steps

# Advent of Code 6

