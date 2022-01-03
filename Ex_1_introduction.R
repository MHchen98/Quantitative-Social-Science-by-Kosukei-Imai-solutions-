# Exercise 1 - Bias in turnout

data <- read.csv("turnout.csv")

## Q1
dim(data) # check the dimension of the data
names(data) # check the variables' name in the data
length(data) # the number of columns

print(data$year) # check a specific column by calling the column name
print(max(data$year) - min(data$year))
summary(data) # description of each column

## Q2

# Solution 1: add a new column
data$vap_rates <- 100*(data$total/(data$VAP+data$overseas))
data$vep_rates <- 100*(data$total/(data$VEP))
data[c("year", "vap_rates", "vep_rates")]

# Solution 2: assign new variables to a vector
VAPtr <- turnout$total/(turnout$VAP + turnout$overseas) * 100
names(VAPtr) <- turnout$year
VAPtr

VEPtr <- turnout$total/turnout$VEP * 100
names(VEPtr) <- turnout$year
VEPtr

## Q3

# Solution 1
print(summary(data$ANES - data$vap_rates))
print(summary(data$ANES - data$vep_rates))

# Solution 2
# ANES vs. VAP

diffVAP <- data$ANES - VAPtr
mean(diffVAP)
range(diffVAP)

# ANES vs. VEP
diffVEP <- data$ANES - VEPtr
mean(diffVEP)
range(diffVEP)

## Q4
# Solution 1
diff1 <- mean(midterm$vep_rates) - mean(midterm$ANES)
diff2 <- mean(pre$vep_rates) - mean(pre$ANES)
diff1; diff2

# Solution 2

n.obs <- nrow(data)
## presidential
pdiffVEP <- diffVEP[c(seq(from = 1, to = n.obs, by = 2), 14)]

## midterm (1982 - 2002)
mdiffVEP <- diffVEP[seq(from = 2, to = n.obs - 1, by = 2)]

## comparison
summary(pdiffVEP)
summary(mdiffVEP)

# we can also graph the difference

midterm <- subset(data, year%%4 != 0)
pre <- subset(data, year%%4 == 0)

par(mfrow=c(1,2))

#graphing midterm election

plot(midterm$year, midterm$vep_rates, main = "midterm election", 
     col=1, type="b", xlim=c(1980, 2008), ylim=c(0, 100), xlab = "Year", ylab="Voting Rates", xaxt ="n")
axis(side=1, at=seq(from=1980, to=2008, by=4))
lines(midterm$year, midterm$ANES, col=2, type="b")
legend("topright", pch=c(15,15), legend=c("VEP", "ANES"), col=c(1, 2), bty="n")

#graphing president election

plot(pre$year, pre$vep_rates, main = "president election", 
     col=1, type="b", xlim=c(1980, 2008), ylim=c(0, 100), xlab = "Year", ylab="Voting Rates", xaxt ="n")
axis(side=1, at=seq(from=1980, to=2008, by=4))
lines(pre$year, pre$ANES, col=2, type="b")
legend("topright", pch=c(15,15), legend=c("VEP", "ANES"), col=c(1, 2), bty="n")

## Q5
# Solution 1

p_1 <- subset(data, data$year < 1980 + ((max(data$year) - min(data$year))/2))
p_2 <- subset(data, data$year >= 1980 + ((max(data$year) - min(data$year))/2))

diff1 <- mean(p_1$vep_rates) - mean(p_1$ANES)
diff2 <- mean(p_2$vep_rates) - mean(p_2$ANES)

diff1; diff2

# Solution 2

earlier.diffVEP <- diffVEP[1:(n.obs/2)] # first half
later.diffVEP <- diffVEP[(n.obs/2 + 1):n.obs] # second half

## comparison
summary(earlier.diffVEP)
summary(later.diffVEP)

# we can also graph the difference
par(mfrow=c(1,1))

plot(p_1$year, p_1$ANES - p_1$vep_rates, main = "Coparing VEP and ANES", 
     col=1, type="b", xlim=c(1980, 2008), ylim=c(0, 40), 
     xlab = "Year", ylab="Difference between ANES and VEP", xaxt ="n")
axis(side=1, at=seq(from=1980, to=2008, by=2))
lines(p_2$year, p_2$ANES - p_2$vep_rates, col=2, type="b")
legend("topright", pch=c(15,15), legend=c("Difference between 1980-1992", "Difference between 1994-2008"), col=c(1, 2), bty="n")


# Q6
# Solution 1

year2008 <- subset(data, data$year == 2008)
adjust_vap <- (year2008$VAP - year2008$felons - year2008$noncit)
year2008$adjust_vap_rates <- 100*((year2008$total - year2008$osvoters)/adjust_vap)

adj.diff <- year2008$ANES - year2008$adjust_vap_rates

diff.vap <- year2008$ANES - year2008$vap_rates
diff.vep <- year2008$ANES - year2008$vep_rates

adj.diff
diff.vap
diff.vep

# Exercise 2 - Population

#Preparing data
kenya <- read.csv("Kenya.csv")
sweden <- read.csv("Sweden.csv")
world <- read.csv("World.csv")

# Q1
# Solution 1

sweden$py.total <- sweden$py.men + sweden$py.women
kenya$py.total <- kenya$py.men + kenya$py.women
world$py.total <- world$py.men + world$py.women

## calculate the CBR
sweden.CBR <- c(sum(sweden$births[1:15]) / sum(sweden$py.total[1:15]),
                sum(sweden$births[16:30]) / sum(sweden$py.total[16:30]))
kenya.CBR <- c(sum(kenya$births[1:15]) / sum(kenya$py.total[1:15]),
               sum(kenya$births[16:30]) / sum(kenya$py.total[16:30]))
world.CBR <- c(sum(world$births[1:15]) / sum(world$py.total[1:15]),
               sum(world$births[16:30]) / sum(world$py.total[16:30]))

## assign labels
names(sweden.CBR) <- c("1950-1955", "2005-2010")
names(kenya.CBR) <- c("1950-1955", "2005-2010")
names(world.CBR) <- c("1950-1955", "2005-2010")

## alternative, more efficient way to assign the same labels
names(sweden.CBR) <- names(kenya.CBR) <- names(world.CBR) <-
  c("1950-1955", "2005-2010")

## display results
sweden.CBR
kenya.CBR
world.CBR


# Solution 2

# create a function for repeating calculation
cbr <- function(x, p){
  x<-filter(x, period == p)
  CBR <- sum(x$births)/sum(x$py.men+x$py.women)
  return(round(CBR,4))
}

kenya_cbr <- c(cbr(kenya, "1950-1955"), cbr(kenya, "2005-2010"))
sweden_cbr <- c(cbr(sweden, "1950-1955"), cbr(sweden, "2005-2010"))
world_cbr <- c(cbr(world, "1950-1955"), cbr(world, "2005-2010"))

options(digits = 3) # Retain 3 decimal places
cat('     ', paste("1950-1955", "2005-2010"), '\n',
    ' Kenya:', paste(kenya_cbr, sep=' '), '\n',
    'Sweden:', paste(sweden_cbr, sep=' '), '\n',
    'World:', paste(world_cbr, sep=' '))


# Q2. Calculating ASFR

asfr <- function(x, p){
  x<-filter(x, period == p) 
  ASFR <- x$births/x$py.women
  return(ASFR)
  
}

kenya$asfr <- c(asfr(kenya, "1950-1955"), asfr(kenya, "2005-2010"))
sweden$asfr <- c(asfr(sweden, "1950-1955"), asfr(sweden, "2005-2010"))
world$asfr <- c(asfr(world, "1950-1955"), asfr(world, "2005-2010"))

kenya[c("age", "asfr")][c(4:10, 19:25), ]

sweden[c("age", "asfr")][c(4:10, 19:25), ]

world[c("age", "asfr")][c(4:10, 19:25), ]


# Q3 Calculating TFR

tfr <- function(x, p){
  x <- filter(x, period == p)
  return(sum(x$asfr*5))
  
}

kenya_tfr <- c(tfr(kenya, "1950-1955"), tfr(kenya, "2005-2010"))
sweden_tfr <- c(tfr(sweden, "1950-1955"), tfr(sweden, "2005-2010"))
world_tfr <- c(tfr(world, "1950-1955"), tfr(world, "2005-2010"))

#changes in women amount
print(sum(world[world$period == "2005-2010", "py.women"]) - sum(world[world$period == "1950-1955", "py.women"]))

#changes in total births
print(sum(world[world$period == "2005-2010", "births"]) - sum(world[world$period == "1950-1955", "births"]))

names(kenya_tfr) <- names(sweden_tfr) <- names(world_tfr) <-
  c("1950-1955", "2005-2010")


kenya_tfr; sweden_tfr; world_tfr

sum(world[world$period == "2005-2010", "births"])
sum(world[world$period == "1950-1955", "births"])


# Q4 Calculating CDR

cdr <- function(x, p){
  x<-filter(x, period == p)
  CDR <- sum(x$death)/sum(x$py.men+x$py.women)
  return(CDR)
}

kenya_cdr <- c(cdr(kenya, "1950-1955"), cdr(kenya, "2005-2010"))
sweden_cdr <- c(cdr(sweden, "1950-1955"), cdr(sweden, "2005-2010"))
world_cdr <- c(cdr(world, "1950-1955"), cdr(world, "2005-2010"))

names(kenya_cdr) <- names(sweden_cdr) <- names(world_cdr) <-
  c("1950-1955", "2005-2010")

kenya_cdr; sweden_cdr; world_cdr

# Q5 Calculating ASDR

asdr <- function(x, p){
  x<-filter(x, period == p) 
  ASDR <- x$deaths/(x$py.women + x$py.men)
  return(ASDR)
  
}

kenya_asdr <- asdr(kenya, "2005-2010")
sweden_asdr <- asdr(sweden, "2005-2010")

summary(kenya_asdr)
summary(sweden_asdr)

# Q6 counterfactual CDR

kenya$p_counter <- (sweden$py.men+sweden$py.women)/sum(sweden[sweden$period == "1950-1955", "py.men"], sweden[sweden$period == "1950-1955", "py.women"])
kenya$p_counter[c(16:30)] <- (sweden[sweden$period == "2005-2010", "py.men"] + sweden[sweden$period == "2005-2010", "py.women"])/sum(sweden[sweden$period == "2005-2010", "py.men"], sweden[sweden$period == "2005-2010", "py.women"])

kenya_cdr_counter <- sum(kenya_asdr*kenya[kenya$period == "2005-2010", "p_counter"])

# original data
kenya_cdr

# counter facts
kenya_cdr_counter









