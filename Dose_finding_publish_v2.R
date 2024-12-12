#### Imatinib Dose finding study ####
## Author: Mikel Mckie ##
## Changing original design to make the TTL 20% and addin an intermediate dose of 250mg instead of 50mg

#### Load packages, set random seed ####
library(bcrm)
library(rjags)
library(R2WinBUGS)
set.seed(1234)


#### Setting up design and initial simulations ####

NSIM <- 10 # Set to 10 for users to run checks. Increase to 1000 for finding design.

stop_rules <- list(nmax = 13,
                   safety = 0.9,
                   nmin = 6)
PI <- c(0.12, 0.20, 0.30, 0.50)
dose <- c(100, 200, 300, 400)
model <- "power"
pa <- list(1,1,1)
TTL <- 0.20
s.calc <- "mean"

# Simulation for expected scenario:
sim.conts <- bcrm(stop = stop_rules,
                  data = NULL,
                  p.tox0 = PI, # Prior probs of outcome at each dose level
                  dose = dose, # actual doses
                  ff = model, # Functional form of model
                  prior.alpha = pa, # Distributional information for prior (gamma(1,1))
                  cohort = 1, # Size of each cohort
                  target.tox = TTL, # Target toxicity prob.
                  constrain = TRUE, # Use dose-skipping constraint
                  sdose.calculate = s.calc, # Plug-in estimate of prior alpha for std'd doses
                  start = 1,  # Dose level for beginning of trial
                  simulate = TRUE,
                  method = "rjags", # Optimisation method
                  nsims = NSIM, # number of simulations
                  plot = TRUE, # Plot dose-response curve after each cohort
                  truep = PI, # True probs. of toxicity at each dose level
                  threep3 = TRUE) # Calculate operating chars. of 3+3 design

print(sim.conts, threep3 = TRUE, tox.cutpoints = c(0.1,0.3,0.6))
capture.output(print(sim.conts, threep3 = TRUE, tox.cutpoints = c(0.1,0.3,0.6)),
               file = "conts_res.txt")
plot(sim.conts, threep3 = TRUE)
plot(sim.conts, trajectories = TRUE)

#### Skeleton plot ####

ndoses <- length(dose)
dose.labels <- sim.conts[[1]]$sdose
delta <- 0.1

beta<-rgamma(n = 1e6, shape = 1, scale = 1) # Prior distribution 'beta'
out.list<-lapply(1:ndoses, function(z) (dose.labels[z])^(beta)) # List of prior distributions for probability of DLT per dose level
ci.limits<-c(0.05, 0.95)
cred.intervals<-sapply(1:ndoses, function(z) quantile(out.list[[z]], ci.limits)) # Prior 5% and 95% percentiles for probability of DLT

plot(dose.labels, PI, type = "b", las = 1, lwd = 2,
     xlim = 0.05*c(round(range(dose.labels)/0.05)), ylim = c(0, 1), xlab = "Standardised Dose", ylab = "Probability of DLT", lty = 1) # Plot skeleton relationship (x-axis limits rounded to nearest 0.05)
lines(dose.labels, cred.intervals[1,], type = "b", lty = 3, lwd = 2)
lines(dose.labels, cred.intervals[2,], type = "b", lty = 3, lwd = 2)
abline(h = TTL, lty = 3, col = "red", lwd = 2)
abline(h = c(TTL-delta, TTL+delta), lty = 3, col = "red", lwd = 1)
legend(y = 0.8, dose.labels[1],
       lty = c(1, 3, 2, 3),
       pch = c(21, 21, NA, NA),
       pt.bg = c("white", "white", NA, NA),
       lwd = rep(2, 4),
       col = c("black", "black", "red", "red"),
       legend = c("Skeleton", "90% Credible Interval", "TTL", "TTL+/-0.1"))


#### Running simulations on different scenarios ####

s.up <- PI*1.20 ## 20% increase to toxicity
s.dn <- PI*0.80 ## 20% decrease in expected toxicity
s.tox <- PI+0.21 ## all doses higher than TTL
s.steep <- c(0.12, 0.28, 0.48, 0.88) ## Toxicity increase between levels is doubled
s.shallow <- c(0.12, 0.16, 0.21, 0.31)

scn <- list(s.up, s.dn, s.tox, s.steep, s.shallow)

plot(dose.labels, PI, type = "b", ylim = c(0,1), xlab = "Standardised Dose", ylab = "Probability of DLT")
abline(h = TTL, lty = 1, col = "blue", lwd = 1)
for(i in 1:length(scn)){
  lines(dose.labels, scn[[i]], type = "b", lty = 3, lwd = 2, col = "red")
}

for (i in 1:length(scn)) {
  plot(dose.labels, PI, type = "b", ylim = c(0,1))
  lines(dose.labels, scn[[i]], type = "b", col = "blue")
  abline(h = TTL, lty = 3, col = "red", lwd = 2)
  abline(h = c(TTL-delta, TTL+delta), lty = 3, col = "red", lwd = 1)
}

scenario.output <- vector("list", length(scn))

for(i in 1:length(scn)){
  scenario.output[[i]] <- bcrm(stop = stop_rules,
                               data = NULL,
                               p.tox0 = PI,
                               dose = dose,
                               ff = model,
                               prior.alpha = pa,
                               cohort = 1,
                               target.tox = TTL,
                               constrain = TRUE,
                               sdose.calculate = s.calc,
                               start = 1,
                               simulate = TRUE,
                               method = "rjags",
                               nsims = NSIM,
                               plot = TRUE,
                               truep = scn[[i]],
                               threep3 = TRUE)
  print(scenario.output[[i]], threep3 = TRUE, tox.cutpoints = c(0.1,0.3,0.6))
  capture.output(print(scenario.output[[i]],
                     threep3 = TRUE,
                     tox.cutpoints = c(0.1,0.3,0.6)),
                 file = paste0("scenario_", i, "_result.txt"))
  plot(scenario.output[[i]], threep3 = TRUE, file = paste0("Operating_chars_scen_", i, ".pdf"))
  plot(scenario.output[[i]], trajectories = TRUE, file = paste0("Trajectories_scen_", i, ".pdf"))
}


## Extracting average incidence on toxicities across simulations

tox.counter <- function (sim.result, nsims = NSIM, dose){
  tox <- data.frame()
  for (i in 1:nsims){
    tox <- rbind(tox, sim.result[[i]]$tox)
  }
  colnames(tox) <- dose
  tox.means <- colMeans(tox)
  return(tox.means)
}

tox <- vector("list", length(scenario.output))

for(i in 1:length(scenario.output)){
  tox[[i]] <- tox.counter(sim.result = scenario.output[[i]], nsims = NSIM, dose = dose)
}

tox.all <- do.call("rbind", tox)
tox.tot <- sapply(1:nrow(tox.all), function(z) sum(tox.all[z,]))
tox.all <- cbind(tox.all, tox.tot)

write.csv(tox.all, file ="DLTs_res.csv")
