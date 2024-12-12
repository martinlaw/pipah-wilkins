# Based on "Interim 7 code.R" by Mikel Mckie, run on 24/02/2021.

#### Load packages, set random seed ####
if("librarian" %in% installed.packages()==FALSE) install.packages("librarian")
librarian::shelf(bcrm, rjags, R2WinBUGS, reshape2)
set.seed(1234)


#### Toxicity data import ####
tox.data <- data.frame(patient = c(1,2,3,4,5,6, 7),
                       dose = c(1,2,3,3,4,4,3),
                       tox = c(0,0,0,0,0,1,0))

# CRM parameters (details below)

stop_rules <- list(nmax = 13,    # max sample size of trial
                   safety = 0.9, # stop if post. prob. (lowest dose > TTL)>0.9
                   nmin = 6)     # min sample size of trial
PI <- c(0.12, 0.20, 0.30, 0.50)
dose <- c(100, 200, 300, 400)
model <- "power"
pa <- list(1,1,1)
TTL <- 0.20
s.calc <- "mean"

# Run BCRM:
trial.output <- bcrm(stop = stop_rules,
                     data = tox.data,
                     p.tox0 = PI, # Prior probs of outcome at each dose level
                     dose = dose, # actual doses
                     ff = model, # Functional form of model
                     prior.alpha = pa, # Distributional information for prior (gamma(1,1))
                     target.tox = TTL, # Target toxicity prob.
                     cohort = 1, # Size of each cohort
                     constrain = TRUE, # Use dose-skipping constraint
                     sdose.calculate = s.calc, # Plug-in estimate of prior alpha for std'd doses
                     start = 1, # Dose level for beginning of trial
                     method = "rjags", # Optimisation method
                     nsims = 1000)

# BCRM results:
print(trial.output)
plot(trial.output)
plot(trial.output, trajectory = TRUE)

point.estimate <- trial.output[["ndose"]][[1]][["est"]]
next.dose <- trial.output[["ndose"]][[1]][["ndose"]]

dose.labels <- trial.output[["sdose"]]
rec.dose.label <- dose.labels[next.dose]
delta = 0.1

plot(dose.labels,
     PI,
     type = "b", las = 1, lwd = 2,
     xlim = 0.05*c(round(range(dose.labels)/0.05)),
     ylim = c(0, 1), xlab = "Standardised Dose",
     ylab = "Probability of DLT", lty = 1) # Plot skeleton relationship (x-axis limits rounded to nearest 0.05)
lines(dose.labels, point.estimate, type = "b", lwd = 2, col = "blue")
abline(h = TTL, lty = 3, col = "red", lwd = 2, las = 1)
abline(h = c(TTL-delta, TTL+delta), lty = 3, col = "red", lwd = 1)
text(rec.dose.label, point.estimate[next.dose]-0.05, "Recommended next dose:\n300mg", pos = 4,
     offset = 1.2, cex=0.75)
legend(y = 1, dose.labels[1],
       lty = c(1, 1, 2, 3),
       pch = c(21, 21, NA, NA),
       pt.bg = c("white", "white", NA, NA),
       lwd = rep(2, 4),
       col = c("black", "blue", "red", "red"),
       legend = c("Prior Pobabilities of Toxicity",
                  "Updated Probabilities of Toxicity",
                  "Target Toxicity level",
                  "+/- 0.1"))

out <- trial.output[["ndose"]][[1]]

out.post <- rbind(out$mean, out$sd, out$quantiles[4,], out$est )
row.names(out.post) <- c("Mean", "SD", "Median", "Plug-in Estimate")
colnames(out.post) <- c(100,200,300,400)
out.post

colnames(out$quantiles) <- c(100,200,300,400)
out$quantiles

write.csv(out.post, file = "I7_out.csv")
write.csv(out$quantiles, file = "I7_out_2.csv")



#### Adverse event data (not part of modelling) ####
# AE.data <- read.csv("AE interim 7 export.csv")
#
# AE.data$Dose <- "400mg" # may need manual update
# AE.data <- subset(AE.data, select = c("AE.Number","Subject", "Adverse.Event.Term", "Date...Onset.date" ,
#                                       "itmAEGrade...Severity",  "Dose", "itmAERelStdy...Relationship.to.Study.IMP",
#                                       "itmAEActTak...Action.taken", "Comments"))
# # change "itmAESev...Severity" for itmAEGrade ~ Severity
#
# colnames(AE.data) <- c("AE Number", "Subject", "CTCAE Term", "Onset date of AE", "CTCAE Severity", "Study drug Dose",
#                        "Causality/Relationship", "Action Taken", "Notes")
#
# AE.dat <- t(AE.data)
#
# write.csv(AE.dat, file = "AEtable.csv")


