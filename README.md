# pipah-wilkins
Code for CRM-based dose finding and interim analysis for PIPAH study

## Restoring the project

To help reproduce the environment, the package `renv` is used. The `renv` package should install automatically when the .proj file is opened.
If it does not, install `renv` using the command `install.packages("renv")`.

The packages used (and their corresponding versions) can then be installed using the command `renv::restore()`.

For a more complete picture of the environment in which the code was run, the output of `sessionInfo()` is included at the bottom of this document.

NOTE: for both included scripts, we have set

`NSIM <- 10`

that is, the number of simulations used is 10. This is purely for checking that the code runs without errors, and was set to 1000 in actual use.


## Trial Design

The code used to design the trial is shown in **Dose_finding_publish_v2.R**.

The `bcrm` ("Bayesian CRM") package is used to obtain the trial design. Throughout, plots and reports are generated and saved to the hard drive (in the working directory). This includes operating characteristics, trajectories and the skeleton.

A variety of further simulations are run, pertaining to different possible scenarios. These are condensed into a `for` loop.

Average incidence of toxicity for the various scenarios is also recorded and saved.




## Output of sessionInfo()


R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=English_United Kingdom.utf8 
[2] LC_CTYPE=English_United Kingdom.utf8   
[3] LC_MONETARY=English_United Kingdom.utf8
[4] LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.utf8    

time zone: Europe/London
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods  
[7] base     

loaded via a namespace (and not attached):
[1] BiocManager_1.30.25 compiler_4.4.1      tools_4.4.1        
[4] librarian_1.8.1     renv_1.0.11       