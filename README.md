# SWAT_BOA
R scripts for running SWAT and rewriting parameters

In RunAllScripts_SWATv60.5.2 right after line 43 is where I would imagine you pass the parameters from BOA to the R script to have it rewrite the parameters. I noted the range that you could start off with for the two parameters.

Does RunAllScripts_SWATv60.5.2  need to be put on some sort of loop with BOA?

Outputs of interest for the two parameters we are looking at:
In the "outputs" folder --> WM_stats contains the performance metrics, I would use 
- sedconc (daily and monthly NSE and pbias)
- totpconc (daily and monthly NSE and pbias)
If it helps you could just pick daily or monthly to start


NSE -> the higher the number the better, perfect model = 1, 
pbias -> closest to 0 is best
