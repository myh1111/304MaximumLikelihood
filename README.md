# BIOL304 Maximum Likelihood Methods
Spring 2015, Professor Robin Snyder
Final project for BIOL 304: Maximum Likelihood Models. Worked with Mark Loria on the analysis, although the code is my own.


Analysis was heavily based on the paper and is pretty much a replication of their work.
*   Gomez, Brad T., Thomas G. Hansford and George A. Krause.  2007.  "The Republicans Should Pray
      for Rain: Weather, Turnout, and Voting in U.S. Presidential Elections."  Journal of Politics  69 (August):
      649-663

Dataset used from here:
http://myweb.fsu.edu/bgomez/research.html

PLMValidation.R - basically a line by line replication of the analysis done in the paper in R.

MakeModel.R - performs essentially the same type of analysis for turnout, but is simply a logistic regression. Includes code to generate AIC to dtermine between a stepwise variable adding model and the full model. Uses the bbmle package to atrain the model through MLE estimation.

VoteShareModel.R - Performs the techniques as the MakeModel function, but instead of predicting turnout, predicts Republican vote share. 

All other files are images used in the report that I did, and should be generated in the code.

The full report is available upon request.
