# r.blip

R binding for Blip (Bayesian network Learning Improved Project)

### Example 

Minimal example: learns a BN from 'child-5000.dat' dataset: 

''' 
library('foreign')
library('bnlearn')
library('r.blip')
dat <- read.table('data/child-5000.dat', sep = ' ')
bn <- blip.learn(dat, time = 10)
'''

The resulting Bayesian network is in bnlearn format. 
