### INITIALIZATION

import numpy as np
import sys
import matplotlib.pyplot as plt
from optparse import OptionParser
import os

from hisix import *
import plottingtools as pt
import lhcTools as lt

path = {}
beam = 1


### CHECK ARGUMENTS

nargs = len(sys.argv)

if nargs<2:
    print 'ERROR: no hisix directory given; ABORT'
    exit()
elif nargs == 2:
    path[1] = sys.argv[1]
elif nargs >2:
    print 'Reading multiple directories'
    for j in range(1,nargs):
        path[j] = sys.argv[j]



### LOAD THE OBJECTS TO PROCESS THE LOSSES

f3pos  = ['../../fort.3', '../../input/fort.3',
          path[1]+'../fort.3']
twpos  = ['../../preprocessing/optics/twiss_b1.tfs', 
          '../../input/twiss_b1.tfs',
          path[1]+'../twiss_b1.tfs']


# check where the fort.3 is located

for p in f3pos:
    if os.path.isfile(p):
        fort3 = p
        break

# check where the twiss file is located

for p in twpos:
    if os.path.isfile(p):
        twiss = p
        break



#### READ THE LOSSES FROM THE SIMULATION

losses = losses(fort3,twiss)


### READ THE SUMMARY FILES

for k in path.keys():
    losses.getSummary(path[k])
losses.finalize()




# cycle for beam 2

if beam == 2:
    losses.cbin=(losses.cbin-lt.clhc)*-1
    losses.wbin=(losses.wbin-lt.clhc)*-1
    for k in range(len(losses.collimator_array)):
        losses.collimator_array[k][0]=(losses.collimator_array[k][0]-lt.clhc)*(-1)




### PLOT THE LOSSES

xmin,xmax = 0,lt.clhc
ymin,ymax = 1e-6,3

nbins     = int(lt.clhc/0.10)
width     = 0.80*lt.clhc/nbins

fig, (ax0) = plt.subplots()


ax0.bar(losses.cbin,losses.cold/(0.1*losses.normfac),width=width, 
        color='blue', edgecolor='blue', label='Cold')
ax0.bar(losses.wbin,losses.warm/(0.1*losses.normfac),width=width, 
        color='red', edgecolor='red', label='Warm')
ax0.bar(losses.collimator_array[:,0],losses.collimator_array[:,1]/losses.normfac,width=1.0,color='black',edgecolor='black',label='Collimator')


## PLOT PROPERTIES

ax0.set_yscale('log', nonposy='clip')
ax0.set_xlim(xmin,xmax)
ax0.set_ylim(ymin,ymax)
ax0.set_xlabel(r'Longitudinal Coordinate (m)')
ax0.set_ylabel(r'Local cleaning inefficiency $\eta$ (1/m)')
ax0.xaxis.grid(True)
ax0.yaxis.grid(False)
ax0.grid()
ax0.legend(loc=0)

plt.show()
