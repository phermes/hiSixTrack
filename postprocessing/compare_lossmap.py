### INITIALIZATION

import numpy as np
import sys
import matplotlib.pyplot as plt
from optparse import OptionParser
import BLM as blm 
import os

from hisix import *
import plottingtools as pt
import lhcTools as lt

path = {}
beam = 2


### INPUT TO READ MEASURED LOSSES

# path_measurement = '/media/phermes/1E10-926A/exp_data/20151213/losses_2015-12-13new.dat.gz'
# times            = "13/12/2015 22:07:58"

path_measurement = '/media/phermes/1E10-926A/exp_data/20151123/BLMLHC_2015-11-23_#1.txt.gz'
times            = "23/11/2015 23:32:08"



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

f3pos  = ['../fort.3', '../input/fort.3']
twpos  = ['../preprocessing/optics/twiss_b2.tfs', '../input/twiss_b2.tfs']

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

losses = losses(fort3,twiss)



### READ THE SUMMARY FILES

for k in path.keys():
    for f in ['fort.999','fort.208','fort.66']:
        losses.ReadHiSixSummary(path[k]+f)

losses.finalize()

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



### READ MEASURED LOSSES 

b      = blm.BLM()
b.read(path_measurement,times)



### PLOT THE COMPARISON

xmin,xmax  = 0,lt.clhc
ymin,ymax  = 1e-6,3
clhc       = 26658.883 
nbins      = int(clhc/0.10)
smax       = clhc
width      = 0.80*clhc/nbins                                 # bar width=80% of bin width 

#fig_size = pt.FigSize('talk',0.5)
#plt.matplotlib.rcParams.update(pt.updateParams(1.0,plottype='talk'))

# fig = plt.figure(figsize=(fig_size[0],1*fig_size[1]))

# initialize the plot

fig        = plt.figure()
ax0        = fig.add_subplot(211)
ax1        = fig.add_subplot(212,sharex=ax0)

# plot the data

ax0.bar(b.pos[b.cold],b.signal[b.cold]/(b.normfac),width=width, 
        color='blue', edgecolor='blue', label='Cold')
ax0.bar(b.pos[b.warm],b.signal[b.warm]/(b.normfac),width=width, 
        color='red', edgecolor='red', label='Warm')
ax0.bar(b.pos[b.coll],b.signal[b.coll]/(b.normfac),width=1.0,
        color='black',edgecolor='black',label='Collimator')


ax1.bar(losses.cbin,losses.cold/(0.1*losses.normfac),width=width, 
        color='blue', edgecolor='blue', label='Cold')
ax1.bar(losses.wbin,losses.warm/(0.1*losses.normfac),width=width, 
        color='red', edgecolor='red', label='Warm')
ax1.bar(losses.collimator_array[:,0],losses.collimator_array[:,1]/losses.normfac,width=1.0,color='black',edgecolor='black',label='Collimator')


# plot properties

ax0.set_yscale('log', nonposy='clip')
ax0.set_xlim(xmin,xmax)
ax0.set_ylim(ymin,ymax)
ax0.set_xlabel(r'Longitudinal Coordinate (m)')
ax0.set_ylabel(r'Local cleaning inefficiency $\eta$ (1/m)')
ax0.xaxis.grid(True)
ax0.yaxis.grid(False)
ax0.grid()
ax0.legend(loc=0)

ax1.set_yscale('log', nonposy='clip')
ax1.set_xlim(xmin,xmax)
ax1.set_ylim(ymin,ymax)
ax1.set_xlabel(r'Longitudinal Coordinate (m)')
ax1.set_ylabel(r'Local cleaning inefficiency $\eta$ (1/m)')
ax1.xaxis.grid(True)
ax1.yaxis.grid(False)
ax1.grid()
ax1.legend(loc=0)

plt.show()