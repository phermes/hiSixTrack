import numpy as np
import sys
import gzip
import glob
import os
from hisix import gopen

version='0.1'

print ''' 
              **** hiSixTrack post-processing **** 
                   Version''', version
print '''

--> Compress the hiSixTrack output for loss map generation
 '''

#### DEVELOPMENT INFO
####


####


debugging           = False
impact_distribution = False                                # extract the impact distribution 

aperturefile   = "fort.999.gz" 
collimatorfile = "fort.208.gz"

print '--> Using the following files:'
print '--> Collimator losses:', collimatorfile
print '--> Aperture losses:  ', aperturefile


### Define progress bar

def progbar(progress):
    nmax = round(progress*40,1)
    nn   = 0
    fs   = ''
    while nn<nmax:
        fs = fs+'-'
        nn +=1
    while nn<40:
        fs = fs+' '
        nn +=1
    print '\r-->  ['+fs+'] ('+str(round(progress*100,1))+'%)',

### CHECK ARGUMENTS

if debugging:
	sys.argv.append('../../160323_b08-15_left/')
	sys.argv.append('100')

if len(sys.argv)<2:
    print '--> ERROR: no hisix directory given; ABORT'
    exit()
elif len(sys.argv) == 2:
	path = sys.argv[1]
	#print '--> imax not given'
	imax = -1
elif len(sys.argv) == 3:
	path = sys.argv[1]
	imax = sys.argv[2]
	imax = int(imax)
	print 'imax =', imax

### CHECK IF PATH ENDS WITH '/'

if path[-1]!='/':
    path = path+'/'
 
### INITIALIZE ARRAYS AND DICTIONARIES

collimator_leaking = {}                                                   
collimator_losses  = {}
aperture_losses    = []
aperture_impact    = []
k                  = 0    # number of runs read


####### MAIN LOOP

direct = glob.glob(path+"/run*/")    # find the directories to loop over
ndirec = len(direct)                 # number of directories to loop over

print '--> Number of runs    ', ndirec

for ii,fname in enumerate(direct):                        # loop over directories

	#### BREAK THE LOOP AT IMAX
	if k == imax:
		print 'break'
		break

	#### READ THE DATA
	corrupted = False
	# correction factors
	try:
		### FIND CORRECTION FILE - ONLY DONE ONCE
		if 'correctionfile' not in globals().keys():                      # check if correctionfile already
			for corrfilename in os.listdir(fname):
				if corrfilename.endswith(".66.gz"):
					correctionfile = corrfilename                         # define correction file
					print '--> Correction file:  ', corrfilename

		### READ CORRECTION FILES
		with gzip.open(fname+'/'+correctionfile) as f:                              # read fort.66 file
			for j,line in enumerate(f):                                   # loop over lines in file
				if j==-1:
					break
				pid, A, Z, cid, energy = line.split()
				cid    				   = int(cid)
				A                      = int(A)
				energy                 = float(energy)
			    
				if cid not in collimator_leaking.keys():                  # generate entry if not existing
					collimator_leaking[cid] = [cid,0,0.]

				collimator_leaking[cid][1]+=A                             # add A/energy to dictionary
				collimator_leaking[cid][2]+=energy
	except:
		corrupted = True
		pass

	#### COLLIMATOR LOSSES

	if not corrupted:
		try:
			with gzip.open(fname+'/'+collimatorfile) as f:
				for j,line in enumerate(f):
					if j==-1:
						break
					cid, A, energy = line.split()
					cid            = int(cid)
					A              = int(A)
					energy         = float(energy)

					if cid not in collimator_losses.keys():
						collimator_losses[cid] = [cid,0,0.]
					
					collimator_losses[cid][1]+=A
					collimator_losses[cid][2]+=energy
		except:
			corrupted = True
			pass

	#### APERTURE LOSSES

	if not corrupted:
		try:
			with gzip.open(fname+'/'+aperturefile) as f:
				for j,line in enumerate(f):
					if j==-1:
						break

					s,E            = line.split()[4] , line.split()[12]
					A,Z            = line.split()[-2], line.split()[-1]

					s,E            = float(s), float(E)
					A,Z            =   int(A),   int(Z)

					aperture_losses.append([s,E,A,Z])
					if A>208:
						print s,E,A,Z
					#
					if impact_distribution:
						x,xp        = line.split()[8],  line.split()[9]
						y,yp        = line.split()[10], line.split()[11]
						x,xp        = float(x), float(xp)
						y,yp        = float(y), float(yp)

						aperture_impact.append([s,E,A,Z,x,xp,y,yp])
		except:
			corrupted = True
	#		print 'aperture losses not readable:', fname
			pass
	if not corrupted:
		k+=1
        #print ' '         
#        print '\r--> Compressed runs:', k,
        #print ' '
#        print '\r--> Total runs read:', ii, '/', ndirec,
        #print ' '
        progbar(float(ii)/ndirec)
        #print ' '

print '--> Compressed runs:', k
print '--> Total runs read:', ii, '/', ndirec
        
runsread = k


# SORT APERTURE LOSSES

aperture_losses = np.array(aperture_losses)
aperture_losses = aperture_losses[np.argsort(aperture_losses[:,0])]

# COMPRESS APERTURE LOSSES

compressed_aperture = np.zeros(shape=(len(np.unique(aperture_losses[:,0])),4))
for i,s in enumerate(np.unique(aperture_losses[:,0])):
	compressed_aperture[i,0]=s

for i,s in enumerate(compressed_aperture[:,0]):
	compressed_aperture[i][1] = sum(aperture_losses[aperture_losses[:,0]==s][:,1])
	compressed_aperture[i][2] = sum(aperture_losses[aperture_losses[:,0]==s][:,2])
	compressed_aperture[i][3] = sum(aperture_losses[aperture_losses[:,0]==s][:,3])


# SORT APERTURE LOSSES WITH X,XP,Y,YP

if impact_distribution:
	aperture_impact = np.array(aperture_impact)
	aperture_impact = aperture_impact[np.argsort(aperture_impact[:,0])]
	aperture_impact = aperture_impact.tolist()

### REMOVE OLD OUTPUT

correctionfile = path+'fort.66'              # correction of collimator losses
collimatorfile = path+'fort.208'             # collimator losses
aperturlosfile = path+'fort.999'             # aperture losses
aperturcomfile = path+'fort.999.comp'        # compressed aperture losses
aperimpactfile = path+'fort.impacts'         # aperture losses with impact coordinates

for fname in [correctionfile, collimatorfile, aperturlosfile, aperimpactfile, aperturcomfile]: 
	try:
		os.remove(fname)
	except OSError:
		pass

### WRITE DATA TO OUTPUT FILES

f = open(correctionfile,'w+')
for k in collimator_leaking.keys():
	f.write('%2i %14i %14.8e \n' % tuple(collimator_leaking[k]))
f.close()

f = open(collimatorfile,'w+')
for k in collimator_losses.keys():
	f.write('%2i %14i %14.8e \n' % tuple(collimator_losses[k]))
f.close()

f = open(aperturlosfile,'w+')
for l in aperture_losses:
	f.write('%7.1f %9.1f %5i %5i\n' % tuple(l))
f.close()

f = open(aperturcomfile,'w+')
f.write('# %3s %16s %9s %7s\n' % ('s [m]','E [GeV]','A','(Z)'))
for l in compressed_aperture:
	f.write('%7.1f %16.1f %9i %9i\n' % tuple(l))
f.close()

if impact_distribution:
	f = open(aperimpactfile,'w+')
	for l in aperture_impact:
		f.write('%7.1f %9.1f %5i %5i %14.9f %14.9f %14.9f %14.9f \n' % tuple(l))
	f.close()

print '''
--> hiSixTrack compression run finished.
--> Total number of runs read:
--> ''', runsread,'/',ii



