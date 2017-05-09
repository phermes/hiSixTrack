
import numpy as np
from lhcutils.io import *
import lhcTools as lt
import gzip
import sys
import glob
import os
import pandas as pd
import math

def warmQ(sposition,beam=1):
    '''warmQ(s)

    Returns True if s is in warm LHC region, False if not.'''
    warm = False
    if beam==2:
        sposition=lt.clhc-sposition
    for i in lt.WarmRegions:
        if i[0]<sposition and i[1]>sposition:
            warm = True
    return warm

def gopen(filename):
    if 'gz' in filename:
	f = gzip.open(filename)
    else:
	f = open(filename)
    return f

class collimator_positions:

    def __init__(self):
        self.description = "Tools for collimator positions"
        self.author = "ph"                

    def twiss(self,fname):
        r = TFSReader(fname)
        return r

    def get_position(self,tfsobject,element):
        names = tfsobject.get_parameter('name')
        s     = tfsobject.get_parameter('s')
        nid   = names.index(element)
        pos   = s[nid]
        pos   = float(pos)
        return pos

    def assign_position(self,fort3,twissfile):
        '''Assign the collimator id to the collimator position
        usage:
        assign_position(fort.3,)
        '''
        # read optics file
        r     = self.twiss(twissfile)
        # assign collimator name, id, position
        fluka     = False
        collposid = []
        with open (fort3) as f:
            for line in f:
                # read the FLUKA block of fort.3 is found
                if ('FLUKA' in line) and ('/' not in line):       
                    fluka = True
                # stop at end of FLUKA block
                if (fluka) and ('NEXT' in line):                  
                    break
                # check if 'tc' is in the line (==Collimator)
                if (fluka) and ('tc' in line) and ('/' not in line):
                    name, cexit, cid, length = line.split()
                    cid  = int(cid)
                    name = name.upper()
                    pos  = self.get_position(r,name)
                    collposid.append([cid,pos,0])
        collposid = np.array(collposid)
        print '--> Assigned ID from FLUKA input to ' + str(len(collposid)) + ' collimators'
        return collposid

class losses(collimator_positions):
    '''Process losses'''
    def __init__(self,fort3,twissfile):
        # initialize the objects to store the loss information

        collposid             = self.assign_position(fort3,twissfile)

        self.cbin             = np.arange(0,lt.clhc,0.1)
        self.wbin             = np.arange(0,lt.clhc,0.1)
        self.cold,self.warm   = np.zeros(len(self.cbin)), np.zeros(len(self.wbin))

        self.beam             = 1
        self.collimator_array = []
        self.normfac          = 1.

        self.finalized        = False

        self.collimator = {}
        for line in collposid:
            self.collimator[str(int(line[0]))] = np.array([line[1],line[2]])

    def aperloss(self,s,energy):
        bin_idx= int(round(s*10))
        if warmQ(s,beam=self.beam):
            self.warm[bin_idx]+=energy
        else:
            self.cold[bin_idx]+=energy

    def colliloss(self,collid,dE):
        self.collimator[collid][1] += dE

    def ReadHiSixSummary(self,filename,scaling=1.0,losstype=None):
        f = gopen(filename)
        if '999' in filename or losstype=='aperture':
            # print 'Reading aperture loss file'
            for j,line in enumerate(f):
                try:
                    s,energy,A,Z = line.split()
                    s,energy,A   = float(s),scaling*float(energy),float(A)
                    self.aperloss(s,energy)
                except:
                    pass
        if '66' in filename or losstype=='correction':
            # print 'Reading correction file'
            for j,line in enumerate(f):
                try:
                    cid, dA, energy = line.split()
                    cid, dA, energy = cid, int(dA), scaling*(-1)*float(energy)       
                    self.colliloss(cid,energy)
                except:
                    pass
        if '208' in filename or losstype=='collimator':
            # print 'Reading collimator file'
            for j,line in enumerate(f):
                cid, dA, energy = line.split()
                cid, dA, energy = cid, int(dA), scaling*float(energy)       
                self.colliloss(cid,energy)

        self.collimator_array = []
        for k in self.collimator.values():
            self.collimator_array.append(k)
        self.collimator_array = np.array(self.collimator_array)

        # compute the normalization factor
        self.normfac = max(self.collimator_array[:,1])

        f.close()


    def AvMomentumPerNucleon(self,filename):
        data = np.genfromtxt(filename)
        if 'fort.208' in filename or 'fort.66' in filename:
            cols = ['ID','A','E']
            idx  = data[:,0]
        elif 'fort.999' in filename:
            cols = ['S','E','A','Z']   
            idx  = None
        # read the file
        data = pd.DataFrame(data,index=idx)
        data.columns = cols
        # find collimator with the highest amout of losses
        peakloss = data[data.E==data.E.max()]
        return peakloss.iloc[0].E/peakloss.iloc[0].A

    def ScalingFactors(self,path):
        coll = self.AvMomentumPerNucleon(path+'fort.208')
        corr = self.AvMomentumPerNucleon(path+'fort.66')
        aper = self.AvMomentumPerNucleon(path+'fort.999.comp')

        refMA = min(coll,corr,aper)

        collF, corrF, aperF = 10**(round(math.log10(coll/refMA))),10**(round(math.log10(corr/refMA))),10**(round(math.log10(aper/refMA)))
        collF, corrF, aperF = 1/collF, 1/corrF, 1/aperF
        return collF, corrF, aperF

    def getSummary(self,path):
        #
        collF, corrF, aperF = self.ScalingFactors(path)

        self.ReadHiSixSummary(path+'fort.208',scaling=collF)
        self.ReadHiSixSummary(path+'fort.66',scaling=corrF)
        self.ReadHiSixSummary(path+'fort.999.comp',scaling=aperF)
        
        
    def finalize(self):
        self.cbin = self.cbin[self.cold>0]
        self.cold = self.cold[self.cold>0]
        self.wbin = self.wbin[self.warm>0]
        self.warm = self.warm[self.warm>0]


        if self.beam==2:
            self.cbin=(losses.cbin-lt.clhc)*-1
            self.wbin=(losses.wbin-lt.clhc)*-1
            for k in range(len(losses.collimator_array)):
                self.collimator_array[k][0]=(losses.collimator_array[k][0]-lt.clhc)*(-1)
            print '--> Coordinate Cycle for B2'
        self.finalized = True

    def lossmap(self,axis,xmin=0,xmax=lt.clhc):
        'Plot loss map'

        if not self.finalized:
        	print 'Losses not finalized'
        	print 'Applying self.finalize()'
        	self.finalize()

        cidx = (self.cbin > xmin) & (self.cbin < xmax)
        widx = (self.wbin > xmin) & (self.wbin < xmax)
        colx = (self.collimator_array[:,0] > xmin) & (self.collimator_array[:,0] < xmax)

        width = 0.08
        axis.bar(self.cbin[cidx],self.cold[cidx]/(0.1*self.normfac),width=width, 
            color='blue', edgecolor='blue', label='Cold')
        axis.bar(self.wbin[widx],self.warm[widx]/(0.1*self.normfac),width=width, 
            color='red', edgecolor='red', label='Warm')
        axis.bar(self.collimator_array[:,0][colx],self.collimator_array[:,1][colx]/self.normfac,
            width=1.0,color='black',edgecolor='black',label='Collimator')
        
        axis.set_xlim(xmin,xmax)
        axis.set_ylim(1e-6,3)
        axis.set_yscale('log', nonposy='clip')
        axis.xaxis.grid(True)
        axis.yaxis.grid(False)
        axis.grid()


class aperture:
    ''' Provides tools to analyze the aperture losses from hiSix 
        Usage:
        
        a = aperture()                               # instantiate the class
        filen = '/path/to/fort.impacts'              # input file name 
        a.read(filen,'left')                         # read the losses
        a.srange(6973.5,7012.5)                      # select s range
        a.sorted_isotope_list()                      # generate sorted isotope list (sorted by energetic contribution)
        print a.compos['all']                        # print the isotopic composition
    '''
    
    def __init__(self):
        self.losses         = {}                                   # dictionary with aperture losses
        self.totalA         = {}                                   # total number of nucleons lost in the region of interest
        self.compos         = {}                                   # nested dictionary of isotopic composition in the region

    def get_totalA(self):
        # compute the totalA for both jaws
        self.totalA['all']=0
        for k in self.losses.keys():
            self.totalA['all'] += self.totalA[k]
        
    def read(self,filename,jaw):
        self.losses[jaw]  = np.genfromtxt(filename)
        self.totalA[jaw]  = sum(self.losses[jaw][:,2])             # initialize the number of nucleons lost in the region
        self.compos[jaw]  = {}                                     # initialize the nested dictionary but don't fill
        self.get_totalA()

    def srange(self,s0,s1):
        '''extract the losses in a certain longitudinal range'''
        print 'extracting aperture losses in range:', s0,'m -',s1, 'm'
        
        # swap variables if s0>s1
        if s0>s1:
            s0,s1 = s1,s0
        
        # remove all entries not in the selected range
        for k in self.losses.keys():
            lsar              = self.losses[k]
            self.losses[k]    = lsar[(lsar[:,0]>s0) & (lsar[:,0]<s1)]
            # refresh the totalA
            self.totalA[k]  = sum(self.losses[k][:,2]) 
        
        self.get_totalA()
            
    def sorted_isotope_list(self):
        ''' Returns a list of the isotopes lost in the region of interest
        sorted by their energetic impact '''
        
        self.compos['all'] = {}
        for k in self.losses.keys():
            self.compos[k] = {}
            for s,e,a,z,x,y in self.losses[k]:
                a,z = int(a),int(z)
                
                if self.compos[k].has_key((a,z)):
                    self.compos[k][a,z]     +=a
                else:
                    self.compos[k][a,z]      =a
                
                if self.compos['all'].has_key((a,z)):
                    self.compos['all'][a,z]     +=a
                else:
                    self.compos['all'][a,z]      =a
        # sort the dictionaries
        for k in ['left','right','all']:
            for sk in self.compos[k].keys():
                self.compos[k][sk] = self.compos[k][sk]/float(self.totalA[k])
            self.compos[k] = sorted(self.compos[k].items(), key=lambda x: x[1])


class penetration:
    '''Provides tools to determine the penetration depth for aperture losses'''
    def __init__(self,path,twisspath=None):

	### PATHS
	self.path   = path
	self.twiss  = path+'/../twiss_b1.tfs'       # twiss file path
	self.aperf  = 'fort.999.gz'                 # aperture file path


	### GET NORMALIZATION FACTOR
	for fi in ['fort.208']:
            if os.path.isfile(self.path+fi):
		fn = fi
	coll_losses = np.genfromtxt(self.path+fi)
	self.nfac   = max(coll_losses[:,2])

	### VARIABLES
	self.s0            = 0.                     # longitudinal range of interest
	self.s1            = 0.                     # could be assigned later
	self.q0            = 0.                     # magnet center
	self.average_depth = 0.                     # average penetration depth

	### INITIALIZE ARRAYS
	self.aper_losses   = []                     # aperture losses array
        self.clusters      = []                     # array to store cluster locations
	self.depth         = {}
	self.posit         = {}
	self.weigh         = {}
        self.avdep         = {}

        ### PREPARE QUADRUPOLE ARRAY
        self.get_quadrupoles()

    def get_quadrupoles(self):
        '''Load all quadrupoles from the twiss file and assign their positions'''
        # load twiss file 
        opt   = TFSReader(self.twiss)
        names = opt.get_parameter('name')
        pos   = opt.get_parameter('s')
        pos   = np.array(pos).astype('float')

        self.quad         = {}
        self.quad['s']    = []
        self.quad['name'] = []

        for i in range(len(names)):
            if 'MQ' in names[i]:
                self.quad['s'].append(float(pos[i]))
                self.quad['name'].append(names[i])

        for k in self.quad.keys():
            self.quad[k] = np.array(self.quad[k])

    def read(self,path=None):
        '''Read the aperture impacts into the array self.aper_losses
        File should be generic output from the hisix compress tool.'''
        # if path not given explicitly use self.path
        if path==None:
            path = self.path
        # convert to list if it is a numpy array
        if type(self.aper_losses).__module__ == np.__name__:
            self.aper_losses = self.aper_losses.tolist()
        # loop over the file content
        f   = gopen(path+'fort.impacts.gz')
        for i,line in enumerate(f):
            # debugging
            if i == -1:
                break
            # main loop
            lne = np.array(line.split())
            lne = lne.astype('float')
            self.aper_losses.append(lne[np.array([0,2,4,5])])
        # convert to numpy
        self.aper_losses = np.array(self.aper_losses)
        f.close()
    
    def get_clusters(self,etamin=1e-6,path=None):
        '''Returns a list of the loss clusters'''
        
        #### INITIALIZATION
        lower, upper = 0., 0.

        # if path not given explicitly use self.path
        if path==None:
            path = self.path

        # convert to list if numpy
        if type(self.clusters).__module__ == np.__name__:
            self.clusters = self.clusters.tolist()

        try:
            aper_losses= np.genfromtxt(path+'/fort.999.comp')
        except:
            'Error reading input file:', path+'/fort.999.comp'

        #### MAIN PART
        for k in aper_losses:
            if k[1]/self.nfac>etamin*1e-1:
                # first cluster identification
                if lower==0.:									
                    lower = k[0]
                    upper = k[0]
                # cluster end identified
                elif k[0]>upper+20.:
                    self.clusters.append([lower-15.,upper+15.])
                    lower = k[0]
                    upper = k[0]
                # still in cluster: shift cluster limit to the right
                else:
                    upper = k[0]
        # convert (back) to numpy
        self.clusters = np.array(self.clusters)
    
    def get_depth(self,s0,s1):
        '''Fills relevant penetration depth parameters into the arrays:
           self.depth
           self.posit
           self.weigh
           The applied keys are s0 and s1'''
        #
        #
        q0_idx = self.quad['s']>s0
        q0     = self.quad['s'][q0_idx][0]            # quadrupole position
        #
        # extract the losses in the desired range
        sr_idx = (self.aper_losses[:,0]>s0) & (self.aper_losses[:,0]<s1)
        sr_apl = self.aper_losses[sr_idx]
        #
        # initialize the ouput arrays
        self.depth[s0,s1] = []
        self.posit[s0,s1] = []
        self.weigh[s0,s1] = []
        #
        # loop over all particles in the desired range
        for particle in sr_apl:
            s, a, x, xp = particle
            ds          = q0-s
            if ds>0:                                            # only particles lost BEFORE the quad
                dpt = ds*abs(xp)*1e3                            # [unit is mm]
                self.depth[s0,s1].append(dpt)
                self.posit[s0,s1].append(s)
                self.weigh[s0,s1].append(a)
        #
        # finalize
        self.depth[s0,s1]  = np.array(self.depth[s0,s1])
        self.posit[s0,s1]  = np.array(self.posit[s0,s1])
        self.weigh[s0,s1]  = np.array(self.weigh[s0,s1])
        try:
            self.avdep[s0,s1]  = np.average(self.depth[s0,s1],weights=self.weigh[s0,s1])  # average depth
        except:
            pass
    def check_all_clusters(self):
        '''extracts the impact depth for all identified loss clusters'''
        for (l,u) in self.clusters:
            l,u = int(l),int(u)
            self.get_depth(l,u)            


aperturefile   = "fort.999.gz" 
collimatorfile = "fort.208.gz"
collihitsfile  = "fort.209.gz"
pidfile        = "fort.822.gz"

class summarize_output:
    
    def __init__(self):
        self.runpath = None
        self.path    = None
        self.corrfn  = None

    def get_correction_filename(self):
        for f in os.listdir(self.runpath):
            if f.endswith('.66.gz'):
                self.corrfn = f
                print 'correction file found:', self.corrfn
    
    def readApertureLoss(self):
        aperhits         = pd.read_csv(self.runpath+aperturefile,
                                       delim_whitespace=True,header=None)
        aperhits         = aperhits[[4,5,15,16]]
        aperhits.columns = ["S","#ID","A","Z"]
        aperhits.index   = aperhits["#ID"] 
        
        self.aperhits    = aperhits
        
    def readParticleIDS(self):
        pids = pd.read_csv(self.runpath+pidfile,delim_whitespace=True)
        pids.columns = ["#ID","pID","pCID","A","Z","E"]
        pids.index= pids["#ID"]
        self.pids = pids
        
    def readCollimatorHits(self):
        colhits = pd.read_csv(self.runpath+collihitsfile,delim_whitespace=True,header=None)
        colhits.columns = (["CID","#ID"])
        colhits.index = colhits["#ID"]   
        self.colhits = colhits
        
        
    def readCorrection(self):
        correction = pd.read_csv(self.runpath+self.corrfn,
                                 delim_whitespace=True,header=None)
        correction = correction.groupby(3).sum()[4]
        correction.to_frame().to_csv(self.runpath+'corr.dat.gz',sep='\t',
                             compression='gzip',header=False)
        self.corrlosses = correction.to_frame().reset_index()
        self.corrlosses.columns = ["CID","E"]
        
    def get_summary(self):
        self.collilosses = pd.merge(self.colhits, self.pids,how='inner')
        self.apertlosses = pd.merge(self.pids,self.aperhits, on=["#ID","A","Z"])
        
    def save_run_summary(self):
        self.apertlosses.to_csv(self.runpath+'aper.dat.gz',
                   index=False,compression='gzip',sep='\t')
    
        self.collilosses.to_csv(self.runpath+'coll.dat.gz',
                   index=False,sep='\t',compression='gzip')
        
    def update_coll_summary(self,j):
        run_collfile = self.collilosses.groupby("CID").sum()[["A","E"]].reset_index()
        if j==0:
            self.coll_summary = run_collfile
        else:
            self.coll_summary = self.coll_summary.append(run_collfile)
            self.coll_summary = self.coll_summary.groupby("CID").sum().reset_index()
            
    def update_corr_summary(self,j):
        run_corrfile = self.corrlosses.groupby("CID").sum().reset_index()
        if j==0:
            self.corr_summary = run_corrfile
        else:
            self.corr_summary = self.corr_summary.append(run_corrfile)
            self.corr_summary = self.corr_summary.groupby("CID").sum().reset_index()
    
    def update_aper_summary(self,j):
        run_aperfile = self.apertlosses.groupby("S").sum()[["E","A","Z"]].reset_index()
        if j==0:
            self.aper_summary = run_aperfile
        else:
            self.aper_summary = self.aper_summary.append(run_aperfile)
            self.aper_summary = self.aper_summary.groupby("S").sum().reset_index()
            
    def save_summary(self):
        self.aper_summary.to_csv(self.path+'/hisix.aperture.dat.gz',sep='\t',compression='gzip',
                                 index=False,header=False)
        self.corr_summary.to_csv(self.path+'/hisix.correction.dat.gz',sep='\t',compression='gzip',
                                 index=False,header=False)
        self.coll_summary.to_csv(self.path+'/hisix.collimator.dat.gz',sep='\t',compression='gzip',
                                 index=False,header=False)
            
    def summarize_simulation_set(self):
        totalruns  = len(glob.glob(self.path+"/run*/"))
        totalrunsf = float(totalruns)
        for ii,fname in enumerate(glob.glob(self.path+"/run*/")):                        # loop over directories
            
            progress     = "{0:.2f}".format(100.*ii/totalrunsf)
            self.runpath = fname
            if ii==0:
                self.get_correction_filename()
            
            try:
                self.readApertureLoss()
                self.readParticleIDS()
                self.readCollimatorHits()
                self.readCorrection()
                self.get_summary()
                self.save_run_summary()
                self.update_coll_summary(ii)
                self.update_corr_summary(ii)
                self.update_aper_summary(ii)
                
            except:
                pass
            
            
            if ii==-1:
                break
            print "\rReading run", ii, "("+str(progress)+"%)",

        self.save_summary()


# tools to quickly read hiSixTrack outputfiles


def hiST_read_822(fn):
    df = pd.read_csv(fn,delim_whitespace=True)
    df.columns= ['PID','PARID','ECOLID','A','Z','EE','EPX']
    df['EE'] = df['EE']/1000.
    return df

def hiST_read_touchmap(fn):
    tm = pd.read_csv(fn,delim_whitespace=True)
    tm.columns = ['ICOLID','A','Z','IE','S','IX','IPX','IY','IPY','PID','PARID','ITURN','NON','NON2']
    tm = tm[['ICOLID','A','Z','IE','IX','IPX','IY','IPY','PID','PARID','ITURN']]
    return tm

def hiST_merge_822_tm(df1,df2):
    '''merges the touchmap information with information on the particle generation'''
    df = pd.merge(df1, df2, on=['PID','A','Z','PARID'])
    return df


def hiST_read_batch(path,f822fn,toucfn='',f999fn='',imax=-1):
    '''imax is the number of runs which should be read, default=-1 (all exiting runs)'''
    
    df = pd.DataFrame() # initialize dataframe

    i  = 0              # running index
    br = False          # break loop
    
    for subdir, dirs, files in os.walk(path):
        ldir = len(dirs)
        for d in dirs:
            try:
                f822i  = hiST_read_822(path+d+'/'+f822fn)
                touci  = hiST_read_touchmap(path+d+'/'+toucfn)            
                alli   = hiST_merge_822_tm(f822i,touci)
                df     = df.append(alli)
            except:
                pass

            i+=1
            print '\r--> reading run', i, '/', ldir, 

            if i==ldir:
                br=True
                break

            if i==imax:
                br = True
                break
        if br:
            break
    return df
