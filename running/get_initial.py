# get initial conditions from initial run in hisix


import numpy as np
import sys
import gzip
import glob
import os
import pandas as pd


#### DEFINE FUNCTIONS

def progbar(progress):
    nmax = round(progress*20,1)
    nn   = 0
    fs   = ''
    while nn<nmax:
        fs = fs+'-'
        nn +=1
    while nn<20:
        fs = fs+' '
        nn +=1
    print '\r-->  ['+fs+'] ('+str(round(progress*100,1))+'%)',


def impact_in_range(xtcp,x,b0,b1,jaw=0):
    '''Returns True if b in selected range. xtcp and x in cm, b0 and b1 in um'''
    if jaw==0:  
        b = (abs(x)-xtcp)*1e4
        if b>b0 and b<b1:
            return True
        else:
            return False
    elif (jaw==1 and x>0):
        b = (x-xtcp)*1e4
        if b>b0 and b<b1:
            return True
        else:
            return False
    elif (jaw==-1 and x<0):
        b = (-x-xtcp)*1e4
        if b>b0 and b<b1:
            return True
        else:
            return False        
        
    
def get_side(x):
    if x>0:
        return 'left'
    else:
        return 'right'

class initial:
    def __init__(self,path):
        self.initial = []
        self.left    = open(path+'initial.left','w+')
        self.righ    = open(path+'initial.right','w+')
        self.lwrit   = 0
        self.rwrit   = 0
    def load(self,fname):
        self.initial = []
        with gzip.open(fname+'initial.dat.gz') as f:
            for j,line in enumerate(f):
                self.initial.append(line)
    def write_by_index(self,indx,side):
        string_to_write = self.initial[indx]
        if side == 'left':
            self.lwrit+=1
            self.left.write(string_to_write)
        else:
            self.rwrit+=1
            self.righ.write(string_to_write)
    def flush(self):
        print '-->  Finalizing'
        self.left.close()
        self.righ.close()
        print '-->  Written lines:'
        print '-->  Left: %i Right:%i' % (self.lwrit,self.rwrit)
        print '-->  Done'



def generate_initialdat(path,npart,jaw='both'):
    print '-->  WARNING:'
    print '-->  function generate_initialdat depreciated'
    print '-->  using generate_initial instead'
    generate_initial(path,npart,jaw=jaw)


def generate_initial(path,npart,jaw='both'):

    # some initialization
    if jaw not in ['left','right','both']:
        raise Exception("jaw keyword unknown, user 'left', 'right' or 'both'")

    lQ = True
    rQ = True
    col= ['ID','WEIGHT','WEIGHT2','X','Y','S','PX','PY','PS','A','Z','m','E','Sigma']
    
    # read the generated files with initial distribution
    try:
        left = pd.read_csv(path+'initial.left',delim_whitespace=True)
        left.columns = col
    except:
        lQ   = False                                                    # don't handle when file empty
    try:
        righ = pd.read_csv(path+'initial.right',delim_whitespace=True)
        righ.columns = col
    except:
        rQ   = False

    if not (lQ and rQ) and (jaw=='both'):
        raise Exception("Can't read left and right particle distribution")

    if (rQ and lQ and jaw=='both'):
        print '-->  Sampling distribution for particles starting from both jaws'                
        idx   = min(len(left),len(righ))
        left  = left[0:idx]
        righ  = righ[0:idx]
        rawdf = pd.concat((left,righ))
    if (rQ and not lQ) or jaw=='right':
        print '-->  Sampling distribution for particles starting from right jaw'        
        rawdf = righ
    if (lQ and not rQ) or jaw=='left':
        print '-->  Sampling distribution for particles starting from left jaw'
        rawdf = left       

    iturn = 1+npart/len(rawdf)                                   # number of iterations to fill file

    rawdf['WEIGHT2']  = rawdf['WEIGHT2'].map(lambda x: '%2.1f' % x)
    rawdf['S']        = rawdf['S'].map(lambda x: '%2.1f' % x)
    rawdf['X']        = rawdf['X'].map(lambda x: '%14.13E' % x)
    rawdf['PX']       = rawdf['PX'].map(lambda x: '%14.13E' % x)
    rawdf['Y']        = rawdf['Y'].map(lambda x: '%14.13E' % x)
    rawdf['PY']       = rawdf['PY'].map(lambda x: '%14.13E' % x)
    rawdf['PS']       = rawdf['PS'].map(lambda x: '%2.1f' % x)
    rawdf['m']        = rawdf['m'].map(lambda x: '%16.13f' % x)
    rawdf['E']        = rawdf['E'].map(lambda x: '%14.13E' % x)
    
    with open('initial.dat', 'a') as f:
        for i in range(iturn):
            #print '\r-->  writing pack', i, '/', iturn,
            progbar(float(i)/float(iturn))
            rawdf.ID    = np.arange(1+i*len(rawdf),(1+i)*len(rawdf)+1)
            rawdf.to_csv(f,header=False,index=False,sep='\t')            



            

def main(path,b0,b1,xtcp,jaw=0):
    # jaw=0 when impacts on both jaws should be sampled
    
    b0,b1 = float(b0), float(b1)
    xtcp  = float(xtcp)
    impQ  = False

    print '-->  Running get_initial in directory: %s' % path
    print '-->  Selected impact parameters between %2.1f um and %2.1f um' % (b0, b1)

    ini = initial(path)
    ntot = len(glob.glob(path+"/run*/"))
    for i,fname in enumerate(glob.glob(path+"/run*/")):                        # loop over directories
        # find the impact file
        if not impQ:
            for fl in os.listdir(fname):
                if fl.endswith(".67.gz") or fl.endswith(".67"):
                    impactfile = fl
                    impQ       = True
                    print '-->  Impact file found:', impactfile
                    print '-->  Impact file directory:', fname
            if not impQ:
                continue

        print '\r-->  Analzying run', i, '/', ntot ,

        try:
            with gzip.open(fname+impactfile) as f:
                ini.load(fname)                                                    # load initial file
                for line in f:
                    pid,x = np.array(line.split())[[1,6]]
                    pid,x = int(pid),float(x)
                    if impact_in_range(xtcp,x,b0,b1,jaw=jaw):
                        side = get_side(x)
                        ini.write_by_index(pid-1,side)
                        
        except:
            pass
        if i==-1:
            break
    ini.flush()


def bdist(path,xtcp,jaw=0):
    '''sample the distribution of impact parameters'''

    barray = []
    xtcp   = float(xtcp)                                            # tcp half gap
    impQ   = False                                                  # logical switch
    ini    = initial(path)                                          
    ntot   = len(glob.glob(path+"/run*/"))                          # total number of directories
    nleft  = 0                                                      # number of particles on left jaw
    nrigh  = 0                                                      # right jaw
    
    print '-->  Sampling impact parameter distribution in directory: %s' % path
    
    for i,fname in enumerate(glob.glob(path+"/run*/")):             # loop over directories
        if not impQ:
            for fl in os.listdir(fname):
                if fl.endswith(".67.gz"):                           # find the impact file
                    impactfile = fl
                    impQ       = True
                    print '-->  Impact file found:', impactfile
                    print '-->  Impact file directory:', fname
            if not impQ:
                continue                                            # continue trying to find file

        print '\r-->  Analzying run', i, '/', ntot ,                # only if .67 file found

        try:
            with gzip.open(fname+impactfile) as f:
                ini.load(fname)                                     # load initial file
                for line in f:
                    pid,x,xp = np.array(line.split())[[1,6,7]]           
                    pid,x,xp = int(pid),float(x),float(xp)
                    if x>0:
                        nleft+=1
                    else:
                        nrigh+=1
                    barray.append([float(x),float(xp)])
        except:
            pass
        if i==-1:
            break
    #ini.flush()
    print '\n-->  Particles at x>0: ', nleft
    print '-->  Particles at x<0: ', nrigh
    print '-->  Total:            ', nleft+nrigh
    na = np.array(barray)
    df = pd.DataFrame(na)
    df.columns=['x','xp']
    return df

# run the program if it is started independently

if __name__ == "__main__":
    if len(sys.argv)!=5:
        print 'ERROR: Wrong argument list'
        print 'Usage:'
        print 'python get_initial.py path b0, b1, xtcp [cm]'
        print 'leaving...'
        exit()
    else:
        main(sys.argv[1],sys.argv[2],sys.argv[3],sys.argv[4])


