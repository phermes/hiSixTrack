import pandas as pd
import glob
import os, sys
import numpy as np
import getopt


            
existing = False

def usage():
    mess='''
    get_initial.py 

    Creates the initial distributions for hisixtrack with predefined parameters.

    Usage: 

    python get_initial.py --path="/path/to/study" --b1=1 --b2=3 --halpgap=0.1512391
    
    will create the initial distribution summary files

    If the summary files exist and we want to create a initial.dat file for a study, we use:
    
    python get_initial.py --path="/path/to/study/new_initial/initial_right.dat" --existing  --npart=1000000
    this will create a new file containing 10000 particles 
    
    '''
    print(mess)


        
def create_new_ini_files(path,b0,b1,hgap):
    h = initial_result(path, hgap=hgap, b0=b0,b1=b1)
    h.read_all()

    
class initial_result:
    def __init__(self, dirname, hgap, b0, b1):
        self.newini = None      # compressed collimator loss data
        
        self.hgap = hgap
        self.b0   = b0
        self.b1   = b1
        
        self.successful, self.unsuccessful = 0, 0       
        self._dirs   = glob.iglob("{0}/run_*".format(dirname))
        self.maindir = dirname
        
        while True:
            try:
                self._next()
                self._find_hitfile()
                break
            except IndexError:
                continue
                
    def _next(self):
        self.dir = next(self._dirs)
        
    def _find_hitfile(self):
        self.hitfn = glob.glob('{0}/*fort.67.gz'.format(self.dir))[0].split('/')[-1]
        print('Found hit filename: {0}'.format(self.hitfn))   
        
    def _read_hits(self,sign):
        df = pd.read_csv("{0}/{1}".format(self.dir,self.hitfn),
                         delim_whitespace=True,usecols=[1,6],names=['pid','x'], error_bad_lines=False)
        
        df = df.assign(b=df['x'].apply(self._get_impact_par))
        self.hit = df
        self._filter_hits(sign)
        
    def _get_impact_par(self,x):
        x    = abs(x)
        dx   = (x-self.hgap)*1e4
        return dx
        
    def _filter_hits(self,sign):
        '''Selects entries with the desired impact parameters'''
        if sign>0:
            self.hit = self.hit[(self.hit['x']>0) & (self.hit['b']<self.b1) & (self.hit['b']>self.b0)]
        else:
            self.hit = self.hit[(self.hit['x']<0) & (self.hit['b']<self.b1) & (self.hit['b']>self.b0)]
        
    def _read_ini(self):
        df2 = pd.read_csv("{0}/{1}".format(self.dir,'initial.dat.gz'),
                          delim_whitespace=True, header=None, error_bad_lines=False)

        cols = []
        for i in range(len(df2.columns)):
            if i==0:
                _j = 'pid'
            else:
                _j = i
            cols.append(_j)

        df2.columns = cols
        df2['pid']  = df2.index+1
        self._ini   = df2
        
    def _get_filtered_ini(self,s):
        self._read_hits(s)
        self._read_ini()
        self._filteredini = pd.merge(self.hit[['pid']],self._ini,on='pid')
        self._add_filtered_ini()
        
    def _add_filtered_ini(self):
        if self.newini is None:
            self.newini = self._filteredini
        else:
            self.newini = self.newini.append(self._filteredini, ignore_index=True)
        pass

    def save_newini(self,i):
        # make new directory if it doesn't exist
        
        self.newini['pid']  = self.newini.index+1
        
        outputdir = "{0}/new_initial".format(self.maindir)
        if not os.path.exists(outputdir):        
            os.makedirs(outputdir)
        lr = {-1:'right',1:'left'}
        self.newini.to_csv("{0}/initial_{1}.dat".format(outputdir,lr[i]),
                           sep='\t', index=False, header=False)
        print("wrote file to {0}/initial_{1}.dat".format(outputdir,lr[i]))

    def read_all(self,verbose=False):
        for s in [-1, 1]:
            print('Performing analysis for jaw {0}...'.format(s))
            self._dirs   = glob.iglob("{0}/run_*".format(self.maindir))
            self.newini  = None      
            while True:
                try:
                    self._next()
                except StopIteration:
                    break
                if verbose:
                    print("Processing {0}".format(self.dir))
                try:
                    self._get_filtered_ini(s)
                except FileNotFoundError:
                    continue
            self.save_newini(s)


def floor(x):
    return int(x - x % 1)
            
def multiply(path,npart):
    df = pd.read_csv(path,delim_whitespace=True,header=None, error_bad_lines=False)
    newdf = df
    
    # get number of required iterations
    iters = floor(np.log2(npart/float(len(df))))
    print("iterations", iters)

    for i in range(iters):
        if len(newdf)<npart:
            newdf = newdf.append(newdf,ignore_index=True)
        else:
            break    
    remaining_rows = npart-len(newdf)
    newdf_short    = newdf.head(remaining_rows)
    newdf = newdf.append(newdf_short,ignore_index=True)
    newdf[0] = newdf.index+1
    newdf.to_csv("{0}.big".format(path), sep='\t', index=False, header=False)
    print("Desired length: {0} - Final length: {1}".format(npart,len(newdf)))


          
try:
    opts, args = getopt.getopt(sys.argv[1:], "hp:b:B:g:en:", ["help","path=", "b1=", "b2=", "halfgap=",
                                                                    "existing", "npart="])
except getopt.GetoptError as err:
    # print help information and exit:
    print("Error: option not recognized")
    usage()
    sys.exit(2)
    output = None
    verbose = False
for o, a in opts:
    if o in ["-h", "--help"]:
        usage()
        sys.exit(0)
    elif o in ["-p", "--path"]:
        path = a
    elif o in ["--b1"]:
        b1   = float(a)
    elif o in ["--b2"]:
        b2   = float(a)
    elif o in ["-g", "--halfgap"]:
        halfgap = float(a)
    elif o in ["-e", "--existing"]:
        existing=True
    elif o in ["-n", "--npart"]:
        npart = int(a)

    else:
        assert False, "unhandled option"
        # ...


if not existing:
    create_new_ini_files(path,b1,b2,halfgap)
else:
    multiply(path,npart)

