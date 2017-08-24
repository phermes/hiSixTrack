import sys, glob, os
import pandas as pd

### COMPRESS THE HISIXTRACK OUTPUT


if len(sys.argv)==1:
    print("ERROR: no directory given")
    print("Usage: ")
    print("python compress.py path/to/directory")
    sys.exit(2)

print("Running compression script for directory: {0}".format(str(sys.argv[1])))


class hisix_result:
    def __init__(self, dirname):
        self.coll = None      # compressed collimator loss data
        self.aper = None      # compressed aperture loss data
        self.apun = None      # uncompressed aperture loss data
        self.corr = None      # correction data
        
        self.successful, self.unsuccessful = 0, 0       
        self._dirs   = glob.iglob("{0}/run_*".format(dirname))
        self.maindir = dirname
        
        while True:
            try:
                self._next()
                self._find_correction()
                break
            except IndexError:
                continue
              
    def _next(self):
        self.dir = next(self._dirs)
        
    def _read_fort208(self):
        df = pd.read_csv("{0}/{1}".format(self.dir,'fort.208.gz'), delim_whitespace=True, 
                         usecols=[0,2],names=['colid','energy'])
        return df
    
    def _find_correction(self):
        self.corrfn = glob.glob('{0}/*fort.66.gz'.format(self.dir))[0].split('/')[-1]
        print('Found correction filename: {0}'.format(self.corrfn))
        
    def _read_correction(self):
        df = pd.read_csv("{0}/{1}".format(self.dir,self.corrfn), delim_whitespace=True, 
                         usecols=[3,4],names=['colid','energy'])
        return df
    
    def _read_fort999(self):
        df = pd.read_csv("{0}/{1}".format(self.dir,'fort.999.gz'), delim_whitespace=True, 
                         usecols=[4,12,15,16],names=['s','energy','A','Z'])
        return df
    
    def _add_lossmap_data(self):
        # read the fort.208 and fort.999 and skip if either of the files is not existing
        try:
            _newap = self._read_fort999()
            _newco = self._read_fort208()
            _newcr = self._read_correction()
        except FileNotFoundError:
            self.unsuccessful+=1
            return
            
        # initialise the self.coll or add the values to it
        if self.coll is None:
            self.coll = _newco
            self.aper = _newap
            self.apun = _newap
            self.corr = _newcr
        else:
            self.coll = self.coll.append(_newco, ignore_index=True)
            self.coll = self.coll.groupby(self.coll['colid'],as_index=False).sum()
            
            self.corr = self.corr.append(_newcr, ignore_index=True)
            self.corr = self.corr.groupby(self.corr['colid'],as_index=False).sum()
            
            self.apun = self.aper.append(_newap, ignore_index=True)            
            self.aper = self.aper.append(_newap, ignore_index=True)
            self.aper = self.aper.groupby(self.aper['s'], as_index=False).sum()
        self.successful+=1
    
    def _finalize(self):
        # make correction energy negative
        self.corr['energy'] = self.corr['energy'].apply(lambda x: (-1)*x)
        
        # apply to the coll dataframe
        self.coll = self.coll.append(self.corr, ignore_index=True)
        self.coll = self.coll.groupby(self.coll['colid'],as_index=False).sum()
        
        # make new directory if it doesn't exist
        outputdir = "{0}/summary".format(self.maindir)
        if not os.path.exists(outputdir):        
            os.makedirs(outputdir)
            
        # save summary to new directory
        print("Writing output files to {0}".format(outputdir))
        self.coll.to_csv("{0}/fort.208".format(outputdir),sep=' ', index=False, header=False)
        self.apun.to_csv("{0}/fort.999".format(outputdir),sep=' ', index=False, header=False)
        self.aper.to_csv("{0}/fort.999.comp".format(outputdir),sep=' ', index=False, header=False)

        print("Summary: ")
        print("Successfully read runs: {0}".format(self.successful))
        print("Unsuccessful read attempts: {0}".format(self.unsuccessful))

        
    def read_losses(self):
        '''produce compressed loss files for all studies in the selected directory'''
        while True:
            try:
                self._next()
            except StopIteration:
                break
            print("Processing {0}".format(self.dir))
            self._add_lossmap_data()
        
        self._finalize()


h = hisix_result(str(sys.argv[1]))
h.read_losses()
