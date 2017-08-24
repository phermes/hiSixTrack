import sys, glob, os
import pandas as pd

### COMPRESS THE HISIXTRACK OUTPUT


if len(sys.argv)==1:
    print("ERROR: no directory given")
    print("Usage: ")
    print("python compress.py path/to/directory")
    sys.exit(2)

print("Running compression script for directory: {0}".format(str(sys.argv[1])))




def read_twiss(tfname):
    for line in open(tfname):
        if 'KEYWORD' in line:
            cols=line.split()[1:]
            break
    twiss = pd.read_csv(tfname,
                    skiprows=47,
                    delim_whitespace=True,
                    names=cols)
    twiss = twiss[twiss['KEYWORD']=='RCOLLIMATOR']
    return twiss[['NAME','S']]

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
                
        # get the filename of the twiss file in input
        self._find_twiss_filename()
              
    def _next(self):
        self.dir = next(self._dirs)
        
    def _read_fort208(self):
        df = pd.read_csv("{0}/{1}".format(self.dir,'fort.208.gz'), delim_whitespace=True, 
                         usecols=[0,2],names=['colid','energy'])
        return df
    
    def _find_correction(self):
        self.corrfn = glob.glob('{0}/*fort.66.gz'.format(self.dir))[0].split('/')[-1]
        print('Found correction filename: {0}'.format(self.corrfn))
        
    def _find_twiss_filename(self):
        print('Searching twiss file in: {0}/../../input/'.format(self.maindir))

        self.twissfn = glob.glob('{0}/../../input/*.tfs'.format(self.maindir))[0].split('/')[-1]
        print('Found twiss file: {0}'.format(self.twissfn))
        
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
        
        self._get_coll_pos()
        
        # make new directory if it doesn't exist
        outputdir = "{0}/summary".format(self.maindir)
        if not os.path.exists(outputdir):        
            os.makedirs(outputdir)
            
        # save summary to new directory
        print("Writing output files to {0}".format(outputdir))
        self.coll.to_csv("{0}/fort.208".format(outputdir),sep='\t', index=False, header=False)
        self.apun.to_csv("{0}/fort.999".format(outputdir),sep='\t', index=False, header=False)
        self.aper.to_csv("{0}/fort.999.comp".format(outputdir),sep='\t', index=False, header=False)

        print("Summary: ")
        print("Successfully read runs: {0}".format(self.successful))
        print("Unsuccessful read attempts: {0}".format(self.unsuccessful))
        print("Output written to {0}".format(outputdir))

    def _get_coll_pos(self):
        # read the twiss file to get collimator id and position
        twiss = read_twiss('{0}/../../input/{1}'.format(self.maindir,self.twissfn))

        # read the collimator list to get id and name
        clist = pd.read_csv('{0}/../../input/{1}'.format(self.maindir,'fort3.list'),
                             delim_whitespace=True,
                             usecols=[0,2],
                             names=['NAME','colid'])
        clist['NAME'] = clist['NAME'].apply(lambda x:x.upper())

        # merge on common name
        df2 = pd.merge(twiss, clist, on='NAME', how='outer')
        df2 = df2[df2['colid']>0]

        # merge with the losses
        df3 = pd.merge(df2,self.coll, on='colid', how='outer')
        df3 = df3[df3.energy>0]

        self.coll = df3

        
    def read_losses(self,verbose=False):
        '''produce compressed loss files for all studies in the selected directory'''
        while True:
            try:
                self._next()
            except StopIteration:
                break
            if verbose:
                print("Processing {0}".format(self.dir))
            self._add_lossmap_data()
        
        self._finalize()



h = hisix_result(str(sys.argv[1]))
h.read_losses(verbose=True)
