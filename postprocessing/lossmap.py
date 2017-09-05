import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys


WarmRegions=np.array([[  0.00000000e+00,   2.25365000e+01],
       [  5.48530000e+01,   1.52489000e+02],
       [  1.72165500e+02,   1.92400000e+02],
       [  1.99484700e+02,   2.24300000e+02],
       [  3.09545428e+03,   3.15562858e+03],
       [  3.16774008e+03,   3.18843308e+03],
       [  3.21144458e+03,   3.26386758e+03],
       [  3.30990008e+03,   3.35497408e+03],
       [  3.40100558e+03,   3.45342858e+03],
       [  3.47644008e+03,   3.49406558e+03],
       [  3.50588528e+03,   3.56831858e+03],
       [  6.40540880e+03,   6.45791380e+03],
       [  6.46877850e+03,   6.85951380e+03],
       [  6.87037850e+03,   6.92353380e+03],
       [  9.73590702e+03,   9.82473052e+03],
       [  9.83083202e+03,   9.86173052e+03],
       [  9.87873202e+03,   9.93998552e+03],
       [  9.95054802e+03,   1.00434620e+04],
       [  1.00540245e+04,   1.01152780e+04],
       [  1.01322795e+04,   1.01639705e+04],
       [  1.01700720e+04,   1.02576030e+04],
       [  1.31049892e+04,   1.31298045e+04],
       [  1.31368892e+04,   1.31571237e+04],
       [  1.31768002e+04,   1.32716472e+04],
       [  1.33067527e+04,   1.33518257e+04],
       [  1.33869312e+04,   1.34817782e+04],
       [  1.35014547e+04,   1.35227845e+04],
       [  1.35298692e+04,   1.35546845e+04],
       [  1.63946378e+04,   1.64508713e+04],
       [  1.64569728e+04,   1.64872713e+04],
       [  1.64933728e+04,   1.68308713e+04],
       [  1.68369728e+04,   1.68672713e+04],
       [  1.68733728e+04,   1.69282948e+04],
       [  1.97740644e+04,   2.02179087e+04],
       [  2.30899797e+04,   2.31385770e+04],
       [  2.31503967e+04,   2.31713755e+04],
       [  2.31943870e+04,   2.32468100e+04],
       [  2.32928425e+04,   2.33379155e+04],
       [  2.33839480e+04,   2.34363710e+04],
       [  2.34593825e+04,   2.34800825e+04],
       [  2.34921940e+04,   2.35531160e+04],
       [  2.64334879e+04,   2.64583032e+04],
       [  2.64653879e+04,   2.64867177e+04],
       [  2.65063942e+04,   2.66012412e+04],
       [  2.66363467e+04,   2.66588832e+04]])


### CHECK ARGUMENTS

nargs = len(sys.argv)

if nargs<2:
    print('ERROR: no hisix directory given; ABORT')
    exit()
elif nargs == 2:
    path = str(sys.argv[1])
    fac  = 1
elif nargs == 3:
    path = str(sys.argv[1])
    fac  = int(sys.argv[2])

print(path)
    

def assign_warm_cold(s):
    for i in WarmRegions:
        if (s>i[0]) and (s<i[1]):
            return 'red'
    return 'blue'

class plot_hisix:
    def __init__(self,path, fac):
        self.path = path
        self.read_collimator_losses()
        self.read_aperture_losses()
        if fac==2:
            self.coll['s'] = 26658.8832 - self.coll['s']
            self.aper['s'] = 26658.8832 - self.aper['s']
        
        self.plot_losses()
    def read_collimator_losses(self):
        self.coll = pd.read_csv('{0}/summary/fort.208'.format(self.path),
                    delim_whitespace=True,names=['name','s','colid','energy'])
        self.norm = self.coll['energy'].max()
        
    def read_aperture_losses(self):
        self.aper = pd.read_csv('{0}/summary/fort.999'.format(self.path),
                    delim_whitespace=True,usecols=[0,1],names=['s','energy'])
        self.aper = self.aper.assign(temp=self.aper['s'].apply(assign_warm_cold))
        
    def plot_losses(self):
        plt.bar(self.coll['s'],self.coll['energy']/self.norm,edgecolor='k',width=1)

        for c in ['blue','red']:
            _data = self.aper[self.aper['temp']==c]
            plt.bar(_data['s'].values,_data['energy'].values/(self.norm*0.1),edgecolor=c,width=0.1)

        plt.yscale('log')
        plt.grid(axis='hor')
        plt.xlabel('Position [m]')
        plt.ylabel(r'$\eta [1/m]$')
        plt.grid(axis='y')
        plt.xlim(0,26658.88)
        plt.show()

        
plot_hisix(path, fac)
