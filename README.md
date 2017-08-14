# hiSixTrack

Source files and tools for running the hiSixTrack-FLUKA coupling to simulate heavy-ion collimation. 


# Installation

This github repository contains only files which are modified with respect to the nominal SixTrack-FLUKA coupling for protons. The first step is hence to check out the nominal coupling:

	svn co svn+ssh://svn.cern.ch/reps/fluka_coupling/trunk ion_coupling

Then switch to the directory ion_coupling and clone the hiSixTrack github repository

	cd ion_coupling
	git clone http://www.github.com/phermes/hiSixTrack 
	

Run the installation script for hiSixTrack
	
	./hiSixTrack/src/tools/install/install.sh
	
This script will copy over the necessary hiSixTrack and FLUKA files to allow for the tracking and particle matter interaction simulation of heavy ions instead of protons. 

# Compilation

With the files copied over, the compilation can be started. To compile hiSixTrack we switch to the directory sixtrack and perform the compilation command:

	cd sixtrack
	./make_six gfortran fluka -crlibm -cernlib backtrk
