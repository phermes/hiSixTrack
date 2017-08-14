# hiSixTrack

Source files and tools for running the hiSixTrack-FLUKA coupling to simulate heavy-ion collimation. 


# Installation

This github repository contains only files which are modified with respect to the nominal SixTrack-FLUKA coupling for protons. 



compile hiSixTrack:
	./make_six gfortran fluka -crlibm -cernlib backtrk
