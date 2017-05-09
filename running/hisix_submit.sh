#!/bin/bash



# basic definitions for initialization

FLUPRO=/afs/cern.ch/project/fluka/flukadev
ncycles=4          


function how_to_use() {
    cat <<EOF
   `basename $0` [action] [option]

    actions
    -i      run for initial distribution
    -s      submit hisix jobs for production run
    -d      debugging run
    options
    -t      produce touches output
            only compatible with option -s 
    -n      specify the number of runs. first time used will define nmin, second time nmax
            example: `basename ${0}` -s -n 30 -n 555 
                     will launch a production run starting from job count 30 to 555
    -I      specify FLUKA input file name. by default the input from the hisix.conf will be read
    -L      specify the name of the limi file. by default it is set to "fort3.limi"
    -k      specify the number of cycles per run, default is 4 [not yet implemented]
    -f      do return the list of files when copying back
    -l      run the script locally, no submission to LSF

EOF
}


function define_runs(){
    if ${linitial}; then
	nmin=0
	nmax=1000
    elif ${lsubmit}; then
	nmin=0
	nmax=30000
    elif ${ldebug}; then
	nmin=0
	nmax=1
    fi

    if ${lnruns}; then

	nmin=${nruns[0]}

	if [ ! -z ${nruns[1]} ]; then
	    nmax=${nruns[1]}
	fi

	echo "User selected nmin=${nmin} and nmax=${nmax}"
	
    fi
    }


function source_conf(){
    # read the settings from the hisixtrack input file
    source hisix.conf
    
    }


function initialize(){

    local __inilines
    local __qcontinue=false

    if [ ! -e hisix.conf ]; then
	echo "ERROR: hisix.conf was not found. Aborting..."
	exit 1
    else
	echo "Reading hisix.conf"
        source_conf
    fi

    #### FORT.3 CHECKS
    
    if [ ! -e fort.3 ]; then
	echo "ERROR: fort.3 was not found. Aborting..."
	exit 1
    else
	echo "Checking fort.3"
	if ! grep -q 'HION' fort.3; then
	    echo "ERROR: fort.3 requires a HION block. Aborting..."
	    exit 1
	else
	    echo "Found fort.3 file. Reading content."
	    read_fort3
	fi
    fi    

    #### INITIAL DISTRIBUTION CHECKS
    
    # check if the intial distribution filename was assigned in hisix.config
    # otherwise, set it to intial.dat
    if [ -z ${initial_filename} ]; then
	echo 
	echo "Initial distribution filename not found in hisix.config"
	echo "Assigning initial distribution filename: initial.dat"
	echo
	initial_filename="initial.dat"
    else
	echo
	echo "Initial distribution filename assigned to: ${initial_filename}"
	echo
    fi

    # check if the intial distribution file exists. Abort if not.
    if [ ! -e ${initial_filename} ]; then
	echo "ERROR: ${initial_filename} not found. Aborting..."
	exit 1
    else
	__inilines=$(less ${initial_filename} | wc -l)
	echo "Found the initial distribution file: ${initial_filename}"
	echo "containing ${__inilines} lines."
    fi    

    #### FLUKA FILENAME CHECKS

    # check if the fluka filename is defined in the conf file or if the option is used
    if [ -z ${fluka_filename} ] && ! ${lflukain}  ; then
	echo "ERROR: fluka_filename not defined in hisix.conf and no -F option given. Aborting..."
	exit 1
    fi    
    
    # get the fluka filename if option is given
    if ${lflukain}; then
	fluka_filename=${fluka_input}
	echo "User selected the following FLUKA input file: ${fluka_filename}"
    fi

    #### LIMI FILE CHECKS
    
    # re-define the name of the limi file
    if ${lsetlimi}; then
	limifile=${limifn}
	echo "User selected the following limi file: ${limifile}"
    else
	echo "Using the standard limi file: fort3.limi"
	limifile="fort3.limi"
    fi
    
    #### QUEUE CHECKS
    
    # re-define the name of the queue to be used
    if ${lsetqueue}; then
	queue=${setqueue}
	echo "User selected the following queue: ${queue}"
    else
	echo "Using standard queue 1nd"
	queue="1nd"
    fi

    
    #### WORKDIR CHECKS

    # stop script if workspace isn't defined anywhere
    if [ -z ${workdir} ] &&  ! ${lsetworkdir}  ; then
	echo "ERROR: workdir not defined in hisix.conf and no -W option given. Aborting..."
	exit 1 
    fi      
    
    # re-define the name of the workdir to be used
    if ${lsetworkdir}; then
	workdir=${setworkdir}
	echo "User selected the following workdir: ${workdir}"
    elif ${linitial}; then
	workdir="initial"
	echo "Initial run and no other workdir defined: workdir=initial"
    elif ${ldebug}; then
	workdir="debug"
#	echo "debugging run and no other workdir defined"	
    fi    

    # check if the workdir already extists. print warning message if so
    if [ -d ${workdir} ]; then
	echo "WARNING: directory ${workdir} already existing."
	echo "Submitting jobs to an existing workdir may cause data loss!"	
	read -p "Continue (y/n)?" choice
	case "$choice" in 
	    y|Y ) __qcontinue=true;;
	    n|N ) __qcontinue=false ;;
	    * ) echo "invalid"; __qcontinue=false;;
	esac

	if ! ${__qcontinue}; then
	    echo "User stopped the submission"
	    exit 1
	fi
    fi
	
    

    #### get the number of runs
    define_runs


    #### PRINT SUMMARY
    
    echo
    echo "Summary of the hisixtrack initialization"
    echo
    echo "------- RUN INFORMATION --------"
    echo "runtype:         ${runtype}"
    echo "workdir:         ${workdir}"    
    echo "nmin:            ${nmin}"
    echo "nmax:            ${nmax}"
    echo "FLUKA input:     ${fluka_filename}"
    echo "queue:           ${queue}"
    echo "cycles:          ${ncycles}"
    echo "limi file:       ${limifile}"
    if ${ltouches}; then
	echo "touches:         selected   "
    else
	echo "touches:         not selected"
    fi

    echo
    echo "------- ION INFORMATION --------"    
    echo "Mass number A:   ${ion_species[0]}"
    echo "Charge number Z: ${ion_species[1]}"
    echo "Mass [GeV/c^2]:  ${ion_species[2]}"
    echo 


    if ${linitial}; then
	prepare_initial
    fi
    
    }


function prepare_initial(){

    echo "Preparing FLUKA input for initial run"
    # produce the fluka file for the initial run
    oldstr="ASSIGNMA    AC150GPH  TCP_Jawl"
    newstr="ASSIGNMA    HYDROGEN  TCP_Jawl"
    
    sed -e "s/${oldstr}/${newstr}/g" ${fluka_filename}.inp > ${fluka_filename}_ini.inp
    


}


function read_fort3(){
    
    ion_species=($(sed -e '1,/HION/d' fort.3 | sed -e '/NEXT/,$d' |  sed '/[/]/d'))
 
}




function produce_retscript(){


    if ${linitial}; then
	echo "Producing return script for initial run"
	cat > returnscript.sh <<EOF
        # return script for initial distribution
	rm                                 \${RUNDIR}/\${NAMENUMDIR}/*
        gzip  *
	cp    *.67*                        \${RUNDIR}/\${NAMENUMDIR}   
	cp    initial.dat*                 \${RUNDIR}/\${NAMENUMDIR}   
        if ${lreturnls}; then
           ls > filenames.dat
           cp filenames.dat                \${RUNDIR}/\${NAMENUMDIR}   
        fi
	exit 0 
EOF
    elif ${ldebug}; then
	echo "Producing return script for debugging run"
    	cat > returnscript.sh <<EOF
        # return script for debugging
        rm                            \$RUNDIR/\${NAMENUMDIR}/*
        cp -r *                       \$RUNDIR/\${NAMENUMDIR}   
        exit 0	
EOF
    elif ${lsubmit}; then
	echo "Producing return script for production run"	
	cat > returnscript.sh <<EOF
        # return script for production run
        rm                           \$RUNDIR/\${NAMENUMDIR}/*
        ls                        >  \$RUNDIR/\${NAMENUMDIR}/files.dat
        gzip * 
        cp fort.999.gz               \$RUNDIR/\${NAMENUMDIR}   # aperture losses
        cp fort.208.gz               \$RUNDIR/\${NAMENUMDIR}   # collimator losses
        cp fort.209.gz               \$RUNDIR/\${NAMENUMDIR}   # collimator IDs
        cp *tracks*                  \$RUNDIR/\${NAMENUMDIR}   # collimator IDs
        cp fort.822.gz               \$RUNDIR/\${NAMENUMDIR}   # generated isotopes
        cp *.66.gz                   \$RUNDIR/\${NAMENUMDIR}   # correction file
        cp *toucMap*                 \$RUNDIR/\${NAMENUMDIR}   # correction file
EOF

    fi

    RETSCRIPT="returnscript.sh"
    


    }







linitial=false
lsubmit=false
lnruns=false
lflukain=false
lsetlimi=false
lsetqueue=false
ltouches=false
ldebug=false
lsetworkdir=false
llocal=false
lreturnls=false

# get options (heading ':' to disable the verbose error handling)
while getopts  "hidltfsn:I:L:q:W:" opt ; do
    case $opt in
	i)
	    linitial=true
	    runtype="initial"
	    ;;
	l)
	    llocal=true
	    ;;
	t)
	    ltouches=true
	    ;;
	f)
	    lreturnls=true
	    ;;		
	s)
	    lsubmit=true
	    runtype="production"
	    ;;
	d)
	    ldebug=true
	    runtype="debug"
	    ;;	
	I)
	    lflukain=true
	    fluka_input=${OPTARG}
	    ;;
	L)
	    lsetlimi=true
	    limifn=${OPTARG}
	    ;;
	W)
	    lsetworkdir=true
	    setworkdir=${OPTARG}
	    ;;	
	q)
	    lsetqueue=true
	    setqueue=${OPTARG}
	    ;;			
	n)
	    lnruns=true
	    nruns+=("$OPTARG")
	    ;;	
	h)
	    how_to_use
	    exit 1
	    ;;
	:)
	    how_to_use
	    echo "Option -$OPTARG requires an argument."
	    exit 1
	    ;;
	\?)
	    how_to_use
	    echo "Invalid option: -$OPTARG"
	    exit 1
	    ;;
    esac
done
shift "$(($OPTIND - 1))"


# user's request
# - actions
if ! ${linitial} && ! ${lsubmit} && ! ${ldebug} ; then
    how_to_use
    echo "No action specified!!! aborting..."
    exit 1
fi



### MAIN



initialize
produce_retscript

#exit

if ${ldebug}; then

    if ${ltouches}; then
	options="${options} -a relcol.dat"
    fi

    echo "Submitting FLUKA pilot run"
    ../tools/running/hisix_submit.sh -I ${fluka_filename} -q ${queue} -j ${nmax} -k ${ncycles} -e ${RETSCRIPT} -a ${limifile} -f ${FLUPRO} -r ${workdir} -F ${nmin} ${options}
    
fi


if ${linitial}; then
    if ! ${llocal}; then
	echo "Submitting"
	../tools/running/hisix_submit.sh -I ${fluka_filename}_ini -q ${queue} -j ${nmax} -k ${ncycles} -e ${RETSCRIPT} -a ${limifile} -f ${FLUPRO} -r ${workdir}
    else
	echo "Local run selected. "
    fi
fi
   
if ${lsubmit}; then

    if ${ltouches}; then
	options="${options} -a relcol.dat"
    fi

    echo "Submitting FLUKA pilot run"
    ../tools/running/hisix_submit.sh -I ${fluka_filename} -q ${queue} -j ${nmax} -k ${ncycles} -e ${RETSCRIPT} -a ${limifile} -f ${FLUPRO} -r ${workdir} -F ${nmin} ${options}
    
fi





exit 0






