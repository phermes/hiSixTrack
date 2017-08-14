#!/bin/bash

# basic definitions for initialization

FLUPRO=/afs/cern.ch/project/fluka/flukadev
ncycles=4
platform=htcondor
ACCEPTED_PLATFORMS=( htcondor lsf ) 

function how_to_use() {
    cat <<EOF
   `basename $0` [action] [option]

    actions
    -w      Specify workdir [mandatory]
    -N      Create new study with the argument as the name of the new study
    -i      Run for initial distribution 
            Argument: collimator at which the initial distribution should be sampled
    -s      Submit hisix jobs for production run
    -d      Debugging run
    options
    -t      Produce touches output
            Only compatible with option -s 
    -n      Specify the number of runs. first time used will define nmin, second time nmax
            Example: `basename ${0}` -s -n 30 -n 555 
                     will launch a production run starting from job count 30 to 555
    -I      Specify FLUKA input file name. By default the name of the study directory is used.
    -L      Specify the name of the limi file. By default it is set to "fort3.limi"
    -k      Specify the number of cycles per run, default is 4 [not yet implemented]
    -f      Do return the list of files when copying back
    -l      Run the script locally, no submission to LSF

EOF
}




# We assume the following directory structure
# /path/to/hisix_study/
#   hisix.sh




#### FUNCTIONS FOR CREATING A NEW STUDY


function create_new_study(){

    echo "Creating new study"
    
    if [ -d ${newstudyname} ]; then
	echo "ERROR: Directory ${newstudyname} exists!"
	echo "       Please use another name for your new study."
	exit 1
    fi

    mkdir ${newstudyname}
    mkdir ${newstudyname}/input
    mkdir ${newstudyname}/runs

    
    cat > ${newstudyname}/input/REQUIRED_INPUT_FILES <<EOF
The following files are required to be stored in the input directory to run hiSixTrack

fort.2
fort.3
fort3.limi
fort3.list
initial.dat
insertion.txt
studyname.inp
twissfile.tfs

EOF
    

    echo "Directory ${newstudyname} created"
    exit 0

}



#### FUNCTIONS FOR INITIALIZATION


function hello(){
    echo
    echo "hiSixTrack control "
    echo "V. 0.3 [11/JUL/2017]"
    echo
}



function initialize(){

    local __inilines
    local __qcontinue=false

    
    #### CHECK THE PLATFORM [redundant, already performed in hisix_submit_ht]
    
    for POSSIBLE_PLATFORM in ${ACCEPTED_PLATFORMS[@]} ; do
	if [ "${platform}" == "${POSSIBLE_PLATFORM}" ] ; then
	    PLATFORM_SELECTED=true
	    break
	fi
    done
    if ! ${PLATFORM_SELECTED} ; then
	echo "No valid platform selected: ${platform}"
	exit 1
    else
	echo "Selected Platform: ${platform}"
    fi

    #### GET THE TIMESTAMP AND DEFINE THE RUN DIRECTORY NAMES

    mkdir -p ${workdir}/runs
    timestamp=$(date +%Y_%m_%d_%H_%M_%S)
    inirun="runs_ini_${timestamp}"
    dbgrun="runs_debug_${timestamp}"
    prorun="runs_production_${timestamp}"        
    

    #### PREPARE THE DIRECTORY WITH THE INPUT FOR THE PRESENT SIMULATION
    
    mkdir -p latest_run                                             # contains the input file for the latest run

    if [ -e latest_run/zip_latest_run.zip ]; then
	rm latest_run/zip_latest_run.zip                            # remove previous backup
	echo "Removed latest_run/zip_latest_run.zip"
    fi
    
                                                                    # back up old data from latest_run
    if test "$(ls -A "latest_run/")"; then
	zippedfiles=$(zip zip_latest_run.zip latest_run/* | wc -l)
	echo "Backed up ${zippedfiles} files"
	mv zip_latest_run.zip latest_run/	
    fi

    echo 
    
    #### INITIAL DISTRIBUTION CHECKS

    initial_filename="initial.dat"
    # check if the intial distribution file exists. Abort if not.
    if [ ! -e ${workdir}/input/${initial_filename} ]; then
	echo "ERROR: ${workdir}/input/${initial_filename} not found. Aborting..."
	exit 1
    else
	echo "Found the initial distribution file: ${initial_filename}"
    fi    


    #### FORT.3 CHECKS
    
    if [ ! -e ${workdir}/input/fort.3 ]; then
	echo "ERROR: fort.3 was not found. Aborting..."
	exit 1
    else
	echo "Checking fort.3"
	if ! grep -q 'HION' ${workdir}/input/fort.3; then
	    echo "ERROR: fort.3 requires a HION block. Aborting..."
	    exit 1
	else
	    echo "Found fort.3 file. Reading content."
	    read_fort3
	fi
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
    
    
    if ${lsetqueue}; then                                             # re-define the name of the queue to be used
	queue=${setqueue}
	echo "User selected the following queue: ${queue}"
    else
	echo "Using standard queue 1nd"
	if [ "${platform}" == "lsf" ]; then
	    queue="1nd"
	elif [ "${platform}" == "htcondor" ]; then
	    queue="tomorrow"
	fi
    fi
    
    #### GET NUMBER OF RUNS
    
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


#### FUNCTIONS FOR INPUT PREPARATION


function adjust_fort3(){

#    cd $workdir
    echo "Preparing fort.3"
    cp ${workdir}/input/fort.3 latest_run/newfort.3
    cd latest_run
    
    if ${linitial}; then
	tcpline=$(grep ${targettcp} ../${workdir}/input/fort3.list)
	sed -i '/TRAC/,/NEXT/c\TRAC\n1 0 740 0.0 0.0 0 1 \n1 1 3 0 0 \n0 0 100000 100000 100000 100000 1\nNEXT' newfort.3
	sed -i "/FLUKA/,/NEXT/c\FLUKA\n ${tcpline}\nNEXT"  newfort.3
	mv newfort.3 fort.3
    elif ${lsubmit}; then
	sed -i '/TRAC/,/NEXT/c\TRAC\n700 0 70 0.0 0.0 0 1 \n1 1 3 0 0 \n0 0 100000 100000 100000 100000 1\nNEXT'  newfort.3
	mv newfort.3 fort.3
    elif ${ldebug}; then
	sed -i '/TRAC/,/NEXT/c\TRAC\n1 0 1 0.0 0.0 0 1 \n1 1 3 0 0 \n0 0 100000 100000 100000 100000 1\nNEXT' newfort.3
	sed -i "/FLUKA/,/NEXT/c\FLUKA\ntcp.c6l7.b1          ecp.c6l7.b1            30    1.48200\nNEXT"  newfort.3
	mv newfort.3 fort.3
    fi
    
    cd ..
    
}








function read_fort3(){
    
    ion_species=($(sed -e '1,/HION/d' ${workdir}/input/fort.3 | sed -e '/NEXT/,$d' |  sed '/[/]/d'))
 
}



#### CREATE THE SCRIPT TO RETURN THE DESIRED OUTPUT

function produce_retscript(){


    if ${linitial}; then
	RETSCRIPT="return.ini" 
	
	echo "Producing return script for initial run"
	cat > latest_run/${RETSCRIPT} <<EOF
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

	RETSCRIPT="return.debug"		
	echo "Producing return script for debugging run"	
    	cat > latest_run/${RETSCRIPT} <<EOF
        # return script for debugging
        #rm                            \$RUNDIR/../${workdir}/runs_debug/\${NAMENUMDIR}/*
        #cp -r *                       \$RUNDIR/../${workdir}/runs_debug/\${NAMENUMDIR}   
        cp -r *                       \$RUNDIR/\${NAMENUMDIR}   
        exit 0	
EOF

    elif ${lsubmit}; then
	RETSCRIPT="return.production"		
	echo "Producing return script for production run"	
	cat > latest_run/${RETSCRIPT} <<EOF
        # return script for production run
        # rm                           \$RUNDIR/\${NAMENUMDIR}/*
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

    
    


    }


function get_input(){
    cp ${workdir}/input/initial.dat latest_run/
    cp ${workdir}/input/fort.2 latest_run/
    cp ${workdir}/input/fort3.limi latest_run/
    cp ${workdir}/input/insertion.txt latest_run/
    if ${ldebug}; then
	cp ${workdir}/input/${fluka_filename}.inp latest_run/${fluka_filename}_debug.inp
    elif ${lsubmit}; then
	cp ${workdir}/input/${fluka_filename}.inp latest_run/
    fi
}



#### FUNCTIONS FOR RUNNING AN INITIAL SIMULATION


function prepare_initial(){

    echo "Preparing FLUKA input for initial run"
    # produce the fluka file for the initial run
    oldstr="ASSIGNMA    AC150GPH  TCP_Jawl"
    newstr="ASSIGNMA    HYDROGEN  TCP_Jawl"
    
    sed -e "s/${oldstr}/${newstr}/g" ${workdir}/input/${fluka_filename}.inp > latest_run/${fluka_filename}_ini.inp
    
}



#### MISC FUNCTIONS 



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








lnewstudy=false
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
while getopts  "hi:dw:N:ltfsn:I:L:q:P:" opt ; do
    case $opt in
	i)
	    linitial=true
	    runtype="initial"
	    targettcp=${OPTARG}
	    ;;
	N)
	    lnewstudy=true
	    newstudyname=${OPTARG}
	    ;;	
	l)
	    llocal=true
	    ;;
	P)
	    platform=${OPTARG}
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
	    fluka_filename=${OPTARG}
	    ;;
	L)
	    lsetlimi=true
	    limifn=${OPTARG}
	    ;;
	w)
	    lsetworkdir=true
	    workdir=${OPTARG}
	    fluka_filename=${workdir}
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


hello


if ! ${lsetworkdir} && ! ${lnewstudy}; then
    echo "ERROR: You haven't specified the workdir."
    echo "       Please run hisix.sh -w workdir [other options and actions]"
    echo "       Use the -h option to see how to use hiSixTrack"
    exit 1
fi


# user's request
# - actions
if ! ${linitial} && ! ${lsubmit} && ! ${ldebug} && ! ${lnewstudy} ; then
    how_to_use
    echo "No action specified!!! aborting..."
    exit 1
fi



### MAIN

if ${lnewstudy}; then
    create_new_study
fi




initialize
produce_retscript
adjust_fort3
get_input



# do the actual submission

cd latest_run

if ${ldebug}; then

    if ${ltouches}; then
	options="${options} -a relcol.dat"
    fi

    echo "Submitting hiSixTrack-FLUKA debug run"
    ../../tools/running/hisix_submit_ht.sh -I ${fluka_filename}_debug -q ${queue} -j ${nmax} -k ${ncycles} -e ${RETSCRIPT} -a ${limifile} -f ${FLUPRO} -r ../${workdir}/runs/${dbgrun}/ ${options} -P ${platform}
    
fi


if ${linitial}; then
    if ! ${llocal}; then
	echo "Submitting hiSixTrack-FLUKA initial run"
	../../tools/running/hisix_submit_ht.sh -I ${fluka_filename}_ini -q ${queue} -j ${nmax} -k ${ncycles} -e ${RETSCRIPT} -a ${limifile} -f ${FLUPRO} -r ../${workdir}/runs/${inirun}/ -P ${platform}
    else
	echo "Local run selected. Not yet implemented."
    fi
fi
   
if ${lsubmit}; then

    if ${ltouches}; then
	options="${options} -a relcol.dat"
    fi

    echo "Submitting hiSixTrack-FLUKA production run"
    ../../tools/running/hisix_submit_ht.sh -I ${fluka_filename} -q ${queue} -j ${nmax} -k ${ncycles} -e ${RETSCRIPT} -a ${limifile} -f ${FLUPRO} -r ../${workdir}/runs/${prorun}/ ${options} -P ${platform}
    
fi





exit 0






