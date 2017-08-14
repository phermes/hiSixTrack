#!/bin/bash

# paths:
PROJECT_PATH=$PWD
MATLAB_PATH=/soft/matlab
if ( hostname | grep lxplus > /dev/null ); then 
    FLUPRO_PATH=/afs/cern.ch/project/fluka/flukadev
else
    FLUPRO_PATH=/home/soft/flukadev
fi
COUPLING_PATH=`dirname $0 | awk '{print ($1"/../../")}'`
COUPLING_PATH=$(readlink -f ${COUPLING_PATH})
CURRENT_PATH=$PWD


RUNLOG="log.runs"

# files:
# mandatory project files (needed by FLUKA):
ICO_MAND_PROJECT_FILES="optics.tfs config.txt"
SIX_MAND_PROJECT_FILES=""
SIX_FLK_MAND_PROJECT_FILES="insertion.txt"
# file with the entire population sampled with IcoSim:
INITIAL_BEAM_DIST="initial.dat"
# file with the population to be given to any SixTrack job:
SIX_SAMPLED_FILE="initial.dat"
# file containing info for the network communication:
netFile='network.nfo'
# additional files from the user:
ADD_PROJECT_FILES=()
# Post-processing file to be appended at the end of the job
POST_FILE=""
# Optional gpdist template for generating the beam distribution on runtime
BEAM_DIST_TEMPLATE=""
CREATE_BEAM_DIST=false

# in case of lxplus, store all the pre-processing in a dedicated job file, so
#    that jobs can be easily restarted without going through these stages
#    (eg update of random seeds, generation of input distribution...)
PREPROC="preproc.job"

# default values:
TMPDIR=
RFIRST=1
NJOBS=1
NCYCLES=1
let RLAST=$RFIRST+$NJOBS-1
JFIRST=1
RUN_IN_CURRENT=false
SPLIT_JOBS=false
OUTDIRNAME=""
ASSIGN_OUTDIRNAME=false
FLUKA_INP_NAME=""
ACCEPTED_PLATFORMS=( 'pbs' 'lsf' 'htcondor' )
if ( hostname | grep lxplus > /dev/null ); then 
    QUEUE_DEF="8nm"
    PLATFORM_DEF="lsf"
    POSSIBLE_QUEUEs=( "test" "8nm" "1nh" "8nh" "1nd" "2nd" "1nw" "2nw" )
else
    QUEUE_DEF="short"
    PLATFORM_DEF="pbs"
    POSSIBLE_QUEUEs=( "urgent" "short" "normal" "long" "external" "unlimited" )
fi
PLATFORM=${PLATFORM_DEF}
SIX_OR_ICO="s"
RUN_ON_LOCAL_MACHINE=false
DUMMY_FLUKA_SERVER=false
JOB_NAME='temp'
AT_LEAST_ONE_MISSING_FILE=false
QUEUE_SELECTED=false
BATCH_OPT=""
PLATFORM_SELECTED=false
QUEUE=""

# in order to store an empty string when issuing commands like:
#   BLA=*.sh
# and actually, there are no files
shopt -s nullglob

# other variables
svnLogFile=".svn.log"
NUMDIR_TEMPLATE="%05i"
DIRNAME_TEMPLATE="run_${NUMDIR_TEMPLATE}"
EXENAME_TEMPLATE="job_${NUMDIR_TEMPLATE}"
PBSNAME_TEMPLATE="coupling_${NUMDIR_TEMPLATE}_${NUMDIR_TEMPLATE}"
tmpPBSName='<tempPBSName>'

die() {
  echo >&2 "$1"
  exit $E_BADARGS
}

how_to_use() {
       script_name=`basename $0`
cat <<EOF

       ${script_name} [options]


       Script for running coupling simulations.
       By default, all the jobs are sent over a batch system.
       Multiple parallel jobs of a single case or group of jobs (cycles) 
       can be handled: the single case is located in a dedicated folder, 
       and each parallel job is contained in a dedicated subfolder 
       (with its own I/O files), e.g.:

            ./scraper/
              |_ run_0001/
              |_ run_0002/
              |_ run_0003/
       

       options:

       -a<additional_file> any additional file needed by the tracker/server

       -b<Gpdist template> Additional gpdist template for generating 
                           the beam distribution on runtime. By default, 
                           the beam distribution is taken from 'initial.dat'.
                           But, if a template is given, gpdist will be called and
                           a beam distribution file will be created for each parallel job.

       -c                  the single case is run in the current folder
                           (i.e. no directory containing the run case will be created)

       -e                  Includes an user-defined script file to be run after the coupling ends. 
                           It is useful to post-process the output files automatically once the simulation
                           is over.
			   
       -h                  print this help file
			   
       -i/s:               choice of tracker:
                           'i': icosim
                           's': sixtrack
                           --> default value: '${SIX_OR_ICO}'
			   
       -j<# jobs>          total number of jobs
                           --> default value: ${NJOBS}
			   
       -k<# cycles>        number of cycles per run
                           --> default value: ${NCYCLES}
			   
       -l                  run on a local machine, thus no submission to the PBS/LSF system.
                           The usual .job file is generated with all the original lines,
                           but it is run as a simple bash script;
	
       -o<Batch options>   set extra optional arguments for the submit script to the
                           PBS/LSF system. The options will be added before the jobs script.
                           Useful, for example, if you need to set memory/space limit in 
                           LSF system.
                           --> default value: No options;
		   
       -p<coupling path>   path to the coupling repository (only in case
                           a local copy of the submit script is used)
                           --> default value: '${COUPLING_PATH}'
			   
       -q<queue type>      queue on the PBS/LSF/HTCONDOR system
                           --> present platform: '${PLATFORM}'
                           --> possible queue values: '${POSSIBLE_QUEUEs[@]}'
                           --> default value: '${QUEUE_DEF}'
			   
       -r<run_directory>   the directory will be named as <run_directory>,
                           otherwise <job_name>.xxx
			   
       -2                  the Server job and the Tracker job will be
                              sent on two different CPUs
                           the default is to have both processes on
                              only one CPU

       options specific to the Server:

       -d                  dummy Server (not Fluka)

       -f<FLUPRO_PATH>     path to Fluka installation
                           --> default value: '${FLUPRO_PATH}'
       
       -I<Fluka .inp file> input file for fluka (without .inp)

       -P<platform>        platform where to run
                           available ones: ${ACCEPTED_PLATFORMS[@]}


       Additional remarks:
       . if the coupling job makes use of a dummy server (-d option),
         neither the .inp file (-I option) nor the specifiction of the FLUPRO path
         (-f option) are needed;
       . if the job is run locally (-l option) and Fluka is used, it may be necessary
         to overwrite the default value of the FLUPRO variable (-f option);


EOF
}


### Progress bar
progress(){

    [[ -z $1 || -z $2 || -z $3 ]] && exit  # on empty param...

    percent=$3
    completed=$(( $percent / 2 ))
    remaining=$(( 50 - $completed ))

    echo -ne "\r$1 ["
    printf "%0.s=" `seq $completed`
    echo -n ">"
    [[ $remaining != 0 ]] && printf "%0.s." `seq $remaining`
    echo -ne "] $percent% (of $2)  "\\r

}



echo ""
# 
# no arguments
if [ $# -eq 0 ] ; then
    how_to_use
    exit
fi
#
# get options
while getopts  "a:b:cde:f:hiI:j:k:lo:p:q:r:P:s2" opt ; do
# AM -> while getopts  "chsil2a:j:J:q:p:r:f:" opt ; do
  case $opt in
    a)
      ADD_PROJECT_FILES=( ${ADD_PROJECT_FILES[@]} "$OPTARG" )
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    b)
      BEAM_DIST_TEMPLATE=$OPTARG
      CREATE_BEAM_DIST=true
      echo " --> The beam distribution will be created from gpdist"
      ;;
    c)
      RUN_IN_CURRENT=true
      echo " --> numerical directories will be created here!"
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    d) 
      DUMMY_FLUKA_SERVER=true
      echo " --> use dummy server!"
      ;;
    e) 
      POST_FILE=$OPTARG
      echo " --> post-processing file: '${POST_FILE}'"
      ;;
    f) 
      FLUPRO_PATH=$OPTARG
      echo " --> new path in FLUPRO: '${FLUPRO_PATH}'"
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    h)
       how_to_use
       exit
       ;;
    i) 
      SIX_OR_ICO="i"
      echo " --> chosen tracker: IcoSim"
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    I)
      FLUKA_INP_NAME=$OPTARG
      JOB_NAME=${FLUKA_INP_NAME}
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    j)
      NJOBS=$OPTARG
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    k)
      NCYCLES=$OPTARG
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    l)
      RUN_ON_LOCAL_MACHINE=true
      echo " --> do not submit to PBS/LSF system"
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    o)
      BATCH_OPT=$OPTARG
      echo " --> optional arguments for PBS/LSF system"
      ;;
    p)
      COUPLING_PATH=$OPTARG
      echo " --> coupling path defined by user"
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    q) 
      QUEUE=$OPTARG
      echo " --> selected queue: '${QUEUE}'"
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    r)
      ASSIGN_OUTDIRNAME=true
      OUTDIRNAME=$OPTARG
      echo " --> numerical directories will be saved in '${OUTDIRNAME}'"
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    s) 
      SIX_OR_ICO="s"
      echo " --> chosen tracker: SixTrack"
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    2)
      SPLIT_JOBS=true
      echo " --> Tracker and Server will run on separate CPUs"
# AM ->       shift $(($OPTIND - 1))
# AM ->       OPTIND=1
      ;;
    P)
      PLATFORM=`echo $OPTARG | tr '[A-Z]' '[a-z]'`
      echo " --> platform explicitely set to '${PLATFORM}'"
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument." >&2
      exit 1
      ;;
  esac
done

#
# ----------------------------------------------------------------------------------------- #
# preliminary checks
# ----------------------------------------------------------------------------------------- #
#

if [ -z "${COUPLING_PATH}" ] ; then
    die "no valid coupling path"
else
    echo " --> coupling path: '${COUPLING_PATH}'"
fi

if ${DUMMY_FLUKA_SERVER} ; then
    SERVER_EXE=${COUPLING_PATH}/dummy/server
else
    # Fluka .inp file:
    if [ -z "${FLUKA_INP_NAME}" ] ; then
        die "FLUKA .inp file name not specified (without .inp)"
    else
        INFILE=${CURRENT_PATH}/${FLUKA_INP_NAME}.inp
        if [ ! -f $INFILE ]; then
    	    die "FLUKA input file '${INFILE}' does not exist"
        else
    	    echo " --> FLUKA .inp file: '${INFILE}'"
        fi
    fi

    # FLUPRO:
    if [ ! -d ${FLUPRO_PATH} ] ; then
      how_to_use
      die "no valid path for FLUPRO var"
    fi

    # FLUKA exe:
    SERVER_EXE=${COUPLING_PATH}/fluka/flukaserver
    if [ ! -f ${SERVER_EXE} ]; then
	die "Fluka exe not found, please run make in the fluka folder"
    fi
    FLUKAFLAGS="-e ${SERVER_EXE} -M 1"
fi
echo " --> server exe: '${SERVER_EXE}'"

if [ ${#POST_FILE} -gt 0 ] ; then
    PSFILE=$(readlink -f ${POST_FILE})
fi
if [ ! -f $PSFILE ]; then
    echo "Post-processing file $PSFILE not found."
    die "Please provide a valid file or remove the -e option when submitting"
fi


if [ ${#ADD_PROJECT_FILES[@]} -gt 0 ] ; then
    for PROJECT_FILE in ${ADD_PROJECT_FILES[@]} ; do
	echo " --> make a copy of additional user file: '${PROJECT_FILE}'"
    done
fi

if [ ${SIX_OR_ICO} == "s" ] ; then
    TRACKER_PATH=${COUPLING_PATH}/sixtrack
    TRACKER_EXE=${TRACKER_PATH}/SixTrack
    ADD_PROJECT_FILES=( ${ADD_PROJECT_FILES[@]} "fort.2" "fort.3" )
    if [ ! -f $TRACKER_EXE ]; then
	die "Tracker not found, please run make in the sixtrack folder"
    fi
    #
elif [ ${SIX_OR_ICO} == "i" ] ; then
    TRACKER_PATH=${COUPLING_PATH}/icosim
    TRACKER_EXE=${TRACKER_PATH}/build/tracker
    ADD_PROJECT_FILES=( ${ADD_PROJECT_FILES} "aperture.csv" "sps-collilist.dat" )
    if [ ! -f $TRACKER_EXE ]; then
	die "Tracker build not found, please run make in the icosim folder"
    fi
else
  die "Please select SixTrack or IcoSim"
fi
if [ ! -f ${TRACKER_EXE} ]; then
  die "Tracker exe not found, please make it!"
fi
echo " --> full path to tracker: '${TRACKER_EXE}'"

for POSSIBLE_PLATFORM in ${ACCEPTED_PLATFORMS[@]} ; do
    if [ "${PLATFORM}" == "${POSSIBLE_PLATFORM}" ] ; then
	PLATFORM_SELECTED=true
	break
    fi
done
if ! ${PLATFORM_SELECTED} ; then
    die "no valid queue selected: ${PLATFORM}"
fi
if [ ${PLATFORM} == "htcondor" ] ; then 
    POSSIBLE_QUEUEs=( "espresso" "microcentury" "longlunch" "workday" "tomorrow" "testmatch" "nextweek" )
    QUEUE_DEF="espresso"
fi

if [ "${QUEUE}" == "" ] ; then
    QUEUE="${QUEUE_DEF}"
fi
for POSSIBLE_QUEUE in ${POSSIBLE_QUEUEs[@]} ; do
    if [ ${QUEUE} == ${POSSIBLE_QUEUE} ] ; then
	QUEUE_SELECTED=true
	break
    fi
done
if ! ${QUEUE_SELECTED} ; then
    die "no valid queue selected: ${QUEUE} - possible values: ${POSSIBLE_QUEUEs[@]}"
fi

# AM -> PROJECT_NAME=$(echo $(basename $INFILE) | cut -d\- -f1 | cut -d\. -f1)

if $RUN_IN_CURRENT ; then
  if $ASSIGN_OUTDIRNAME ; then
    ASSIGN_OUTDIRNAME=false
  fi
  RUNDIR=${CURRENT_PATH}
else
  if $ASSIGN_OUTDIRNAME ; then
    RUNDIR=${CURRENT_PATH}/$OUTDIRNAME
    if [ ! -e $RUNDIR ] ; then
	mkdir $RUNDIR
    fi
  else
    RUNDIR=$(mktemp -d -p $CURRENT_PATH $JOB_NAME.XXX) || exit 1
  fi
  cd $RUNDIR
fi

# check existence of other jobs
runs=run*
runs=( ${runs} )
if [ ${#runs[@]} -gt 0 ] ; then 
    lastOne=`\ls -d1 run* | \tail -1 | \cut -d\_ -f2 | awk '{print ($1+0)}'`
    let RFIRST=$RFIRST+${lastOne}
fi
let RLAST=$RFIRST+$NJOBS-1

if [ $RFIRST -lt 1 ]; then
  echo "first dir < 1 !"
  exit 1
fi

if [ $RFIRST -gt $RLAST ]; then
  echo " --> inverted initial final seed:"
  echo " -->    from ${$RFIRST} ${$RLAST}"
  $RTEMP=$RFIRST
  $RFIRST=$RLAST
  $RLAST=$RTEMP
  echo " -->    to   ${$RFIRST} ${$RLAST}"
fi

if [ $NCYCLES -gt $NJOBS ]; then
  echo ""
  echo " --> Number of cycles > Total number of jobs: "
  echo "     from ${NCYCLES} ${NJOBS}"
  echo ""
  NCYCLES=$NJOBS
fi

# check existence of other job files (for cycling)
jobfs=job_*
jobfs=( ${jobfs} )
if [ ${#jobfs[@]} -gt 0 ] ; then 
    lastOne=`\ls -1 job_* | \tail -1 | \cut -d\_ -f2 | awk '{print ($1+0)}'`
    let JFIRST=$JFIRST+${lastOne}
fi

if [ ${SIX_OR_ICO} == "i" ] ; then
    for PROJECT_FILE in ${ICO_MAND_PROJECT_FILES} ; do
	if [ ! -e ${CURRENT_PATH}/${PROJECT_FILE} ] ; then
	    echo " missing file: ${CURRENT_PATH}/${PROJECT_FILE}"
	    if ! ${AT_LEAST_ONE_MISSING_FILE} ; then
		AT_LEAST_ONE_MISSING_FILE=true
	    fi
	fi
    done
fi
if [ ${SIX_OR_ICO} == "s" ] ; then
    if ! ${DUMMY_FLUKA_SERVER} ; then
	SIX_MAND_PROJECT_FILES=${SIX_FLK_MAND_PROJECT_FILES}
    fi
    for PROJECT_FILE in ${SIX_MAND_PROJECT_FILES} ; do
	if [ ! -e ${CURRENT_PATH}/${PROJECT_FILE} ] ; then
	    echo " missing file: ${CURRENT_PATH}/${PROJECT_FILE}"
	    if ! ${AT_LEAST_ONE_MISSING_FILE} ; then
		AT_LEAST_ONE_MISSING_FILE=true
	    fi
	fi
    done
fi
for PROJECT_FILE in ${ADD_PROJECT_FILES[@]} ; do
    if [ ! -e ${CURRENT_PATH}/${PROJECT_FILE} ] ; then
	echo " missing file: ${CURRENT_PATH}/${PROJECT_FILE}"
	if ! ${AT_LEAST_ONE_MISSING_FILE} ; then
	    AT_LEAST_ONE_MISSING_FILE=true
	fi
    fi
done
if ${AT_LEAST_ONE_MISSING_FILE} ; then
    die "at least one file is missing: I can't run"
fi

if [ ${SIX_OR_ICO} == "s" ] ; then
    # - number protons per job:
    N_couples_per_job=`\grep -v '/' ${CURRENT_PATH}/fort.3 | \grep -A1 'TRAC' | \tail -1 | awk '{print ($3)}'`
    N_protons_per_couple=`\grep -v '/' ${CURRENT_PATH}/fort.3 | \grep -A1 'INIT' | \tail -1 | awk '{print ($1)}'`
    N_protons_per_job=`echo ${N_couples_per_job} ${N_protons_per_couple} | awk '{print ($1*$2)}'`
    if [ ${N_protons_per_job} -lt 1 ] ; then
	die "Something wrong with the number of protons per job: ${N_protons_per_job}"
    fi

    # in case ixtrack is used, the population sampled by IcoSim should
    #    be imported, but since SixTrack has some limitations on the number
    #    of particles, the initial population should be segmented:
    # - check the existence of the file with the population generated by IcoSim:
    if ! ${CREATE_BEAM_DIST} ; then
	if [ ! -f ${CURRENT_PATH}/${INITIAL_BEAM_DIST} ] ; then
	    die "please provide me with the file of the population sampled with IcoSim"
	else
            # - total number of sampled protons:
	    tot_sampled=`\wc -l ${CURRENT_PATH}/${INITIAL_BEAM_DIST} | awk '{print ($1)}'`
	    # check that there are enough particles in the IcoSim file:
	    N_protons_needed=`echo ${RLAST} ${N_protons_per_job} | awk '{print ($1*$2)}'`
	    if [ ${N_protons_needed} -gt ${tot_sampled} ] ; then
		echo "needed: ${N_protons_needed}, sampled: ${tot_sampled}"
		die "not enough particles in the ${INITIAL_BEAM_DIST} file..."
	    fi
	fi
    fi
fi

# Set submission executable for each cluster type
if [ ${PLATFORM} == "lsf" ] ; then 
    BATCH_EXE="bsub -q ${QUEUE} ${BATCH_OPT}"
elif [ ${PLATFORM} == "pbs" ] ; then 
    BATCH_EXE="qsub ${BATCH_OPT}"
fi

# LSF emails:
if [ ${PLATFORM} == "lsf" ] ; then
    # avoid LSF mails
    LSFerrFile="mail_job_%05i.err"
    LSFoutFile="mail_job_%05i.out"
    echo " --> you won't receive emails from the LSF system;"
    echo "     thus, the same info is contained in ${LSFerrFile} and ${LSFoutFile} files,"
    echo "     locally saved in the numerical directory;"
fi

#
# ----------------------------------------------------------------------------------------- #
# let's go!
# ----------------------------------------------------------------------------------------- #
#

echo
echo " ** Running simulation under directory:"
echo "    $RUNDIR"
echo
echo "    Seeds: $RFIRST - $RLAST"
echo "    Cycles per run: $NCYCLES"
echo "    Queue type: '${QUEUE}'"
echo

# the following if statement will be useless once a final input file to 
#     icosim will be established
if [ ${SIX_OR_ICO} == "i" ] ; then
    mkdir $RUNDIR/tracker
    cp -R -L ${TRACKER_PATH}/build/* $RUNDIR/tracker
fi

# echo command in .submitCommand.txt
echo "[`date`] $0 $*" >> ${CURRENT_PATH}/.submitCommand.txt

# in case the user requires no LSF emails, collect everyting in one place
if [ "${PLATFORM}" == "lsf" ] || [ "${PLATFORM}" == "htcondor" ] ; then
    if [ ! -d ${RUNDIR}/emails ] ; then
	mkdir ${RUNDIR}/emails
    fi
fi

# in case of running on lxplus create a local directory for storing clean input files
#   check that this directory does not exist; otherwise, it means that
#   the user is re-starting a simulation already run or adding statistics;
# so, notify the user, but do not copy new files;
if ( hostname | grep lxplus > /dev/null ); then 
    if [ -d ${RUNDIR}/clean_input ] ; then
	echo " local directory clean_input for storing clean input files already exists."
	echo " no new file will be copied there..."
    else
	mkdir ${RUNDIR}/clean_input
        # Copy files needed for the simulation
	if [ ${SIX_OR_ICO} == "i" ] ; then
            for file in $ICO_MAND_PROJECT_FILES; do
    		cp ${PROJECT_PATH}/$file ${RUNDIR}/clean_input
            done
	elif [ ${SIX_OR_ICO} == "s" ] ; then
            for file in $SIX_MAND_PROJECT_FILES; do
    		cp ${PROJECT_PATH}/$file ${RUNDIR}/clean_input
            done
	fi
	for file in ${ADD_PROJECT_FILES[@]}; do
            cp ${PROJECT_PATH}/$file ${RUNDIR}/clean_input
	done
	if ! ${DUMMY_FLUKA_SERVER} ; then
            cp $INFILE ${RUNDIR}/clean_input
	fi
	if ${CREATE_BEAM_DIST} ; then
	    cp ${CURRENT_PATH}/${BEAM_DIST_TEMPLATE} ${RUNDIR}/clean_input
	else
	    cp ${CURRENT_PATH}/${INITIAL_BEAM_DIST} ${RUNDIR}/clean_input
	fi
    fi
fi

# store the information concerning the revision of the svn repositories used:
echo "########################################################################" >> ${RUNDIR}/${svnLogFile}
echo "########################################################################" >> ${RUNDIR}/${svnLogFile}
echo "########################################################################" >> ${RUNDIR}/${svnLogFile}
echo "####  logs of the svn repositories" >> ${RUNDIR}/${svnLogFile}
echo "####  on `date` " >> ${RUNDIR}/${svnLogFile}
echo "########################################################################" >> ${RUNDIR}/${svnLogFile}
echo "########################################################################" >> ${RUNDIR}/${svnLogFile}
echo "########################################################################" >> ${RUNDIR}/${svnLogFile}
for svnrepo in ${COUPLING_PATH}/fluka ${COUPLING_PATH}/dummy ${TRACKER_PATH} ${COUPLING_PATH}/deps/flukaio ; do
    cd ${svnrepo}
    echo "" >> ${RUNDIR}/${svnLogFile}
    echo "####################################################################" >> ${RUNDIR}/${svnLogFile}
    echo "####################################################################" >> ${RUNDIR}/${svnLogFile}
    echo "####################################################################" >> ${RUNDIR}/${svnLogFile}
    echo "####  " >> ${RUNDIR}/${svnLogFile}
    echo "####  ${svnrepo}" >> ${RUNDIR}/${svnLogFile}
    echo "####  " >> ${RUNDIR}/${svnLogFile}
    echo "####################################################################" >> ${RUNDIR}/${svnLogFile}
    echo "####################################################################" >> ${RUNDIR}/${svnLogFile}
    echo "####################################################################" >> ${RUNDIR}/${svnLogFile}
    echo "" >> ${RUNDIR}/${svnLogFile}
    svn info >> ${RUNDIR}/${svnLogFile}
    svn diff >> ${RUNDIR}/${svnLogFile}
    echo "" >> ${RUNDIR}/${svnLogFile}
    cd - > /dev/null
done

for ((  icount = 0 ;  icount < $NJOBS;  icount++  ))
do

  # an additional string in the submission command which depends on submitted job
  additionalString=""

  # seed
  let iseed=$icount+$RFIRST

  # numerical directory:
  NUMDIR=`printf ${NUMDIR_TEMPLATE} $iseed`
  NAMENUMDIR=`printf ${DIRNAME_TEMPLATE} $iseed`
#  echo "creating ${NAMENUMDIR} ..."
  mkdir ${NAMENUMDIR} || exit 1
  cd ${NAMENUMDIR}

  # in case of running not on lxplus, copy in run_????? dir all the files needed
  # otherwise, the same operations are performed at the level of the job file
  if ! ( hostname | grep lxplus > /dev/null ); then 

      # in case IcoSim is used, copy the tracker script and fix it
      if [ ${SIX_OR_ICO} == "i" ] ; then
          sed -e s/tracker/tracker\\/tracker/ ${TRACKER_PATH}/build/run_tracker.sh > run_tracker.sh
          chmod +x run_tracker.sh
      fi      
    
      # Copy files needed for the simulation
      if [ ${SIX_OR_ICO} == "i" ] ; then
          for file in $ICO_MAND_PROJECT_FILES; do
    	      cp ${PROJECT_PATH}/$file .
              #echo "cp ${PROJECT_PATH}/$file ."
          done
      elif [ ${SIX_OR_ICO} == "s" ] ; then
          for file in $SIX_MAND_PROJECT_FILES; do
    	      cp ${PROJECT_PATH}/$file .
              #echo "cp ${PROJECT_PATH}/$file ."
          done
      fi
      for file in ${ADD_PROJECT_FILES[@]}; do
        cp ${PROJECT_PATH}/$file .
# -->         echo "cp ${PROJECT_PATH}/$file ."
      done
    
      if ! ${DUMMY_FLUKA_SERVER} ; then
          # Copy input file and replace the Random seed by the "cycle" number ($iseed)
          cp $INFILE .
          sed -i "s/^RANDOMIZ.*/RANDOMIZ         1.0`printf "%10.1f" "$iseed"`/g" $(basename $INFILE)
      fi

  fi

  #
  # --------------------------------------------------------------------------------------- #
  # jobfile
  # --------------------------------------------------------------------------------------- #
  #



  NAME=$(basename $RUNDIR).$NUMDIR
  if ${SPLIT_JOBS} ; then
      JOBFILE=${JOB_NAME}.s.job
      PBSNAME=${NAME}.s
  else
      JOBFILE=${JOB_NAME}.job
      PBSNAME=${NAME}
  fi
  
  # in case of lxplus, store all the pre-processing in a dedicated job file, so
  #    that jobs can be easily restarted without going through these stages
  #    (eg update of random seeds, generation of input distribution...)
  if ( hostname | grep lxplus > /dev/null ); then 
      OUTFILE="$PREPROC"
      echo "#!/bin/bash" > $PREPROC
  else
      OUTFILE="$JOBFILE"
  fi

  cat > $JOBFILE << EOF
#!/bin/bash
#PBS -q ${QUEUE}
#PBS -N ${PBSNAME}

EOF
  if ! ${DUMMY_FLUKA_SERVER} ; then
      echo "export FLUKA=${FLUPRO_PATH}" >> $JOBFILE
      echo "export FLUPRO=${FLUPRO_PATH}" >> $JOBFILE
  fi

  cat >> $JOBFILE << EOF

RUNPATH=\$PWD
HOST=\$(hostname)
USERNAME=\$(whoami)
echo "Runpath ${RUNPATH}" >> \$RUNDIR/\${NAMENUMDIR}/progress.dat

echo " ** Host: \$HOST" > \$RUNPATH/server_${NUMDIR}.log
echo " ** Original directory: \$RUNPATH/${NAMENUMDIR}" >> \$RUNPATH/server_${NUMDIR}.log

echo " \$HOST" > ${netFile}
EOF

  if ! ${DUMMY_FLUKA_SERVER} ; then
      cat >> $JOBFILE <<EOF

# Check if the executable is newer than the library
newer_file=0
if [ -e ${COUPLING_PATH}/fluka/flukaserver ] ; then
    if [ ${COUPLING_PATH}/fluka/flukaserver -ot ${FLUPRO_PATH}/flukahp ] ; then
        let newer_file=\${newer_file}+1
    fi
else
    let newer_file=\${newer_file}+1
fi
if [ \${newer_file} -gt 0 ]; then
    echo " ** flukahp newer than the executable flukaserver..." >> \$RUNPATH/server_${NUMDIR}.log
    ls -lhr  /home/soft/flukadev/flukahp >> \$RUNPATH/server_${NUMDIR}.log
    echo " ** Recompiling FLUKA executable..." >> \$RUNPATH/server_${NUMDIR}.log
    cd ${COUPLING_PATH}/fluka/
    make all
    cd -
fi
EOF
  fi

  # in case of running on lxplus, copy in run_????? dir all the files needed
  if ( hostname | grep lxplus > /dev/null ); then 

      # in case IcoSim is used, copy the tracker script and fix it
      if [ ${SIX_OR_ICO} == "i" ] ; then
	  cat >> $OUTFILE <<EOF

# copy the tracker script and fix it
sed -e s/tracker/tracker\\\\/tracker/ \${TRACKER_PATH}/build/run_tracker.sh > run_tracker.sh
chmod +x run_tracker.sh
EOF
      fi      
    
      # Copy files needed for the simulation
      cat >> $OUTFILE <<EOF

# Copy files needed by the simulation
cp ${RUNDIR}/clean_input/* .
EOF
    
      if ! ${DUMMY_FLUKA_SERVER} ; then
	  cat >> $OUTFILE <<EOF
# replace the Random seed by the "cycle" number ($iseed)
sed -i "s/^RANDOMIZ.*/RANDOMIZ         1.0`printf "%10.1f" "$iseed"`/g" $(basename $INFILE)

EOF
      fi

  fi

  # in case of sixtrack, copy a segment of the starting distribution or generate a new one:
  if [ ${SIX_OR_ICO} == "s" ] ; then
      if ( hostname | grep lxplus > /dev/null ); then 
	  INP_PATH=${RUNDIR}/clean_input
      else
	  INP_PATH=${CURRENT_PATH}
      fi
      if ${CREATE_BEAM_DIST} ; then
	  # We create the beam distribution runtime
	  cat >> $OUTFILE <<EOF

# We create the beam distribution runtime
sed -i "s/^OUTPUT.*/OUTPUT  ${N_protons_per_job}  ${iseed}  ${SIX_SAMPLED_FILE}/g" $(basename ${BEAM_DIST_TEMPLATE})

# beam distribution created runtime
if [[ ! -f ${COUPLING_PATH}/tools/partdist/gpdist.exe ]] ; then
    cd ${COUPLING_PATH}/tools/partdist
    make clean
    make all
    cd - >> /dev/null
fi
${COUPLING_PATH}/tools/partdist/gpdist.exe ${BEAM_DIST_TEMPLATE}
EOF
      else
	  # We slice the full distribution
	  cat >> $OUTFILE <<EOF

# beam distribution from existing file
N_head=`echo $iseed ${N_protons_per_job} | awk '{print ($1*$2)}'`
head -n\${N_head} ${INP_PATH}/${INITIAL_BEAM_DIST} | tail -n${N_protons_per_job} > ${SIX_SAMPLED_FILE}

EOF
      fi
  fi

  # common part:
  #     in case two different jobs are sent (one for the server and one for the tracker):
  if ${SPLIT_JOBS} ; then
      #
      # - close the job file for the server with the call to the server:
      echo "# Run Server" >> $JOBFILE
      echo "echo \" - Running server (Fluka)...\" >> \$RUNPATH/server_${NUMDIR}.log " >> $JOBFILE
      if ${DUMMY_FLUKA_SERVER} ; then
	  serverCommandLine="${SERVER_EXE}"
      else
	  serverCommandLine="\$FLUPRO/flutil/rfluka ${FLUKA_INP_NAME} ${FLUKAFLAGS}"
      fi
      echo "${serverCommandLine} >> \$RUNPATH/server_${NUMDIR}.log 2>&1 " >> $JOBFILE
      echo "echo \" - Server done\" >> \$RUNPATH/server_${NUMDIR}.log " >> $JOBFILE
      # 
      chmod +x $JOBFILE

      # - exec file 
      ICYCLE=`echo $icount $NCYCLES ${JFIRST} | awk '{printf("%i",$1/$2+$3)}'`
      EXECFILE=`printf ${EXENAME_TEMPLATE} $ICYCLE`
      cd $RUNDIR
      if [[ ! -f $EXECFILE ]]; then
          # common part:
          echo "#!/bin/bash" >>  $EXECFILE 
          echo "#PBS -q ${QUEUE}" >>  $EXECFILE 
          echo "#PBS -N ${tmpPBSName}" >>  $EXECFILE 
	  if ! ${RUN_ON_LOCAL_MACHINE} ; then
	      echo "CURDIR=\$PWD" >>  $EXECFILE 
	  fi
	  iPBSNameStart=$iseed
      fi
      if ( hostname | grep lxplus > /dev/null ); then 
	  if ! ${RUN_ON_LOCAL_MACHINE} ; then
	      echo "mkdir \$CURDIR/${NAMENUMDIR}" >> $EXECFILE
	      echo "cd \$CURDIR/${NAMENUMDIR}" >> $EXECFILE
	      echo "cp -rf $RUNDIR/${NAMENUMDIR}/* ." >> $EXECFILE
	  else 
	      echo "cd $RUNDIR/${NAMENUMDIR}" >> $EXECFILE
	  fi
      else
	  echo "cd $RUNDIR/${NAMENUMDIR}" >> $EXECFILE
      fi
      echo "./$JOBFILE" >> $EXECFILE

      if [[ `echo $icount $NCYCLES | awk '{printf(($1+1)%$2)}'` -eq 0 ]] || [ $iseed == $RLAST ]; then
	  iPBSNameStop=$iseed
	  if [ ${iPBSNameStart} -ne ${iPBSNameStop} ] ; then
	      PBSNAME=`printf "${PBSNAME_TEMPLATE}" "${iPBSNameStart}" "${iPBSNameStop}"`
	  fi
	  sed -i "s/${tmpPBSName}/${PBSNAME}/g" $EXECFILE
          chmod +x $EXECFILE
      fi
      cd ${NAMENUMDIR}
      #
      # - open the job file for the tracker and start to fill it:
      JOBFILE=${JOB_NAME}.t.job
      PBSNAME=${NAME}.t
      cat > $JOBFILE << EOF
#!/bin/bash
#PBS -q ${QUEUE}
#PBS -N ${PBSNAME}

HOST=\$(hostname)
USERNAME=\$(whoami)
RUNPATH=\$PWD
EOF
  else
      # continue editing the jobfile with the call to the server
      echo "# Run the server in background" >> $JOBFILE
      echo "echo \" - Running server in background...\" >> \$RUNPATH/server_${NUMDIR}.log" >> $JOBFILE
      echo "echo \" - Running server in background...\" >> \$RUNDIR/${NAMENUMDIR}/progress.dat" >> ${JOBFILE}
      
      if ${DUMMY_FLUKA_SERVER} ; then
	  serverCommandLine="${SERVER_EXE}"
      else
	  serverCommandLine="\$FLUPRO/flutil/rfluka ${FLUKA_INP_NAME} ${FLUKAFLAGS}"
      fi
      echo "${serverCommandLine} >> \$RUNPATH/server_${NUMDIR}.log 2>&1 &" >> $JOBFILE
  fi



  # in case IcoSim is used, run in temp directory
  if [ ${SIX_OR_ICO} == "i" ] ; then
      echo "TMPDIR=\$(mktemp -d /tmp/tracker.XXXXXX) || exit 1" >> $JOBFILE
  fi



  # common part:
  #     keep track of the host:
  echo "" >> $JOBFILE
  echo "echo \" ** Host: \$HOST\" > \$RUNPATH/tracker_${NUMDIR}.log" >> $JOBFILE



  # in case IcoSim is used, echo informations about the temp directory
  if [ ${SIX_OR_ICO} == "i" ] ; then
      cat >> $JOBFILE << EOF
echo " ** Original directory: \$RUNPATH/${NAMENUMDIR}" >> \$RUNPATH/tracker_${NUMDIR}.log
echo " ** Running in: \$TMPDIR" >> \$RUNPATH/tracker_${NUMDIR}.log

echo " - Moving tracker to temporary directory..." >> \$RUNPATH/tracker_${NUMDIR}.log
cp -R -L \$RUNPATH/tracker/* \$TMPDIR/
ln -s \$TMPDIR tracker
EOF
  fi



  # common part:
  cat >> $JOBFILE << EOF

# Wait until server is initialized
while [ ! -f ${netFile} ]; do
  echo "server still not running, waiting..." >> \$RUNPATH/tracker_${NUMDIR}.log
  sleep 10
done

# wait until ${netFile} declares both host and port
while [ \`\wc -l ${netFile} | \awk '{print (\$1)}'\` -ne 2 ] ; do
  echo "host/port not fully declared, waiting..." >> \$RUNPATH/tracker_${NUMDIR}.log
  sleep 10
  # Check FLUKA is still running on the background. If not, close the job.
  if [ \` \ps -u \$USERNAME | \grep rfluka | \wc -l \` -eq 0 ]; then
      echo "Something went wrong: FLUKA exited before opening the port!"  >> \$RUNPATH/tracker_${NUMDIR}.log
      exit
  fi
done

EOF

  # common part:
  #     in case two different jobs are sent (one for the server and one for the tracker),
  #        change header to the tracker comments:
  if ${SPLIT_JOBS} ; then
     echo "# Run tracker" >> $JOBFILE
  else
     echo "# Run tracker in foreground" >> $JOBFILE
  fi
  echo "echo \" - Running the Tracker...\" >> \$RUNPATH/tracker_${NUMDIR}.log" >> $JOBFILE



  if [ ${SIX_OR_ICO} == "i" ] ; then
      echo "./run_tracker.sh ${MATLAB_PATH} >> \$RUNPATH/tracker_${NUMDIR}.log 2>&1" >> $JOBFILE
  elif [ ${SIX_OR_ICO} == "s" ] ; then
      echo "$TRACKER_EXE >> \$RUNPATH/tracker_${NUMDIR}.log 2>&1" >> $JOBFILE
  fi



  # common part of the jobfile:
  cat >> $JOBFILE << EOF

sleep 20
echo " - Tracker done" >> \$RUNPATH/tracker_${NUMDIR}.log

EOF

  # common part:
  #     in case two different jobs are sent (one for the server and one for the tracker),
  #     Wait until the server ends only if one global job:
  if ! ${SPLIT_JOBS} ; then
      cat >> $JOBFILE << EOF
# Wait until server ends
wait
echo " - Server done" >> \$RUNPATH/server_${NUMDIR}.log

EOF
  fi


  # in case IcoSim is used rm the temp dir:
  if [ ${SIX_OR_ICO} == "i" ] ; then
      echo "rm tracker" >> $JOBFILE
      echo "#cp -R \$TMPDIR tracker" >> $JOBFILE
      echo "rm -rf \$TMPDIR" >> $JOBFILE
  # in case sixtrack is used, remove the empty units:
  elif [ ${SIX_OR_ICO} == "s" ] ; then
      echo '' >> $JOBFILE
      echo '# remove empty units:' >> $JOBFILE
      echo 'file_list=`\ls .`' >> $JOBFILE
      echo 'if [ -e empties.lis ] ; then' >> $JOBFILE
      echo '   \rm empties.lis' >> $JOBFILE
      echo 'fi' >> $JOBFILE
      echo 'for file in ${file_list} ; do' >> $JOBFILE
      echo '    if [ ! -s ${file} ] ; then' >> $JOBFILE
      echo '       \ls -ltrh ${file} >> empties.lis' >> $JOBFILE
      echo '       \rm ${file}' >> $JOBFILE
      echo '    fi' >> $JOBFILE
      echo 'done' >> $JOBFILE
      echo '' >> $JOBFILE
  fi

  # Post-processing file - if required:
  if [[ -f $PSFILE ]]; then
      eval echo "\"$(cat $PSFILE)\"" >> $JOBFILE
  fi


  chmod +x $JOBFILE

  # in case of lxplus, store all the pre-processing in a dedicated job file, so
  #    that jobs can be easily restarted without going through these stages
  #    (eg update of random seeds, generation of input distribution...)
  if ( hostname | grep lxplus > /dev/null ); then 
      chmod +x $PREPROC
  fi

  # Exec file 
  ICYCLE=`echo $icount $NCYCLES ${JFIRST} | awk '{printf("%i",$1/$2+$3)}'`
  EXECFILE=`printf ${EXENAME_TEMPLATE} $ICYCLE`
  cd $RUNDIR
  if [[ ! -f $EXECFILE ]]; then
      # common part:
      echo "#!/bin/bash" >>  $EXECFILE 
      echo "#PBS -q ${QUEUE}" >>  $EXECFILE 
      echo "#PBS -N ${tmpPBSName}" >>  $EXECFILE 
      if ! ${RUN_ON_LOCAL_MACHINE} ; then
	  echo "CURDIR=\$PWD" >>  $EXECFILE 
      fi
      iPBSNameStart=$iseed
  fi
  echo "" >> $EXECFILE
  if ( hostname | grep lxplus > /dev/null ); then 
      if ! ${RUN_ON_LOCAL_MACHINE} ; then
	  echo "mkdir \$CURDIR/${NAMENUMDIR}" >> $EXECFILE
	  echo "cd \$CURDIR/${NAMENUMDIR}" >> $EXECFILE
	  echo "cp -rf $RUNDIR/${NAMENUMDIR}/* ." >> $EXECFILE
      else 
	  echo "cd $RUNDIR/${NAMENUMDIR}" >> $EXECFILE
      fi
      # in case of lxplus, store all the pre-processing in a dedicated job file, so
      #    that jobs can be easily restarted without going through these stages
      #    (eg update of random seeds, generation of input distribution...)
      echo "./$PREPROC" >> $EXECFILE
  else
      echo "cd $RUNDIR/${NAMENUMDIR}" >> $EXECFILE
  fi
  echo "./$JOBFILE " >> $EXECFILE
  if [ "${PLATFORM}" == "htcondor" ] ; then
      echo "cd -" >> $EXECFILE
      echo "date " >> $EXECFILE
  fi

  if [[ `echo $icount $NCYCLES | awk '{printf(($1+1)%$2)}'` -eq 0 ]] || [ $iseed == $RLAST ]; then
      iPBSNameStop=$iseed
      if [ ${iPBSNameStart} -ne ${iPBSNameStop} ] ; then
	  PBSNAME=`printf "${PBSNAME_TEMPLATE}" "${iPBSNameStart}" "${iPBSNameStop}"`
      fi
      sed -i "s/${tmpPBSName}/${PBSNAME}/g" $EXECFILE
      chmod +x $EXECFILE
      perc=$(echo "scale=0;100*${iseed}/${RLAST}" | bc -l)
#      echo -ne "-->  Submitting $EXECFILE (run_${iseed}) to server [${perc} %]"\\r
      if ${RUN_ON_LOCAL_MACHINE} ; then
	  ./$EXECFILE &
      else
	  if [ "${PLATFORM}" == "lsf" ] ; then
	      additionalString="-o `printf "${RUNDIR}/emails/${LSFoutFile}" "$ICYCLE"` -e `printf "${RUNDIR}/emails/${LSFerrFile}" "$ICYCLE"`"
	  fi
	  if [ "${PLATFORM}" != "htcondor" ] ; then
	      echo " Submitting $EXECFILE to server..."
	      ${BATCH_EXE} ${additionalString} ${EXECFILE}
	  else
#	      echo -ne "-->  Created $EXECFILE (run ${iseed}) [${perc} %]"\\r
	      progress 0 ${RLAST} ${perc}
	      echo ${EXECFILE} >> jobs.txt
	  fi
      fi
      #rm $EXECFILE
  fi
done


echo 
if [ "${PLATFORM}" == "htcondor" ] ; then
    echo " Submitting jobs to htcondor..."
    cp ${COUPLING_PATH}/tools/running/htcondor.sub .
    sed -i "s/^+JobFlavour.*/+JobFlavour = \"${QUEUE}\"/" htcondor.sub
    condor_submit htcondor.sub
    if [ $? -eq 0 ] ; then
	rm jobs.txt
    fi
fi
