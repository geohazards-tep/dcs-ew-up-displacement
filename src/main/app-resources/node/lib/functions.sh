
# define success code
SUCCESS=0

# define error codes
ERR_IDL_ENV=10
ERR_DATA_STAGEIN=20
ERR_IDL=30

# define how to exit gracefully
function cleanExit ()
{
  local retval=$?
  local msg=""
  case "${retval}" in
    ${SUCCESS}) msg="Processing successfully concluded";;
    ${ERR_IDL_ENV}) msg="Failed to create the IDL environment";;
    ${ERR_DATA_STAGEIN}) msg="Failed to stage-in data";;
    ${ERR_IDL}) msg="IDL execution error";;
    *) msg="Unknown error";;
  esac

  [ "${retval}" != "0" ] && ciop-log "ERROR" "Error ${retval} - ${msg}, processing aborted" || ciop-log "INFO" "${msg}"
  exit ${retval}
}

# add the trap
trap cleanExit EXIT

# add any other functions

function set_idl_env() {

  export LM_LICENSE_FILE=1700@idl.terradue.int

}

function get_data() {

  local input=$1
  local target=$2

  zip_archive="$( ciop-copy -f -U -O ${target} ${input} )"
 
  [ -z "${zip_archive}" ] && return ${ERR_DATA_STAGEIN} || return 0

}

function scan_input() {

  local input_path=$1

  cd ${input_path}

  for zip_archive in $( find ${input_path} -name "*.zip" )
  do 
    unzip -qq $( basename ${zip_archive} )
    [ "$( basename ${zip_archive} | cut -c 1-4 )" == "asc_" ] && {
      short_prefix="A"
    } || {
      short_prefix="D"
    }
    echo ${zip_archive} | sed "s/\.zip//" > ${short_prefix}
    mask="$( basename $( zipinfo -1 ${zip_archive} | grep mask | head -n 1 ))"
    vel="$( basename $( zipinfo -1 ${zip_archive} | grep vel | head -n 1 ))" 
    x_coh="$( echo ${mask} | sed 's/mask_GEO_//' | sed 's/\.dat//' | tr "x" "\n" | head -n 1 )" 
    y_coh="$( echo ${mask} | sed 's/mask_GEO_//' | sed 's/\.dat//' | tr "x" "\n" | tail -n 1 )" 
 
    echo ${mask} >> ${short_prefix}
    echo ${x_coh}l >> ${short_prefix}
    echo ${y_coh}l >> ${short_prefix}
    echo ${vel} >> ${short_prefix}

  done

  cat A 
  cat D
  rm -f A D
}

function create_go() {

  local go=$1
  local par1=$2
  local par2=$3 

  echo "." > ${go}
  scan_input ${TMPDIR}/input >> ${go}
  echo ${par1} >> ${go}
  echo ${par2} >> ${go}

  ciop-publish -m ${go}
}

function main() {
  
  set_idl_env || return ${ERR_IDL_ENV}

  par1="$( ciop-getparam par1 )"
  par2="$( ciop-getparam par2 )"

  cd ${TMP_DIR}

  # copy .sav 
  cp ${_CIOP_APPLICATION_PATH}/node/idl/combine_v2.sav ${TMPDIR}
   
  # create .go file 
  go_file=${TMPDIR}/inputparams.par
  create_go ${go_file} ${par1} ${par2}
  idl_file=${TMPDIR}/combine_v2.sav
  
  # invoke IDL
  idl -rt=${idl_file} -IDL_DEVICE Z < ${go_file} &> ${TMPDIR}/combine.log 
  
  # publish log
  ciop-publish -m ${TMPDIR}/combine.log

  # check IDL execution 
  grep halted ${TMPDIR}/combine.log && return ${ERR_IDL}

  # publish result
  ciop-publish -m ${TMPDIR}/result 


}
