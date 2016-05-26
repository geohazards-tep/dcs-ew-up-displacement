
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
    ${ERR_DATA_STAGE_IN}) msg="Failed to stage-in data";;
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
 
  [ -z ${zip_archive} ] && return ${ERR_DATA_STAGEIN}

  cd ${target}
  unzip -qq $( basename ${zip_archive} ) || ${ERR_DATA_STAGEIN}

  cd - &> /dev/null

  rm -f ${zip_archive}
}

function main() {
  
  set_idl_env || return ${ERR_IDL_ENV}

  cd ${TMP_DIR}
  # invoke IDL
  idl -rt=${_CIOP_APPLICATION_PATH}/node/idl/combine_v2.sav 2> ${TMPDIR}/combine.log 
 
  grep halted ${TMPDIR}/combine.log && return ${ERR_IDL}

  # publish result
  # ciop-publish -m <path_to_file>
}
