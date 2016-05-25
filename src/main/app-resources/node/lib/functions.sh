
# define success code
SUCCESS=0

# define error codes
ERR_IDL_ENV=10
ERR_DATA_STAGEIN=20


# define how to exit gracefully
function cleanExit ()
{
  local retval=$?
  local msg=""
  case "${retval}" in
    ${SUCCESS}) msg="Processing successfully concluded";;
    ${ERR_IDL_ENV}) msg="Failed to create the IDL environment";;
    ${ERR_DATA_STAGE_IN}) msg="Failed to stage-in data";;
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

  ciop-copy -O ${target} ${input} || return $ERR_DATA_STAGEIN

}

function main() {
  
  set_idl_env || return ${ERR_IDL_ENV}

  # invoke IDL
  idl -rt=/${_CIOP_APPLICATION_PATH}/node/bin/combine_v2.sav


  # publish result
  # ciop-publish -m <path_to_file>
}
