
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
 
  #[ ! -z ${zip_archive} ] && return ${ERR_DATA_STAGEIN}

  #cd ${target}
  #unzip -qq $( basename ${zip_archive} ) || ${ERR_DATA_STAGEIN}

  #cd - &> /dev/null

  #rm -f ${zip_archive}
}

function scan_input() {

  local input_path=$1

  cd ${input_path}

  for zip_archive in $( find ${input_path} -name "*.zip" )
  do 
    unzip -qq $( basename ${zip_archive} )
    [ "$( basename ${zip_archive} | cut -c 1-4 )" == "asc_" ] && {
      prefix="Asc"
      short_prefix="A"
      echo "pathAsc   = '$( echo ${zip_archive} | sed "s/\.zip//" )'" 
    } || {
      prefix="Desc"
      short_prefix="D"
      echo "pathDesc  = '$( echo ${zip_archive} | sed "s/\.zip//" )'"
    }

    mask="$( basename $( zipinfo -1 ${zip_archive} | grep mask | head -n 1 ))"
    vel="$( basename $( zipinfo -1 ${zip_archive} | grep vel | head -n 1 ))"
    x_coh="$( echo ${mask} | sed 's/mask_GEO_//' | sed 's/\.dat//' | tr "x" "\n" | head -n 1 )"
    y_coh="$( echo ${mask} | sed 's/mask_GEO_//' | sed 's/\.dat//' | tr "x" "\n" | tail -n 1 )"

cat << EOF > ${short_prefix}
mask_coh${short_prefix} = '${mask}'
x_coh${short_prefix}    = ${x_coh}l
y_coh${short_prefix}    = ${y_coh}l
mask_vel${short_prefix} = '${vel}'
EOF
    rm -f ${zip_archive}
  done

  cat A 
  cat D
  rm -f A D
}

function create_go() {

  local go=$1

  echo "pathgen='.'" > ${go}
echo "pathgen=\'${TMPDIR}/input\'" > ${go}
  scan_input ${TMPDIR}/input >> ${go}
  
  echo "combine,pathgen,d_zz,d_ew,COH_COM,pathAsc=pathAsc,pathDesc=pathDesc,mask_cohA=mask_cohA,x_cohA=x_cohA,y_cohA=y_cohA,mask_velA=mask_velA,mask_cohD=mask_cohD,x_cohD=x_cohD,y_cohD=y_cohD,mask_velD=mask_velD" >> ${go}
  ciop-publish -m ${go}
}

function main() {
  
  set_idl_env || return ${ERR_IDL_ENV}

  cd ${TMP_DIR}

  # copy .sav 
  cp ${_CIOP_APPLICATION_PATH}/node/idl/combine_v2.sav ${TMPDIR}
   
  # create .go file 
  #go_file=${TMPDIR}/combine_v2.go
  #create_go ${go_file}
  go_file=${TMPDIR}/combine_v2.sav
  
  # invoke IDL
  idl -rt=${go_file} 2> ${TMPDIR}/combine.log 

  # publish log
  ciop-publish -m ${TMPDIR}/combine.log

  # check IDL execution 
  grep halted ${TMPDIR}/combine.log && return ${ERR_IDL}

  # publish result
  #ciop-publish -m ${TMPDIR}/ ??


}
