#! /bin/ksh -x

# Script to be executed by ECFlow

. /etc/profile.d/modules.sh
module use $LUSTREFS/opt/Modules/modulefiles
module load fpp-enkf_conus

. $FPP_ENKFCONUS_ROOT/resources/enkf_conus.config

$FPP_ENKFCONUS_ROOT/bin/fpp-ctl
