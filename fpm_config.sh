#!/bin/bash

# Fortran compiler
FC=ifx
# common compiler flags
FFLAGS_COMMON="-standard-semantics -fpp -qmkl -qopenmp"
# release compiler flags
FFLAGS_RELEASE="-O3"
# debug compiler flags
FFLAGS_DEBUG="-O0 -g -debug all -warn-all -check-all -traceback -fpmodel=precise -qno-openmp-simd"
#FFLAGS_DEBUG="-g -O0 -debug all -debug parallel -warn unused -traceback -fpe3 -fp-model strt -ftrapuv\
#  -check-pointers -check bounds -check noarg_temp_created"

POSITIONAL_ARGS=()
PROFILE=release

while [[ $# -gt 0 ]]; do
  case $1 in
    -p|--profile)
      PROFILE="$2"
      shift # past argument
      shift # past value
      ;;
    --default)
      DEFAULT=YES
      shift # past argument
      ;;
    -*|--*)
      echo "Unknown option $1"
      return
      ;;
    *)
      POSITIONAL_ARGS+=("$1") # save positional arg
      shift # past argument
      ;;
  esac
done

set -- "${POSITIONAL_ARGS[@]}" # restore positional parameters

case $PROFILE in
  release)
    FFLAGS="${FFLAGS_RELEASE} ${FFLAGS_COMMON}"
    ;;
  debug)
    FFLAGS="${FFLAGS_DEBUG} ${FFLAGS_COMMON}"
    ;;
  *)
    echo "Unknown profile $PROFILE"
    return
    ;;
esac

export FPM_FC=$FC
export FPM_FFLAGS=$FFLAGS
