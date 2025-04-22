#!/bin/sh

#set -x

#------------------------------------------------------------------------------
#  This script generates BUFR messages corresponding to a given user DX (BUFR
#  dictionary) table.  It then appends these messages to every applicable (see
#  % below) tankfile in the given BUFR database whose type corresponds to the
#  given table, unless the tankfile subtype is explicitly excluded via the
#  environment variable $APXDX_EXCLUDE.  If this environment variable is unset,
#  then all applicable (see % below) tankfiles of the specified type are
#  appended.
#
#  % - This script looks for the environment variable $APXDX_TYPE to
#      determine if it should append the DX table messages to tankfiles
#      associated with the decoder runs or to tankfiles associated with the
#      satellite ingest runs. DX tables bufrtab.003, bufrtab.005,
#      bufrtab.008, bufrtab.012 and bufrtab.021 in 
#      $OPSROOT/obsproc_satingest.v*/fix and 
#      $OPSROOT/decoders/decod_shared/fix are all
#      associated with a mixture of decoder and satellite ingest tanks. When
#      appending tankfiles with any of these DX table messages, it is more
#      efficient to run this script separately for decoder vs. satellite ingest
#      types.  Many satellite ingest tankfiles are very large and it can take
#      several minutes to append the DX table messages to them.  The decoder
#      cannot afford to be shut down this long.  If environment variable
#      $APXDX_TYPE is unset, it defaults to decoder-type runs.  If
#      $APXDX_EXCLUDE is set, then $APXDX_TYPE is meaningless.  All tankfile
#      subtypes (for the given DX table) not listed in $APXDX_EXCLUDE will be
#      appended, whether they are decoder tankfiles or satellite ingest tank
#      files.
#
#  $APXDX_EXCLUDE example: If $APXDX_EXCLUDE=xx002,xx030 and the given BUFR
#  database is production (or development) and the given DX table is
#  $OPSROOT/decoders/decod_shared/fix/bufrtab.255, then the script will
#  generate BUFR messages from $OPSROOT/decoders/decod_shared/fix/bufrtab.255 
#  and append them to every tankfile located within every b255 subdirectory
#  of the production (or development) database *except* for any xx002 or xx030 tankfiles.
#
#  $APXDX_TYPE example 1: If $APXDX_TYPE=satingest and $APXDX_EXCLUDE is not
#  set and the given BUFR database is production and the given DX table
#  is $OPSROOT/obsproc_satingest/v*/fix/bufrtab.005, then the script will generate
#  BUFR messages from $OPSROOT/obsproc_satingest.v*/fix/bufrtab.005 and append them
#  to every satellite ingest-generated tankfile located within every b005
#  subdirectory of the production database.  This means that tankfiles
#  xx015, xx016, xx017, xx018, xx020, xx021, xx022, xx023, xx024, xx025, xx026,
#  xx041, xx042, xx043, xx044, xx045, xx046, xx047, xx048, xx049, xx061, xx062, xx063, xx067, xx068
#  and xx069 will always be excluded, if they are present, since they are
#  decoder-generated.  All other tankfiles will be appended but only if they are owned
#  by ops.prod {this accounts for any new decoder tankfiles that might appear
#  in either the production or development database and
#  which might not yet be accounted for by this script; it
#  also accounts for any checkout tankfiles which then will not be appended}.
#  Again, this example applies only when $APXDX_EXCLUDE is not set.
#
#  $APXDX_TYPE example 2: If $APXDX_TYPE=decoder and $APXDX_EXCLUDE is not set
#  and the given BUFR database is production (or development) and the given DX table is
#  $OPSROOT/decoders/decod_shared/fix/bufrtab.005, then the script will generate
#  BUFR messages from $OPSROOT/decoders/decod_shared/fix/bufrtab.005 and append
#  them to every decoder-generated tankfile located within every b005
#  subdirectory of the production (or development) database. Decoder-generated
#  tankfiles are defined here as those being owned by either dfprod or dfdev
#  {since all satellite ingest-generated tankfiles are owned by ops.prod; it also accounts for
#  any checkout tankfiles which then will not be appended} for all BUFR types
#  associated with a table which is known to also contain satellite ingest-
#  generated tankfiles (more about this later). For the
#  $OPSROOT/decoders/decod_shared/fix/bufrtab.005 DX table, decoder-generated tankfiles
#  xx015, xx016, xx017, xx018, xx020, xx021, xx022, xx023, xx024, xx025, xx026, xx041,
#  xx042, xx043, xx044, xx045, xx046, xx047, xx048, xx049, xx061, xx062, xx063, xx067, xx068 or xx069 will
#  be appended, if they are present.  All other tankfiles (i.e., those that are
#  satellite ingest-generated) will be excluded. If the database is something
#  other than production (or development), 
#  then all tankfiles will be appended (i.e., the group is not tested). In  
#  addition, if the table is NOT one of the types which can contain satellite ingest-
#  generated tankfiles (i.e., the table is not one of bufrtab.003, bufrtab.005,
#  bufrtab.008, bufrtab.012 or bufrtab.021), then all tankfiles will be
#  appended (i.e., the group is not tested). Again, this example applies only
#  when $APXDX_EXCLUDE is not set.
#
#  $APXDX_TYPE example 3: If $APXDX_TYPE=satingest and $APXDX_EXCLUDE is not
#  set and the given BUFR database is production and the given DX table is
#  $OPSROOT/obsproc_satingest.v*/fix/bufrtab.021, then the script will generate BUFR
#  messages from $OPSROOT/obsproc_satingest.v*/fix/bufrtab.021 and append them to
#  every satellite ingest-generated tankfile located within every b021 subdirectory
#  of the production database.  This means that tankfiles xx033, xx034, xx035,
#  xx036, xx037, xx038, xx039, xx042, xx043 and xx044 will always be excluded, if they
#  are present, since they are decoder-generated.  All other tankfiles will be
#  appended UNLESS:
#       1)  They are not owned by ops.prod (for the reasons noted in
#           example 1).
#  or, if they are owned by ops.prod:
#       2)  They are one of the following b021 tankfiles: xx051, xx052, xx053,
#           xx054, xx201, xx202, xx203, xx204, xx205, xx206, xx212, xx239,
#           xx241, xx246, xx248, xx249, xx250 or xx255 and the tankfile is
#           associated with a year, month, day date directory which is older by
#           5 days or more than the current year, month, day.  These tanks are
#           very large meaning that it can take quite a bit of time to read
#           through them in order to append a table at the end.  Appending a
#           table to only the most recent 5 days in the database (rather than
#           to all 10 days which can reside in the database) allows this script
#           to complete in half the time (or more) and thus means that the
#           satellite ingest jobs will not be suspended as long as they would
#           otherwise be. There is little chance of any data more than 2-3 days
#           old being appended into these tanks. (Note: This is the only case
#           where only a portion of the tankfiles for a given type and subtype
#           in the database are appended.)
#  Again, this example applies only when $APXDX_EXCLUDE is not set.
#
#
#
#  NOTE: The script is set up to run in the Bourne shell. If you are a
#  C-shell user, enter 'sh ./run_apxdx.sh'.
#  -------------------------------------------------------------------
#  Usage: run_apxdx.sh <BUFRdbroot> <DXtable>
#
#  where:
#  <BUFRdbroot>  [path/]filename of BUFR database root directory
#  <DXtable>     [path/]filename of DX dictionary table
#
#  optional environment variable:
#  $APXDX_EXCLUDE   comma-separated list of tankfile subtypes to exclude
#                   (default is "xx999" a bogus subtype)
#  $APXDX_TYPE      either "satingest" for satellite ingest types or
#                   "decoder" for decoder types (default is "decoder")

#------------------------------------------------------------------------------

[ $# -ne 2 ] && { echo; echo "Usage: $0 <BUFRdbroot> <DXtable>";
                  echo;
                  echo "where:";
                  echo "  <BUFRdbroot> = [path/]filename of BUFR database root";
                  echo "  <DXtable>    = [path/]filename of DX table";
                  echo; exit 99; }

utilexec=${utilexec:-$bufr_ROOT/bin}

excludes=${APXDX_EXCLUDE:-xx999}
excludes_orig=$excludes

type=${APXDX_TYPE:-decoder}
if [ ${type} != 'decoder' -a ${type} != 'satingest' ]; then
  echo "Improper APXDX_TYPE setting.  APXDX_TYPE can be: unset, decoder, or satingest.  Exiting!" 
  exit
fi

BUFRdbroot=${1}

DXtable=${2}

bufrtab=`basename ${DXtable}`

echo
echo "Tanks to be appended to are in database $BUFRdbroot"
echo
echo "DX dictionary table whose BUFR messages are to be appended is $DXtable"
echo

#=============================================================================

#  Satellite ingest case pertains to only certain DX tables (assuming
#  $APXDX_EXCLUDE not set).
#  ------------------------------------------------------------------

satingest_bufrtab=NO
[ $bufrtab = bufrtab.003 -o $bufrtab = bufrtab.005 -o \
  $bufrtab = bufrtab.008 -o $bufrtab = bufrtab.012 -o \
  $bufrtab = bufrtab.021 ]  &&  satingest_bufrtab=YES

if [ $type = satingest -a $excludes_orig = xx999 -a $satingest_bufrtab = NO ]
then
    echo
    echo "---> DX dictionary table $DXtable not valid for type $type"
    echo
    echo "---> Normal exit without appending DX dictionary table to any \
tankfiles"
    echo
    exit 0
fi

#=============================================================================

#  Determine subtypes to exclude for satellite ingest case (all decoder-
#  generated subtypes) (assuming $APXDX_EXCLUDE not set).
#  ---------------------------------------------------------------------

if [ $type = satingest -a $excludes_orig = xx999 ]; then

    if [ $bufrtab = bufrtab.003 ]
    then

# for bufrtab.003 exclude:
# ------------------------
#    xx010 (GPS Radio Occultation)
        excludes=xx010   # GPS Radio Occultation

    elif [ $bufrtab = bufrtab.005 ]
    then

# for bufrtab.005 exclude:
# ------------------------
#    xx015 (GTS GOES IR satwinds)
#    xx016 (GTS GOES WV imager satwinds)
#    xx017 (GTS GOES VIS satwinds)
#    xx018 (GTS GOES WV sounder satwinds)
#    xx020 (NESDIS SCP, GOES alpha-numeric feed)
#    xx021 (GTS INSAT IR satwinds) - NOT PRODUCED AT THIS TIME
#    xx022 (GTS INSAT VIS satwinds) - NOT PRODUCED AT THIS TIME
#    xx023 (GTS INSAT WV imager satwinds) - NOT PRODUCED AT THIS TIME
#    xx024 (GTS INSAT IR satwinds - BUFR)
#    xx025 (GTS INSAT VIS satwinds - BUFR)
#    xx026 (GTS INSAT WV imager satwinds - BUFR)
#    xx041 (GTS HIMAWARI IR satwinds) - NOT PRODUCED AT THIS TIME
#    xx042 (GTS HIMAWARI VIS satwinds) - NOT PRODUCED AT THIS TIME
#    xx043 (GTS HIMAWARI WV imager satwinds) - NOT PRODUCED AT THIS TIME
#    xx044 (GTS HIMAWARI IR satwinds - BUFR)
#    xx045 (GTS HIMAWARI VIS satwinds - BUFR)
#    xx046 (GTS HIMAWARI WV imager satwinds - BUFR)
#    xx047 (GTS HIMAWARI IR satwinds - BUFR)
#    xx048 (GTS HIMAWARI VIS satwinds - BUFR)
#    xx049 (GTS HIMAWARI WV imager satwinds - BUFR)
#    xx061 (GTS METEOSAT IR satwinds)  - NOT PRODUCED AT THIS TIME
#    xx062 (GTS METEOSAT VIS satwinds) - NOT PRODUCED AT THIS TIME
#    xx063 (GTS METEOSAT WV imager satwinds) - NOT PRODUCED AT THIS TIME
#    xx067 (GTS METEOSAT IR satwinds - BUFR)
#    xx068 (GTS METEOSAT VIS satwinds - BUFR)
#    xx069 (GTS METEOSAT WV imager satwinds - BUFR)
        excludes=xx015,xx016,xx017,xx018,xx020,xx021,xx022,xx023,xx024,xx025,\
xx026,xx041,xx042,xx043,xx044,xx045,xx046,xx047,xx048,xx049,xx061,xx062,xx063,xx067,xx068,xx069

    elif [ $bufrtab = bufrtab.008 ]
    then

# for bufrtab.008 exclude:
# ------------------------
#    xx021 (AIRNOW ozone - 1 hour backward averaged, delayed)
#    xx022 (AIRNOW ozone - 8 hour backward averaged, delayed)
#    xx023 (AIRNOW ozone - 1 hour backward averaged, hourly ingest)
#    xx031 (AIRNOW PM - 1 hour backward averaged, daily ingest)
#    xx032 (AIRNOW PM - 1 hour backward averaged, hourly ingest)
        excludes=xx021,xx022,xx023,xx031,xx032

    elif [ $bufrtab = bufrtab.012 ]
    then

# for bufrtab.012 exclude:
# ------------------------
#    xx003 (GPS-IPW)
#    xx004 (GNSS-IPW/TZD)
#    xx150 (NASA Langley pixel-level cloud)
        excludes=xx003,xx004,xx150

    elif [ $bufrtab = bufrtab.021 ]
    then

# for bufrtab.021 exclude:
# ------------------------
#    xx033 (RARS AMSU-A)
#    xx034 (RARS AMSU-B)
#    xx035 (RARS HIRS-3/-4)
#    xx036 (RARS MHS)
#    xx037 (CrIS)
#    xx038 (ATMS)
#    xx039 (IASI)
#    xx042 (SEVIRI All Sky Radiances)
#    xx043 (SEVIRI Clear Sky Radiances)
#    xx044 (HIMAWARI Clear Sky Radiances)
#    xx212 (CrIS direct-broadcast)
#    xx213 (ATMS direct-broadcast)
#    xx239 (IASI direct-broadcast)
        excludes=xx033,xx034,xx035,xx036,xx037,xx038,xx039,xx042,xx043,xx044,xx212,xx213,xx239
    fi
fi

#=============================================================================

#  Generate a list of all of the subdirectories corresponding to this table.
#  -------------------------------------------------------------------------

dirname=b`basename ${DXtable} | cut -f2 -d.`

dirs="`ls -1d ${BUFRdbroot}/20*[0-9]/${dirname}`"

for dir in ${dirs}
do

    cd ${dir}

#   Generate a list of all of the tankfiles in this subdirectory, then append
#   the updated table to each one that has not been explicitly excluded.
#   -------------------------------------------------------------------------

    tank_date=99999999
    target_date=00000000
    if [ ${dirname} = b021 ]
    then

#      For b021 directory only, determine YYYYMMDD date of tankfile as well as
#      a target date which is today's YYYYMMDD minus 5 days - this may be
#      used to bypass table append for certain "old" tankfiles which are very
#      large.
#      -----------------------------------------------------------------------

        length_dir=`echo ${#dir}`
        length_dir_date_start=`expr $length_dir - 12`
        length_dir_date_end=`expr $length_dir - 5`
        tank_date=`echo $dir | \
         cut -c${length_dir_date_start}-${length_dir_date_end}`
        current_date=$(date -u '+%Y%m%d')
        target_date=`$UTILROOT/ush/finddate.sh $current_date d-5`
    fi

    tanks="`ls -1 xx*`"

    for tank in ${tanks}
    do
        if [ `echo "${excludes}" | egrep -c ${tank}` -eq 0 ]
        then 
            if [ $type = satingest -a $excludes_orig = xx999 -a \
                 `echo "${BUFRdbroot}" | egrep -c "/ops/prod/dcom"` -eq 1 -a \
                 `ls -l "${tank}" | egrep -c "ops.prod"` -eq 0 ]
            then 

#          For satellite ingest case, if this tankfile not explicitly excluded
#          and $APXDX_EXCLUDE is not set, then bypass table append if tankfile
#          is in production database (including all children) and
#          tankfile is not owned by "ops.prod" (means it is either decoder-
#          generated or checkout-generated).
#          -------------------------------------------------------------------

                echo "====> DO NOT append DX table messages to ${dir}/${tank}; \
this satingest tank is not owned by ops.prod; owner is `ls -l "${tank}" | awk \
'{print $3}'`"
            elif [ $type = decoder -a $excludes_orig = xx999 -a \
                   $satingest_bufrtab = YES -a \
                   `echo "${BUFRdbroot}" | egrep -c "/ops/(prod|dev)/dcom"` -eq 1 -a \
                   `ls -l "${tank}" | egrep -c "df(prod|dev)"` -eq 0 ]
            then 

#          For decoder case where table is one of the designated satellite
#          ingest types, if this tankfile not explicitly excluded and
#          $APXDX_EXCLUDE is not set, then bypass table append if tankfile is
#          in production or development database (including all children) and
#          tankfile is not owned by "dfprod" or "dfdev" (means it is
#          either satellite ingest-generated or checkout-generated).
#          ------------------------------------------------------------------

                echo "====> DO NOT append DX table messages to ${dir}/${tank}; \
this decoder tank is not owned by dfprod or dfdev; owner is `ls -l "${tank}" | awk \
'{print $3}'`"
            else
                if [ $type = satingest -a ${dirname} = b021 -a \
                     $excludes_orig = xx999 ]; then
                    if [ $tank = xx023 -o $tank = xx024 -o $tank = xx025 -o \
                         $tank = xx027 -o $tank = xx028 -o $tank = xx041 -o \
                         $tank = xx051 -o $tank = xx052 -o $tank = xx053 -o \
                         $tank = xx054 -o $tank = xx123 -o $tank = xx201 -o \
                         $tank = xx202 -o $tank = xx203 -o $tank = xx204 -o \
                         $tank = xx205 -o $tank = xx206 -o $tank = xx241 -o \
                         $tank = xx242 -o $tank = xx246 -o $tank = xx248 -o \
                         $tank = xx249 -o $tank = xx250 -o $tank = xx254 -o \
                         $tank = xx255 ]
                    then
                        if [ $tank_date -le $target_date ]
                        then

#          For satellite ingest case, if this tankfile not explicitly excluded
#          and $APXDX_EXCLUDE is not set, then bypass table append if tankfile
#          YYYYMMDD date is 5 or more days older than today's YYYYMMDD and
#          tankfile is a one of a list of subtypes that is known to be large
#          and static after 2-3 days.
#          -------------------------------------------------------------------

                            echo "====> DO NOT append DX table messages to \
${dir}/${tank} - this large tank is too old"
                            continue
                        fi
                    fi
                fi

#         Having made it to this point, the tankfile is appended with the
#         DX table.
#         ---------------------------------------------------------------

		echo "Using ${utilexec}/apxdx to append DX table messages to \
 ${dir}/${tank}"
                ${utilexec}/apxdx ${dir}/${tank} ${DXtable}
            fi
        else
            if [ $excludes_orig = xx999 ]; then

#   This tankfile has been explicitly excluded from appending - it is a
#   decoder-generated tankfile when $APXDX_TYPE=satingest (imported variable
#   $APXDX_EXCLUDE is not set).
#   ------------------------------------------------------------------------

                echo "====> DO NOT append DX table messages to ${dir}/${tank}; \
tank is on internally-defined exclusion list"

            else

#   This tankfile has been explicitly excluded from appending - it is included
#   in imported variable $APXDX_EXCLUDE.
#   --------------------------------------------------------------------------

                echo "====> DO NOT append DX table messages to ${dir}/${tank}; \
tank is in imported list \$APXDX_EXCLUDE"
            fi
        fi
    done

done

exit 0
