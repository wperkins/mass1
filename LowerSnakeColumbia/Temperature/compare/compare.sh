#! /bin/sh
# -------------------------------------------------------------
# file: compare.sh
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 25, 2001 by William A. Perkins
# Last Change: 2018-03-01 15:25:07 d3g096
# -------------------------------------------------------------

set -x

# -------------------------------------------------------------
# damname
# -------------------------------------------------------------
damname() {
    case $1 in
        GCL) name="Grand Coulee" ;;
        CHJ) name="Chief Joseph" ;;
        WEL) name="Wells" ;;
        RRH) name="Rocky Reach" ;;
        RIS) name="Rock Island" ;;
        WAN) name="Wanapum" ;;
        PRD) name="Priest Rapids" ;;
        LWG) name="Lower Granite" ;;
        LGS) name="Little Goose" ;;
        LMN) name="Lower Monumental" ;;
        IHR) name="Ice Harbor" ;;
        MCN) name="McNary" ;;
        JDA) name="John Day" ;;
        TDA) name="The Dalles" ;;
        BON) name="Bonneville" ;;
        PEKI) name="PEKI" ;;
        SPALDING) name="SPALDING" ;;
        lEWI) name="LEWI" ;;
        100-B) name="100-B Area" ;;
        100-N) name="100-N Area" ;;
        100-D) name="100-D Area" ;;
        100-H) name="100-H Area" ;;
        100-F) name="100-F Area" ;;
        300) name="300 Area" ;;
        *)   name="Unknown"
    esac
    echo $name
}

# -------------------------------------------------------------
# fmsname
# -------------------------------------------------------------
fmsname() {
    case $1 in
        PRXW) name="Priest Rapids" ;;
        LGNW) name="Lower Granite" ;;
        LGSW) name="Little Goose" ;;
        LMNW) name="Lower Monumental" ;;
        IDSW) name="Ice Harbor" ;;
        MCPW) name="McNary" ;;
        JHAW) name="John Day" ;;
        TDDO) name="The Dalles" ;;
        WRNO) name="Bonneville" ;;
        PEKI) name="Peck" ;;
        LEWI) name="Lewiston" ;;
        SPALDING) name="Spaulding" ;;
        *)   name="Unknown"
    esac
    echo $name
}


# -------------------------------------------------------------
# fbloc
# return the time series number for the forebay of the specified
# project
# -------------------------------------------------------------
fbloc() {
    case $1 in
        LWG) loc="865" ;;
        LGS) loc="1085" ;;
        LMN) loc="1637" ;;
        IHR) loc="1875" ;;
        MCN) loc="2345" ;;
        JDA) loc="298" ;;
        TDA) loc="3328" ;;
        BON) loc="4317" ;;
        *)   loc="0000"
    esac
    echo $loc
}

# -------------------------------------------------------------
# twloc
# return the time series number for the tailwater of the specified
# project or location code
# -------------------------------------------------------------
twloc() {
    case $1 in
        PRD) loc="331" ;;
        LWG) loc="101" ;;
        LGS) loc="121" ;;
        LMN) loc="181" ;;
        IHR) loc="201" ;;
        MCN) loc="251" ;;
        JDA) loc="311" ;;
        TDA) loc="351" ;;
        BON) loc="451" ;;
        # 100-B) loc="3351" ;;
        # 100-N) loc="3363" ;;
        # 100-D) loc="3367" ;;
        # 100-H) loc="3382" ;;
        # 100-F) loc="3393" ;;
        # 300) loc="33150" ;;
        *)   loc="0000"
    esac
    echo $loc
}

# -------------------------------------------------------------
# fmsloc
# -------------------------------------------------------------
fmsloc() {
    case $1 in
        PEKI) loc="66" ;;
        SPALDING) loc="629" ;;
        LEWI) loc="658" ;;
        LGNW) loc="104" ;;
        LGSW) loc="124" ;;
        LMNW) loc="184" ;;
        IDSW) loc="2015" ;;
        PRXW) loc="17" ;;
        MCPW) loc="259" ;;
        JHAW) loc="315" ;;
        TDDO) loc="3511" ;;
        WRNO) loc="559" ;;
        SPALDING) loc="629" ;;
        LEWI) loc="646" ;;
        *)   loc="0000"
    esac
    echo $loc
}

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------

fblist="LWG LGS LMN IHR MCN JDA TDA BON"
fmslist="PEKI SPALDING LEWI LGNW LGSW LMNW IDSW MCPW JHAW TDDO WRNO"

R="R --no-save --slave"
# R="R --no-save"

bcdir="../../BCFiles"
obsdir="../../Observed"

dotbl=""

dodisch="no"                    # set to "yes" for discharge/stage stats/plots
dotemp="yes"                    # set to "yes" for temperature stats/plots

                                # need this for R so it wont use
                                # daylight savings time
TZ="UTC"
export TZ

year=2015
if [ -n "$1" ]; then
    year=$1
fi

# -------------------------------------------------------------
# main program
# -------------------------------------------------------------
statlist=""

dir=".."
statfile="$dir/statistics.dat"

if [ -f $statfile ]; then
    mv -f $statfile ${statfile}.old
fi
cp /dev/null $statfile

statlist="$statlist $statfile"

if [ x"$dodisch" == xyes ]; then
    for code in $fblist; do
        loc=`fbloc $code`
        name=""
        mname=`damname $code`
        
        sed -e "s;@SIMFILE@;$dir/ts${loc}.out;g" \
            -e "s;@OBSFILE@;$bcdir/Stage/${code}-FBE.dat;g" \
            -e "s;@VAR@;wselev;g" \
            -e "s;@STATSFILE@;$statfile;g" \
            -e "s;@OUTPS@;$dir/$code-scatter-FBE.eps;g" \
            -e "s;@DATA@;Forebay Stage, feet;g" \
            -e "s;@TAG@;$code FBE;g" \
            -e "s;@NAME@;$name;g" \
            -e "s;@FACTOR@;1.0;g" \
            -e "s;@YEAR@;$year;g" compare.R  > tmp.R 
        $R < tmp.R
        
        sed -e "s;@SIMFILE@;$dir/ts${loc}.out;g" \
            -e "s;@OBSFILE@;$bcdir/Flow/${code}-Qtotal.dat;g" \
            -e "s;@VAR@;discharge;g" \
            -e "s;@STATSFILE@;$statfile;g" \
            -e "s;@OUTPS@;$dir/$code-scatter-QTL.eps;g" \
            -e "s;@DATA@;Discharge, cfs;g" \
            -e "s;@TAG@;$code QTL;g" \
            -e "s;@NAME@;$name;g" \
            -e "s;offset=0;offset=0.5/24;g"  \
            -e "s;@FACTOR@;1.0;g" \
            -e "s;@YEAR@;$year;g" compare.R  > tmp.R 
        $R < tmp.R

    done
fi


if [ x"$dotemp" == xyes ]; then
    for code in $fmslist; do
        loc=`fmsloc $code`

        obs="$obsdir/Temperature/${code}-Temp.dat"
        case $code in
            SPALDING)
                obs="$obsdir/Temperature/Spalding-Temperature.dat"
                ;;
            PECK)
                obs="$obsdir/Temperature/Peck-Temperature.dat"
                ;;
            *)
                
        esac

        sed -e "s;@SIMFILE@;$dir/ts${loc}.out;g" \
            -e "s;@OBSFILE@;$obs;g" \
            -e "s;@VAR@;temp;g" \
            -e "s;@STATSFILE@;$statfile;g" \
            -e "s;@OUTPS@;$dir/$code-scatter-Temp.eps;g" \
            -e "s;@DATA@;Temperature, deg Celcius;g" \
            -e "s;@TAG@;$code Temp;g" \
            -e "s;@NAME@;$name;g" \
            -e "s;@FACTOR@;1.0;g" \
            -e "s;@YEAR@;$year;g" compare.R  > tmp.R 
        $R < tmp.R
    done
fi

#perl fbtable.pl $statlist > fbstats.tex
