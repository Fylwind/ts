#!/bin/sh

# note: base time for an empty script is ~110ms on Windows

# TODO: we still have access to "$@" as an array and haven't used it for
# anything yet; it _might_ work as a stack, but then we can't use functions
# since they would always shadow the previous one ...

# we shouldn't use these flags normally,
# but our code must tolerate them
set -e
set -u

# ----------------------------------------------------------------------------
# runtime library
# ----------------------------------------------------------------------------
# if a variable is needed, _ts_rt_rN is used where N is an integer;
# these are called the "internal registers"
# (be very careful with this: functions that use these cannot call other
# functions that also use these!)

_ts_rt_n=0

_ts_rt_set() {
   eval "$1"'=$2'
}

_ts_rt_get() {
   eval "$1"'=$'"$2"
}

# we want to use '$((...))' rather than 'expr ...' where possible
# as the former is much faster
#
# SLOW: this if-check alone costs 50ms on Windows!
# perhaps we should find cheaper methods even if they are less reliable,
# and only fallback to subshell if absolutely necessary?
if ( eval '[ $((1 + 2)) = 3 ]' >/dev/null 2>&1 )
then
    eval '_ts_rt_expr() {
        _ts_rt_r1=$1
        shift
        _ts_rt_r2=$(($@))
        _ts_rt_set "$_ts_rt_r1" "$_ts_rt_r2"
        unset _ts_rt_r1 _ts_rt_r2
    }'
else
    _ts_rt_expr() {
        _ts_rt_r1=$1
        shift
        _ts_rt_r2=`expr "$@"`
        _ts_rt_set "$_ts_rt_r1" "$_ts_rt_r2"
        unset _ts_rt_r1 _ts_rt_r2
    }
fi

# _ts_push VALUE
_ts_push() {
    _ts_rt_set _ts_s"$_ts_rt_n" "$1"
    _ts_rt_expr _ts_rt_n "$_ts_rt_n" + 1
}

# _ts_pop VARNAME
_ts_pop() {
# @debug@
    if [ "$_ts_rt_n" -eq 0 ]
    then
        echo >&2 "shrt panic: popping an empty stack"
        exit 1
    fi
# @end@
    _ts_rt_expr _ts_rt_n "$_ts_rt_n" - 1
    _ts_rt_get "$1" _ts_s"$_ts_rt_n"
}

# ----------------------------------------------------------------------------
# these functions use the proper calling convention
# but also use internal registers (_ts_rt_rN)
# ----------------------------------------------------------------------------
if ( x=aba && eval '[ ${x//a/cd} = cdbcd ]' >/dev/null 2>&1 )
then
    _ts_escape() {
        _ts_r=\'${1//\'/\'\\\'\'}\'
    }
else
    _ts_escape() {
        _ts_rt_r1="s/'/'\\\\''/g"
        # (this costs 190ms on Windows!)
        _ts_r=\'`printf "%s" "$1" | sed "$_ts_rt_r1"; printf "'"`
        unset _ts_rt_r1
    }
fi

# ----------------------------------------------------------------------------
# these functions use the proper calling convention
# ----------------------------------------------------------------------------

_ts_apply() {
    _ts_pop _ts_rt_r1
# @debug@
    if [ "x$_ts_rt_r1" = x ]
    then
        echo >&2 "shrt panic: empty function"
        exit 1
    fi
# @end@
    eval '_ts_rt_r2() {
'"$_ts_rt_r1"'
}'
    unset _ts_rt_r1
    _ts_rt_r2 "$_ts_r"
    unset -f _ts_rt_r2
}

_ts_concat() {
    _ts_escape "$1"
    _ts_r='_ts_r='$_ts_r'$1'
}

_ts_print() {
    echo "%s\n" "$1"
}
