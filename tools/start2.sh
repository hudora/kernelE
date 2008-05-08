#!/bin/sh

# Funktionalitaet um den Kernel im Hintergrund laufen zu lassen - unvollstaendig

[ "x" = "x$NODENAME" ] && NODENAME=kernelE
[ "x" = "x$NODE_IP_ADDRESS" ] && NODE_IP_ADDRESS=0.0.0.0
[ "x" = "x$NODE_PORT" ] && NODE_PORT=5672

MNESIA_BASE=/usr/local/kernelE/mnesia
LOG_BASE=/usr/local/kernelE/log
ERL_ARGS="+K true +A30 -kernel inet_default_listen_options [{sndbuf,16384},{recbuf,4096}]"
MNESIA_DIR=${MNESIA_BASE}/${NODENAME}

erl \
    -pa ./ebin \
    -heart \
    -noinput \
    -boot start_sasl \
    -sname ${NODENAME} \
    -s mypl start \
    +W w \
    ${ERL_ARGS} \
    -sasl errlog_type error \
    -sasl sasl_error_logger '{file,"'${LOG_BASE}'/'${NODENAME}'-sasl.log"}'\
    -os_mon start_cpu_sup true \
    -os_mon start_disksup false \
    -os_mon start_memsup false \
    -os_mon start_os_sup false \
    -mnesia dir "\"${MNESIA_DIR}\"" \
    -setcookie voom3OhlXeeg8vuD \
    "$@"

#erl -pa ./ebin ./vendor/eunit/ebin -sname mypl_produktion@airvent -setcookie voom3OhlXeeg8vuD \
#    -s mypl start
