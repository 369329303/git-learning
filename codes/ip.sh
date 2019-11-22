#!/bin/bash

if [[ $EUID -ne 0 ]]; then
    echo "This script must run as root!"
    exit 1
fi

s1="default via 192.168.43.1 dev wlp3s0 proto dhcp metric 600"
s2="default via 10.0.80.254 dev enp2s0 proto static metric 100"
s3="default via 192.168.43.1 dev wlp3s0 proto dhcp metric 60"
s4="10.0.0.0/16 via 10.0.80.254 dev enp2s0"

r1=$(ip r | grep "$s1")
if [[ ! -z ${r1// /} ]]; then
    ip route del $s1
fi

r2=$(ip r | grep "$s2")
if [ ! -z ${r2// /} ]; then
    ip route del $s2
fi

r3=$(ip r | grep "$s3")
if [ -z ${r3// /} ]; then
    ip route add $s3
fi

r4=$(ip r | grep "$s4")
if [ -z ${r4// /} ]; then
    ip route add $s4
fi

