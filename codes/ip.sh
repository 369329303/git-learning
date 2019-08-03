#!/bin/bash

if [[ $EUID -ne 0 ]]; then
    echo "This script must run as root!"
    exit 1
fi

# ip route del default via 192.168.1.1 dev enp2s0
ip addr add 10.0.80.106/24 dev enp2s0
ip route add 10.0.0.0/16 via 10.0.80.254 dev enp2s0
