#!/bin/bash

iptables -t nat -F
iptables -t filter -F


iptables -t nat -A PREROUTING -p tcp --dport 8001 -j DNAT --to-destination 64.227.42.205:8001
iptables -t nat -A PREROUTING -p udp --dport 8001 -j DNAT --to-destination 64.227.42.205:8001
iptables -t nat -A PREROUTING -p tcp --dport 8002 -j DNAT --to-destination 64.227.42.205:443
iptables -t nat -A PREROUTING -p udp --dport 8002 -j DNAT --to-destination 64.227.42.205:443
iptables -t nat -A POSTROUTING -p all -d 64.227.42.205 -j SNAT --to-source 172.17.0.214

iptables -I INPUT -s 140.205.201.0/28 -j DROP
iptables -I INPUT -s 140.205.201.16/29 -j DROP
iptables -I INPUT -s 140.205.201.32/28 -j DROP
iptables -I INPUT -s 140.205.225.192/29 -j DROP
iptables -I INPUT -s 140.205.225.200/30 -j DROP
iptables -I INPUT -s 140.205.225.184/29 -j DROP
iptables -I INPUT -s 140.205.225.183/32 -j DROP
iptables -I INPUT -s 140.205.225.206/32 -j DROP
iptables -I INPUT -s 140.205.225.205/32 -j DROP
iptables -I INPUT -s 140.205.225.195/32 -j DROP
iptables -I INPUT -s 140.205.225.204/32 -j DROP
iptables -P INPUT ACCEPT
apt install iptables-persistent -y
netfilter-persistent save
