sudo ip route del default via 192.168.43.1 dev wlp3s0 proto dhcp metric 600
sudo ip route add default via 192.168.43.1 dev wlp3s0 proto dhcp metric 60
sudo ip route add 10.0.0.0/16 via 10.0.80.254 dev enp2s0
