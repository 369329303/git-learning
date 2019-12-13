#!/bin/bash

# 文件名: ip.sh
# 功能: 用于将默认为路由从 enp2s0 改为 wlp3s0, 并保持能够连接公司内网

if [[ $EUID -ne 0 ]]; then
    echo "This script must run as root!"
    exit 1
fi

ip addr flush dev enp2s0
ip addr add 10.0.80.105/24 dev enp2s0

# 删除所有路由缓存
ip route flush cache

# 删除范围为global的默认路由
ip route flush scope global

# 添加 10.0.0.0/16 网段
ip route add 10.0.0.0/16 via 10.0.80.254 dev enp2s0

# 添加默认路由
ip route add default via 192.168.43.1 dev wlp3s0
