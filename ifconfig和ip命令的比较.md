# ifconfig 和 ip 命令的比较
## 1. 显示所有网卡的IP地址和活动状态
```
ifconfig
ip addr/link show
```

## 2. 显示某个网卡的IP地址和活动状态
```
ifconfig ens33
ip addr/link show dev ens33
```

## 3. 改变某个网卡的活动状态
```
ifconfig ens33 up/down
ip link set ens33 up/down
```

## 4. 修改某个网卡的IP地址
```
ifconfig ens33 192.168.191.17 netmask 255.255.255.0 ens33
ip addr add/del 192.168.191.17/24 dev ens33
```

## 5. 显示路由表的内容
```
route
ip route show
```

## 6. 查询到达某个指定IP地址的数据包被路由出去的网卡和第一个网关
```
ip route get 8.8.8.8
```

## 7. 改变路由表的内容
```
route add/del -net 192.168.191.0/24 gw 192.168.192.2 dev ens33
ip route add/del 192.168.191.0/24 via 192.168.192.2 dev ens33
```