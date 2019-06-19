## 将私钥和证书转成pkcs12的格式：  
` openssl pkcs12 -export -out certificate.pfx -inkey privateKey.key -in certificate.crt `  
  
## 制作自签证书：  
` openssl req -x509 -newkey rsa:4096 -keyout key2.pem -out cert2.pem -days 365 -nodes -subj "/CN=client" `  
  
## enable HTTPS 网站：  
```
a2enmod ssl
a2ensite ssl1
```
  
## 查看apache的错误日志：  
` tail -f /var/log/apache2/error.log `  
  
## 检查apache的配置文件是否正确：  
` apache2ctl configtest `  
  
## 显示apache所加载的模块：  
` apache2ctl -M `  
  
## 常见模块的加载：  
` a2enmod proxy proxy_ajp proxy_http rewrite deflate headers proxy_balancer proxy_connect proxy_html `  
  
## 反向代理配置：
```
ProxyPass "/images"  "http://www.example.com/"
ProxyPassReverse "/images"  "http://www.example.com/"
```

## 正向代理配置：  
```
ProxyRequests On
ProxyVia On
<Proxy "*">
  Require host internal.example.com
</Proxy>
```
  
## 代理证书配置：  
```
SSLProxyEngine On
SSLProxyMachineCertificateFile  "/home/jack/CA/certkey2.pem"
```
  
## scp文件传输配置：  
` scp -r CA/ jack@192.168.192.128:/home/jack/ `  
  
## tcpdump 常见参数：  
` tcpdump -i ens33 -nn -vvv -XX port 8080 -l -s0 | grep GET `  
  
## emacs  
#### 删除行尾空白符：  
` delete-trailling-whitespace `  
#### 删除空白行：  
` delete-blank-lines `  
#### 删除匹配行：  
` delete-matching-lines `  
#### 查找匹配行：  
` list-matching-lines `  
  
## ssh公私钥生成：  
` ssh-keygen -t rsa -b 4096 -C "jack@example.com" `  
  
## 将ssh公钥文件传输到远程主机：  
` ssh-copy-id jack@192.168.192.1 `  
  
## 在chrome中查看连接的tls版本信息：  
` 打开开发者工具 > 选择security标签栏 `  
  
## 在firefox中查看连接的tls版本信息：  
` 点击锁标志 > 查看更多信息 > 技术细节 `  
  
## wireshark中ssl筛选：  
```
ssl
ssl.record.version == 0x0300/0x0301/0x0302/0x0303
  	              SSL3.0/TLS1.1/TLS1.2/TLS1.3
```
