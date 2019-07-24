## grep在所有文件中搜索文本'hello'
```
grep -rnw . -e 'hello'
-r: recursively
-n: line number
-w: whole word
-e: pattern
```

## wget小技巧
```
wget -A pdf,jpg -m -p -E -k -K -np http://site/path/
或者
wget --accept pdf,jpg --mirror --page-requisites --adjust-extension --convert-links --backup-converted --no-parent http://site/path/
```

## php上传文件
1. 修改 /etc/php/7.2/apache2/php.ini文件

2. 设置
```
file_uploads = On
post_max_size = 100M
upload_max_filesize = 100M
```
用phpinfo()函数来验证。

3. 代码
[参考这个页面](https://gist.github.com/taterbase/2688850)

当前目录下也复制了一份：upload.php

## ctags小技巧
```
/usr/bin/ctags -eR .
-e: tag for emacs
-R: recursively
```


## 对一个目录下所有的.h .c 文件使用 clang-format重新排版
```
find . -iregex .\*\\.[ch] | xargs ls -lh | grep -P -v '\dM'  | awk '{print $NF}' | xargs clang-format -i --style=google
-P: Perl Compatible Regular Expression
find . -iregex .\*\\.[ch] | xargs clang-format -i
-iregex: 大小写不敏感正则表达式
xargs: 将前一个输出作为下一个的输入
-i: inplace替换，不输出到终端
```

## 批量对一个目录DIR下文件从gbk转成utf-8,并放入新创建的newDIR目录中
find DIR -type d -exec mkdir -p newDIR/{} \;
find DIR -type f -exec iconv -f GBK -t UTF-8 {} -o newDIR/{} \;

## 对当前目录下的所有文件进行替换
find . -type f -exec sed -i 's/foo/bar/g' {} +

## edit remote files
C-x C-f /ssh:user@192.168.1.5:/usr/share/nginx/html/index.html

## openssl
```
生成自签证书：openssl req -newkey rsa:2048 -nodes -keyout cakey.pem -x509 -days 3650 -out cacert.pem
生成2048位的RSA密钥：openssl genrsa -out example.key 2048
生成证书CSR：       openssl req -new -key example.key -out example.csr -subj="/CN=www.au.com"
显示一张CSR的文本内容： openssl req -in example.org.csr -noout -text
显示一张证书的文本内容： openssl x509 -in example.org.crt -noout -text
-out: 是否显示文件内容
对CSR文件签名：openssl ca -config openssl.cnf -in example.csr -out example.crt
获取带有SNI的服务器证书：openssl s_client -showcerts -servername www.example.com -connect www.example.com:443 </dev/null | openssl x509 -noout -text
		     openssl s_client -showcerts -servername www.github.com -connect www.github.com:443 < /dev/null | openssl x509 -outform PEM > www.github.com.pem
生成pfx/p12个人证书：openssl pkcs12 -export -out mycert.p12 -inkey mykey.pem -in mycert.pem -certfile more.crt
		     
```