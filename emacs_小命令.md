## 改变文件的编码
```
M-x revert-buffer-with-coding-system
```
## 转换文件的编码格式
```
M-x set-buffer-file-coding-system
```

## 代码自动补全
```
prerequisites: apt install cmake libclang-dev
install: irony-mode
```

## 设置编码的优先级
```
(prefer-coding-system 'gbk)
(prefer-coding-system 'utf-8)
```
```
结果为：
1. utf-8
2. gbk
```

## 查看当前文档的编码属性
```
C-h C 或者 C-h v buffer-file-coding-system
```