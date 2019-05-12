# git 基本命令学习  
  
##### git 是如何工作的？  
1. 在git 服务器上创建一个远程仓库。  
2. 将远程仓库克隆到本地仓库。  
3. 向本地仓库中添加一个文件，并提交修改。  
4. 将本地仓库内容推送到远程仓库。  
5. 在远程仓库上修改文件，并提交修改。  
6. 将远程仓库内容拉取到本地仓库。  
7. 在本地仓库中创建一个分支，修改文件，并提交。  
8. 将本地仓库内容推送到远程仓库。  
9. 在远程仓库的分支上，新建一个“pull request”。  
   （请求master主支合并这个分支。）  
10. 在远程仓库的master主支上，执行与其他分支合并的操作。  
  
  
##### git 是一个版本控制软件，主要用于软件开发过程。  
git 包括两部分，一个客户端，一个服务器端。  
我们可以仅仅只使用其中的一个部分，但一般来说，我们两部分都使用。  
##### 客户端，服务端身份认证  
客户端和服务端进行通信时，需要进行身份确认。  
常用的验证方式有两个，一个是SSH密钥，一个是GPG密钥。  
这里我们使用SSH密钥。  
在U/Linux上，我们可以通过下面的命令生成SSH密钥：  
` $ ssh-keygen -t rsa -b 4096 -C "your_email@example.com" `  
-t type of key to create  
-b bits in the key to create  
-C comment  
这里我们使用的算法类型是rsa，密钥长度是4096位。备注是引号内内容。  
这会生成两个文件，默认是~/.ssh/id_rsa, ~/.ssh/id_rsa.pub.  
前者是私钥文件，后者是公钥文件。  
然后，我们需要将公钥文件中的内容添加到github账户中。  
登录github账户，在Setting > SSH and GPG keys 中添加SSH密钥。  
至此，客户端和服务段身份认证部分完成。  

## git 操作
##### 新建一个远程仓库  
在github网站上，点击新建仓库。  
##### 克隆远程仓库到本地  
` $ git clone https://github.com/369329303/test.git `  
##### 在本地仓库的master分支添加一个新文件。  
` $ touch file1.txt `  
##### 查看master分支的状态  
` $ git status `  
##### 将修改的文件从当前工作目录添加入暂存区  
` $ git add file1.txt `  
##### 查看master分支的状态  
` $ git status `  
##### 将修改的文件从暂存区添加入提交历史中  
` $ git commit -m "add a file1.txt" `  
##### 查看提交历史  
--all: 打印出所有的提交日志记录  
--decorate: 打印出提交历史的ref名字  
--oneline: 每个历史记录占据一行  
--graph: 在提交历史左边添加基于文字的图形符号  
简记为A-DOG 一只狗.  
` $ git log --all --decorate --oneline --graph `   
##### 查看master分支的状态  
` $ git status `  
##### 将本地仓库中的修改推送到远程仓库中  
` $ git push `  
##### 查看master分支的状态  
` $ git status `  
##### 创建一个新分支并切换到该分支  
` $ git checkout -b new_branch `  
##### 在新分支添加一个新文件，添加到暂存区，再加入提交历史中  
` $ touch file2.txt `  
` $ git add . `  
` $ git commit -m "add file2.txt" `  
##### 切换到 master 分支  
` git checkout master `  
##### 将新分支融合到 master 分支中  
` git merge new_branch `  
  
## 显示区别  
##### 比较当前目录和暂存区的区别  
` git diff `  
##### 比较某个文件在当前目录和暂存区的区别  
` git diff -- filepath `  
##### 比较暂存区和提交历史的区别 --cached/staged  
` git diff --staged commit_sha1 `  
##### 比较某个文件在暂存区和提交历史中的区别  
` git diff --staged commit_sah1 -- filepath `  
##### 显示当前工作目录和提交历史的区别  
` git diff commit_sha1 `  
##### 显示某个文件在当前工作目录和提交历史中的区别  
` git diff commit_sha1 -- filepath `  
##### 比较两次提交历史的区别  
` git diff commit1_sha1 commit2_sha1 `  
##### 显示某个文件在两次提交历史中的区别  
` git diff commit1_sha1 commit2_sha1 -- filepath`  
  
## 显示文件内容  
##### 显示某个文件在某次提交历史中的内容  
` git show commit_sha1:filepath `  
  
## 修改当前目录  
##### 放弃所有对于当前目录的更改  
` git stash # 之后使用 git stash pop 可以恢复过来 `  
或者  
` git checkout -- filepath # 永久性的 `  
或者  
` git reset --hard # 永久性的 `  
  
## 修改暂存区  
##### 放弃所有对于当前暂存区的更改  
` git reset [--mixed]  # 放弃所有更改 `  
或者  
` git reset [--mixed] HEAD filepath  # 放弃对于单个文件的修改 `  
或者  
` git stash  # 放弃所有更改，但是保存这些更改以便今后使用 `  
相关知识请参考[撤销更改](https://docs.gitlab.com/ee/topics/git/numerous_undo_possibilities_in_git/)  
  
## 修改提交历史  
注意：一旦将本地历史同步到远程仓库后，请不要在修改本地历史。这会混淆其他查看者。  
##### 修改最后一次的提交历史  
` git commit --amend `  
##### 修改最后3次提交历史  
` git rebase -i HEAD~3 `
这会用编辑器打开一个文件，里面列着一系列的提交历史。
edit： 用于修改和分割提交历史（分割提交历史需要git reset, add , commit的配合）
squash: 用于合并提交历史的
相关知识请参考[重写提交历史](https://git-scm.com/book/en/v2/Git-Tools-Rewriting-History)  
  
完。
