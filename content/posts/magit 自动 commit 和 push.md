+++
title = "magit 自动 commit 和 push"
author = ["flumine@qq.com"]
date = 2022-12-14T23:04:00+08:00
lastmod = 2022-12-14T23:08:45+08:00
categories = ["技术"]
draft = false
+++

在使用 emacs 写东西的时候，当我把写好的 org 文件转成 markdown 文件并放到 hugo 的目录下的时候，我希望可以使用 magit 自动把新添加的文件 stage, commit, 并且 push 到 git 仓库中。于是我查了一下，联合几个 magit 接口，搞了个函数：

```elisp
(defun auto-git-commit-and-push-blog()
  "auto stage and commit blog by magit"
  (interactive)
  (cd (concat org-file-path blog-dir-name))  ;; 跳转到 blog 目录，其中 blog-dir-name 这个变量需要提前定义好
  (magit-stage-modified "--all")  ;; 将所有修改都 stage, 包括没有 track 的文件
  (when (magit-anything-staged-p)  ;; 有 stage 的内容时，才能 commit, 否则会报错
    (setq msg (format-time-string "commit by magit in emacs@%Y-%m-%d %H:%M:%S" (current-time)))  ;; 自动生成 commit 信息
    (message "commit message is %s" msg)
    (magit-commit-create (list "-m" msg)))
  (magit-push-current-to-upstream nil))  ;; 自动 push, 当然前提是 git 仓库已经添加了远程仓库，已经确定了对应提交的分支
```

这样就可以通过这个函数，将 git 仓库自动 push 到远程仓库了。当然，把其中的 blog-dir-name 改成其他的 git 仓库目录，也可以自动 commit 和 push 其他仓库。这样会方便一些，尤其是不想写 commit 信息（commit 信息无意义）的时候。
