+++
title = "org to hugo"
author = ["flumine@qq.com"]
date = 2022-12-12T22:57:00+08:00
lastmod = 2022-12-12T23:26:43+08:00
tags = ["emacs"]
categories = ["技术"]
draft = false
+++

## 目标 {#目标}

使用 ox-hugo 导出 org mode 文件，并发布到 github page 上去。


## 使用 {#使用}

```elisp
(use-package ox-hugo
:pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
:after ox)
```

我现在使用的方式是，用一个文件存储一中分类的文章，每个二级标题表示一篇文章。因此在文件的开头可以加上如下配置：

```elisp
#+author: xxxx
#+filetags: @技术
#+hugo_auto_set_lastmod: t
#+SEQ_TODO: TODO DRAFT DONE
```

表示本文件中的文章都属于“技术”分类，让 ox-hugo 自动配置文章完成时间，且把文章的状态分为三类，一类是还没写 (TODO)，一类是写了但没完成 (DRAFT), 一类是完成 (DONE).
另外，还需要在每篇文章（每个二级标题）下，添加如下性质：

```elisp
:PROPERTIES:
:EXPORT_FILE_NAME: use-package
:END:
```

这是必须添加的属性，用来确定文件名，否则无法导出文件。
也可以一个 org 文件生成一篇文章，同样，也需要至少在文件开头添加一个标题属性才可导出：

```elsip
#+title: xxxx
```

在文件的末尾添加如下注释，可以实现在保存文件时自动转换成 markdown 文件，并发送到 hugo 的 content 目录下。

```elisp
* Footnotes
* COMMENT Local Variables                          :ARCHIVE:
  # Local Variables:
  # eval: (org-hugo-auto-export-mode)
  # End:
```


## 更多属性设置 {#更多属性设置}

参见 [org to hugo](https://ox-hugo.scripter.co/doc/org-meta-data-to-hugo-front-matter/).


## <span class="org-todo todo DRAFT">DRAFT</span> test {#test}

测试。
