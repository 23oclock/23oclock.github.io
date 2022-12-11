+++
title = "lexical-binding"
author = ["flumine@qq.com"]
date = 2022-12-12T00:32:00+08:00
lastmod = 2022-12-12T00:32:28+08:00
tags = ["emacs"]
categories = ["技术"]
draft = false
+++

在看 purcell 的 emacs 配置，发现他在启动 emacs 时，首先调整了 emacs 的垃圾回收阈值 gc-cons-threshold，将设置为 20M（20\*1024\*1024，默认为 800000），并在启动时临时调到 128M。具体使用的配置为

```elisp
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
```

但是我直接复制进我的 init.el 文件中时，启动 emacs 却报错，说找不到 normal-gc-cons-threshold 这个变量。经过查询之后发现，需要在 init.el 文件的头部加上一段魔法注释

```elisp
;;; -*- lexical-binding: t -*-
```

这段注释有什么用呢？原来 elisp 默认是使用 dynamic scope 的，即存储函数的时候不会把函数使用到的自由变量也一起存起来，只有到用的时候再去附近找，找到哪个用哪个，找不到就报错。加了这段注释后，存储函数的时候会把使用到的自由变量一起存起来（一起存起来的所有内容称为一个closure，闭包）。
