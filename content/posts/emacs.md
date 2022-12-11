+++
title = "emacs"
draft = false
+++

## lexical-binding {#lexical-binding}

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


## use-package {#use-package}

use-package 是 emacs 的一个插件，对于管理 emacs 的其他插件的载入和配置非常有用（但它并非是一个包管理器，即它不负责安装卸载等操作，只是调用包管理器的接口）。

use-package 的安装方法很简单，直接 package-install即可。不过为了更方便，我一般这样在 init.el 文件中配置默认安装 use-package：

```elisp
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
```

使用 use-package 管理插件也很方便，最简单的使用例子为

```elisp
(use-package evil
  :config
  (evil-mode))
```

冒号后面的 config 关键字表示，载入 evil 包之后的设置。注意这段代码不会默认自动把 evil 包安装（因为 use-package 不是包管理器），需要添加关键字 ensure 为 t 才可以，如

```elisp
(use-package evil
  :ensure t
  :config
  (evil-mode))
```

当然，如果需要默认每个包都自动安装，可以配置如下命令：

```elisp
(setq use-package-always-ensure t)
```

注意到 init 关键字也可以做一些配置，它与 config 关键字的区别在于，init关键字后面的配置在包还没有被载入时就已经生效，config 关键字后面的配置要等到包载入后才生效。

use-package 添加的包如果定义了 :commands, :bind, :bind\*, :bind-keymap, :bind-keymap\*, :mode, :interpreter, or :hook 等关键字，会默认延迟加载，这对于提高 emacs 的启动速度很有帮助。

也可以手动添加 :defer 关键字，设置为 t，表示延迟启动，设置为整数 N，表示延迟 N 秒启动。


## org to hugo {#org-to-hugo}


### 目标 {#目标}

使用 ox-hugo 导出 org mode 文件，并发布到 github page 上去。


### 安装 {#安装}

```elisp
(use-package ox-hugo
:pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
:after ox)
```
