+++
title = "use-package"
author = ["flumine@qq.com"]
date = 2022-12-12T00:31:00+08:00
lastmod = 2022-12-12T00:31:44+08:00
tags = ["emacs"]
categories = ["技术"]
draft = false
+++

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
