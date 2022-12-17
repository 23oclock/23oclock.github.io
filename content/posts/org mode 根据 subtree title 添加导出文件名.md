+++
title = "org mode 根据 subtree title 添加导出文件名"
author = ["flumine@qq.com"]
date = 2022-12-18T00:10:00+08:00
lastmod = 2022-12-18T00:10:25+08:00
tags = ["emacs"]
categories = ["技术"]
draft = false
+++

当 org mode 添加好 subtree 后，如果想要导出这个 subtree, 那么就需要给这个 subtree 添加一个 EXPORT-FILE-NAME 的属性。但是手动输入比较麻烦，如果能自动根据 subtree 的标题添加这个导出文件名属性就好了。

可以用下面这个函数自动添加导出文件名属性：

```elisp
(defun org-hugo-add-properties-with-subtree-title ()
  (interactive)
  ;; Jump to the parent header if not already on a header
  (when (not (org-at-heading-p))
    (org-previous-visible-heading 1))
  (org-set-property "EXPORT_FILE_NAME" (plist-get (-second-item (org-element-context)) :title)))
```

当然，按照函数中给出的方法，可以给自动添加很多与 subtree 相关的属性，具体细节可以参考 org-element-context 函数的输出结果。
