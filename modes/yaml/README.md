*Author:* Noah Peart <noah.v.peart@gmail.com><br>
*URL:* [https://github.com/nverno/yaml-tools](https://github.com/nverno/yaml-tools)<br>

[![Build Status](https://travis-ci.org/nverno/yaml-tools.svg?branch=master)](https://travis-ci.org/nverno/yaml-tools)

Better indentation for `yaml-mode`.

To use, set `indent-line-function` and `indent-region-function`
to be `yaml-indent-indent-line` and `yaml-indent-indent-region`
respectively in `yaml-mode` hook, eg

```lisp
(defun my-yaml-hook ()
  (setq-local indent-line-function 'yaml-indent-indent-line)
  (setq-local indent-region-function 'yaml-indent-indent-region))
(add-hook 'yaml-mode-hook 'my-yaml-hook)
```


---
Converted from `yaml-tools.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
