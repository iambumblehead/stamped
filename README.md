stamped
=======
**(c)[Bumblehead][0], 2015** [MIT-license](#license)

Stamped gives Emacs the ability maintain stamps at the top of certain files each time one is saved.

```javascript
// Filename: my_app.js
// Timestamp: 2015.01.13-12:10:19 (last modified)
// Copyright: mycopy
// Author(s): me <me@domain.com>
```

```css
/* Filename: my_app.less
 * Timestamp: 2015.01.13-12:10:19 (last modified)
 * Copyright: mycopy
 * Author(s): me <me@domain.com>
 */
```

'stamped' comes with unit-tested javascript functions to read and write stamps. In the past these stamps were used by [scroungejs'][1] build engine.


[0]: http://www.bumblehead.com                            "bumblehead"
[1]: http://github.com/iambumblehead/scroungejs           "scroungejs"

---------------------------------------------------------
## Get Started

Stamped uses [f.el][4] which may be installed through [MELPA][5].

*.emacs*
```elisp
(load-file "~/path/to/stamped/stamped.el")
```

To begin auto-stamping .js, .css and .less files, define a config in your .emacs file. The stamp is created or updated when saving files with those extensions.

Disable and re-enable the stamp with `stamped-disable` and `stamped-enable`.

```elisp
;; neither value is required
(stamped-set-opts
 '((st-author . "bumblehead <chris@bumblehead.com>")
   (st-copy   . "Ganimas LLC")))
```

Define configurations for files in different locations with `stamped-proj-set-opts`. Use `stamped-proj-set-opts` multiple times for multiple configurations.

```elisp 
(stamped-proj-set-opts
 "myproject"
 '((root-dir  . "~/path/to/myproject")
   (log-dir  . "~/path/to/myproject/error.log") 
   (st-author . "bumblehead <chris@bumblehead.com>")
   (st-copy   . "Ganimas LLC")))
```

Stamps are only applied to one 'active' project at a time. By default the last project defined becomes the 'active' project. Set the active project name with `stamped-proj-set-name`.

```elisp
(stamped-proj-set-name "myproject")
```

Convenience functions are available for active projects:

 * `M-x stamped-proj-tail-log`, open a buffer tailing the log file
 * `M-x stamped-proj-node-start`, call `npm start` at the root-dir in a new buffer
 * `M-x stamped-proj-node-test`, call `npm test` at the root-dir in a new buffer


Key-commands for these may are created with something like the following added to *.emacs*:

```elisp
(global-set-key (kbd "C-c o") (lambda() (interactive) (stamped-proj-node-start)))
```

[4]: https://github.com/rejeep/f.el                        "f.el"
[5]: https://malpa.org                                    "MELPA"


---------------------------------------------------------
#### <a id="license">License:

 ![scrounge](https://github.com/iambumblehead/scroungejs/raw/master/img/hand.png) 

(The MIT License)

Copyright (c) 2015 [Bumblehead][0] <chris@bumblehead.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
