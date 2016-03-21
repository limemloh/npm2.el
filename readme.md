# npm2.el
Emacs package with tools for working with [npm - Node Package Manager](https://www.npmjs.com/).

# Installation
So far npm2 is only to be found here on GitHub.
```elisp
(add-to-list 'load-path "~/path/to/dir/with/npm2")
(require 'npm2)
```


If you want to use use-package:
```elisp
(use-package npm2
  :load-path "~/path/to/dir/with/npm2")
```


# Features
- [ ] Generate package.json. `npm2-init-package`
  - [x] Create package.json
  - [x] Suggest current directory as default name
  - [ ] Check if a package.json already exist
- [ ] Package.json Major-mode.
- [ ] wrapper for npm run *script-name*.
- [ ] Install npm packages.
- [ ] Install from require("*package-name*").
- [ ] Install npm2.el from melpa.

# License
[GNU General Public License v3](./LICENSE)
