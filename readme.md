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
  :load-path "~/path/to/dir/with/npm2"
  :config
  (add-hook 'js-mode 'npm2-mode))
```


# Features
- Generate package.json. `npm2-init-package`
  - [x] Create package.json
  - [x] Suggest current directory as default name
  - [x] Check if a package.json already exist
- [x] wrapper for npm run *script-name*. `npm2-run-script`
  - [x] Suggest scripts from package.json
- [x] Install npm packages. `npm2-install-package`
  - [ ] If in js-mode suggest packages from require("*package-name*").
- [ ] Install npm2.el from melpa.
- [ ] Package.json Major-mode.

# License
[GNU General Public License v3](./LICENSE)
