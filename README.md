[![Build Status](https://github.com/ShuguangSun/rutils/workflows/CI/badge.svg)](https://github.com/ShuguangSun/rutils/actions)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

# rutils

R utilities with transient.

## Installation

Clone this repository, or install from MELPA (TODO). Add the following to your `.emacs` to load all rutils:

``` elisp
(require 'rutils-site)
```


# R renv

R renv utilities: `rutils-renv'

| transient                | R command            |
|--------------------------|----------------------|
| rutils-renv-init         | renv::init()         |
| rutils-renv-snapshot     | renv::snapshot()     |
| rutils-renv-status       | renv::status()       |
| rutils-renv-restore      | renv::restore()      |
| rutils-renv-update       | renv::update()       |
| rutils-renv-hydrate      | renv::hydrate()      |
| rutils-renv-dependencies | renv::dependencies() |
| rutils-renv-diagnostics  | renv::diagnostics()  |

# R packrat

R packrat utilities: `rutils-packrat'

| transient                      | R command                  |
|--------------------------------|----------------------------|
| rutils-packrat-init            | packrat::init()            |
| rutils-packrat-snapshot        | packrat::snapshot()        |
| rutils-packrat-status          | packrat::status()          |
| rutils-packrat-restore         | packrat::restore()         |
| rutils-packrat-bundle          | packrat::bundle()          |
| rutils-packrat-unbundle        | packrat::unbundle()        |
| rutils-packrat-clean           | packrat:clean()            |
| rutils-packrat-disable         | packrat::disable()         |
| rutils-packrat-unused_packages | packrat::unused_packages() |
| rutils-packrat-get_opts        | packrat::get_opts()        |
