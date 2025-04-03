cl-cookieweb
============

A [cookiecutter](https://github.com/audreyr/cookiecutter) web template for Common Lisp.

Provides a working toy web app with the Hunchentoot web server, easy-routes, Djula templates, styled with Bulma, based on SQLite, with migrations and an example table definition.

Featuring:

- Recipe to run the project from sources or to build a binary, choosing your Lisp implementation
- Test suite using [Fiveam](https://common-lisp.net/project/fiveam/docs/)
- DB:
  - creation of a DB (SQLite by default. That is easily configurable)
  - a simple models.lisp with a Product table definition with the Mito ORM (see [Cookbook/databases#the Mito ORM](https://lispcookbook.github.io/cl-cookbook/databases.html#the-mito-orm-and-sxql))
  - migrations (built-in Mito feature)
- Djula templates
  - showing inheritance and custom filters
  - styled with [Bulma](https://github.com/jgthms/bulma)
  - showing a default dashboard
- JavaScript:
  - load static assets with a dummy JS file.
- run the project:
  - from sources
    - CLI args parsing (port, help)
    - port is also taken from an environment variable
  - build a self-contained binary
    - (xxx: static assets not included?)
    - with Deploy: ship shared libraries, run on another machine (rely on libssl of the host system). You then have to ship the `bin/` folder.
  - Roswell integration to run the app as a script and to build, install and share binaries.
- Systemd example

Based on [cl-cookieproject](https://github.com/vindarel/cl-cookieproject): a ready-to-use project template.

Might eventually appear:

- access roles
- login and logout
- JSON API example
- Vue.js integration
- HTMX example
- ~~ISSR example~~ [view here](https://github.com/vindarel/ISSR-productlist/)
- ~~SystemD example~~ OK

![](cookieweb.png)

Usage
-----

Create a new project. You'll be prompted to provide some basic
information about your new project, which will then be auto-generated
in the current working directory:

```bash
$ pip install cookiecutter
$ cookiecutter https://github.com/vindarel/cl-cookieweb
project_name [cookie-web-project]:
repo_name [cookie-web-project]:
description []: A test project
version [0.0.1]:
year [1984]:
author [CL User]: me
email [me@mail.com]:
username [mme]:
branch [master]:
remote [origin]:
Initialised empty Git repository in /home/vince/bacasable/lisp-projects/cookie-web-project/.git/
Switched to a new branch 'master'
```

Run it straight away:

```bash
$ cd cookie-web-project
$ make run
rlwrap sbcl --load run.lisp
This is SBCL 2.0.10, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
To load "cffi":
  Load 1 ASDF system:
    cffi
; Loading "cffi"
.
To load "cookie-web-project":
  Load 1 ASDF system:
    cookie-web-project
; Loading "cookie-web-project"
cookie-web-project version 0.0.1
Application started on port 4242.
*
```

You can see the `Hello` from the `main` function, and we are given a Lisp REPL.

Build an executable:

```bash
$ make build
[…]
; Loading "cookie-web-project"
..................................................
[package cookie-web-project/utils]...............
[package cookie-web-project/models]..............
[package cookie-web-project/web].................
[package cookie-web-project]...

#P"/home/vindarel/projects/cl-cookieweb/cookie-web-project/bin/cookie-web-project"
 ==> Running load hooks.
 ==> Gathering system information.
   -> Will load the following foreign libs on boot:
      (#<DEPLOY:LIBRARY LIBSSL>)
 ==> Deploying files to /home/vindarel/projects/cl-cookieweb/cookie-web-project/bin/
   -> Copying library #<LIBRARY LIBZ>
 ==> Running build hooks.
   -> Closing foreign library #<LIBRARY LIBSSL>.
 ==> Dumping image to /home/vindarel/projects/cl-cookieweb/cookie-web-project/bin/cookie-web-project
[undoing binding stack and other enclosing state... done]
[defragmenting immobile space... 2002+33897+3559+3148+28073+31776 objects... done]
[saving current Lisp image into /home/vindarel/projects/cl-cookieweb/cookie-web-project/bin/cookie-web-project:
writing 0 bytes from the read-only space at 0x20000000
compressed 0 bytes into 8 at level -1
writing 1648 bytes from the static space at 0x20100000
compressed 32768 bytes into 582 at level -1
writing 95027200 bytes from the dynamic space at 0x1000000000
compressed 95027200 bytes into 18359087 at level -1
writing 3043328 bytes from the immobile space at 0x20300000
compressed 3047424 bytes into 795041 at level -1
writing 22947392 bytes from the immobile space at 0x21b00000
compressed 22970368 bytes into 6758085 at level -1
done]
```

A `bin/` directory is created with shared .so libraries:

```
$ ls bin
cookie-web-project  libz.so
```
(and when required, you'll find libmagic.so, libosicat.so etc)

Run the binary from the `bin/` folder. We can see that Deploy handles
loading those shared libraries.

```
$ ./bin/cookie-web-project
 ==> Performing warm boot.
   -> Runtime directory is /home/vince/projets/cl-cookieweb/cookie-web-project/bin/
   -> Resource directory is /home/vince/projets/cl-cookieweb/cookie-web-project/bin/
 ==> Running boot hooks.
 ==> Reloading foreign libraries.
   -> Loading foreign library #<LIBRARY LIBSSL>.
 ==> Launching application.
 […]
cookie-web-project version 0.0.1
Loading config file config.lisp…
Skipping config file.Starting Hunchentoot on port 4545…
Application started on port 4545.
```

You can access your app on http://localhost:4545

### Systemd

Create a new file in `/etc/systemd/system/webcookie.service`:

```
[Unit]
Description=Lisp web project skeleton

[Service]
WorkingDirectory=/home/path/to/cookie-web-project/  # <- directory
ExecStart=/home/path/to/cookie-web-project/bin/cookie-web-project  # <- binary
Type=simple
Restart=on-failure
[Install]
WantedBy=network.target
```

<!-- WorkingDirectory=/home/vince/projets/cl-cookieweb/cookie-lisp-project/ -->
<!-- ExecStart=/home/vince/projets/cl-cookieweb/cookie-lisp-project/bin/cookie-lisp-project -->

Commands:

    sudo systemctl start webcookie.service

and also `stop` and `status`.

To see logs:

    journalctl -u webcookie.service

To start the app on (re)boot:

    sudo systemctl enable webcookie.service

If you want to run the app from sources, you can use the command
`/usr/bin/make run`, but tweak `run.lisp` so that it calls `(run)`
instead of `(main)`, so that the server thread is placed on the
foreground. Otherwise, the Lisp process quits instantly and your
process is not started.


## cookiecutter options

You can use command line options: https://cookiecutter.readthedocs.io/en/1.7.2/advanced/cli_options.html

- `--no-input`: do not prompt for parameters and only use cookiecutter.json file content
- ` --replay`: do not prompt for parameters and only use information entered previously
- ` -f, --overwrite-if-exists`: overwrite the contents of the output directory if it already exists
- ` -s, --skip-if-file-exists`: skip the files in the corresponding directories if they already exist
- ` -o, --output-dir`: where to output the generated project dir into
- ` --config-file`: user configuration file

# See also

Other demos and skeletons:

- [Feather](https://hg.sr.ht/~wnortje/feather), a template for web
  application development, shows a functioning Hello World app
  with an HTML page, a JSON API, a passing test suite, a Postgres DB
  and DB migrations. Uses Qlot, Buildapp, SystemD for deployment.
  - => probably a bit more serious as of yet.
- [lisp-web-template-productlist](https://github.com/vindarel/lisp-web-template-productlist),
  a simpler example (no DB models and it is not a project template, only a working example).
- [lisp-web-live-reload-example](https://github.com/vindarel/lisp-web-live-reload-example/) -
  a toy project to show how to interact with a running web app.

Resources:

- The CL Cookbook: https://lispcookbook.github.io/cl-cookbook/web.html
- libraries: https://github.com/CodyReichert/awesome-cl

Apps:

- [ABStock](https://github.com/vindarel/ABStock) - a products catalogue, made for bookshops
- [OpenBookStore](https://github.com/OpenBookStore/openbookstore) (WIP) - books management software. With user access roles, user login, a Vue.js page, API endpoints…

# CHANGELOG

- 2022/06: build with Deploy (handle shared libraries). Make ASDF not crash when running the binary on a system with an old configuration (it's a self-contained binary dude!).
  - <2025-04-03> this was because we didn't reference the DB module in the .asd. Added `:dbd-sqlite3`, see .asd for the other drivers.
  - added Systemd service example.
- 2021/12: load a `config.lisp` init file before web startup. Same could be done with a post-init file.
- init.

---

WTFPL

https://github.com/sponsors/vindarel/

<a href='https://ko-fi.com/K3K828W0V' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://cdn.ko-fi.com/cdn/kofi2.png?v=2' border='0' alt='buy me a coffee!' title='Is that useful and do you want to see more? Buy Me a Coffee, it is useful, no big income here!' /></a>
