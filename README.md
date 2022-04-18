yorick-libmisc
======

Miscellaneous yorick utils and code stash. Much of it needs a lot of work... Also installed -- in `[YPKG_PREFIX]/libexec/bin` -- is the script `yoo` which calls `yorick` through `rlwrap` and send time-stamped logs to `$HOME/.yorick/logs`. Add to path if you need command line editing in the yorick REPL (and you are not using the emacs interface.)

Installation
------------

In short, building and installing the package can be as quick as:
```sh
cd $BUILD_DIR
$SRC_DIR/configure
make install
```
where `$BUILD_DIR` is the build directory (at your convenience) and `$SRC_DIR`
is the source directory of the code.  The build and source directories
can be the same in which case, call `./configure` to configure for building.

More detailed installation explanations are given below.

1. You must have Yorick installed on your machine.

2. Unpack or clone the package repository somewhere.

3. Configure for compilation.  There are two possibilities:

   For an **in-place build**, go to the source directory of the   code
   and run the configuration script:
   ```sh
   cd SRC_DIR
   ./configure
   ```
   To see the configuration options, type:
   ```sh
   ./configure --help
   ```

   To compile in a **different build directory**, say `$BUILD_DIR`, create the
   build directory, go to the build directory, and run the configuration
   script:
   ```sh
   mkdir -p $BUILD_DIR
   cd $BUILD_DIR
   $SRC_DIR/configure
   ```
   where `$SRC_DIR` is the path to the source directory.
   To see the configuration options, type:
   ```sh
   $SRC_DIR/configure --help
   ```

4. Compile the code:
   ```sh
   make clean
   make
   ```

5. Install the package in Yorick directories:
   ```sh
   make install
   ```
