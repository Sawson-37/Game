This repository is for me to experiment with diagrams2.0 + sdl2 + cairo.

# How to Install

## Windows

Check the Internet Connection.

### Installation of stack

Install from the following URL.

<https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows>

### Installation of MSYS2

Install from the following URL.

<http://msys2.github.io/>

※x86_64 for 64bit, i686 for 32bit

Execute following command from MSYS2 Shell:

~~~
$ pacman -Syu
...

警告: terminate MSYS2 without returning to shell and check for updates again
警告: for example close your terminal window instead of calling exit
(完了)
~~~

It will not return to prompt. So close the window manually.

Re open MSYS2 Shell and execute `pacman -Syu` again.

### Installation of pkg-config

Execute following command to install pkg-config from MSYS2 Shell:

(For 64bit OS)

~~~
$ pacman -S --force mingw-w64-x86_64-pkg-config
~~~

(For 32bit OS)

~~~
$ pacman -S --force mingw-w64-i686-pkg-config
~~~

### Installation of C libraries

#### Installation of haskell-gi related libraries

<https://github.com/haskell-gi/haskell-gi/wiki/Using-haskell-gi-in-Windows>

#### Installation of SDL

Execute following command from MSYS2 Shell to install SDL:

(For 64bit OS)

~~~
$ pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2
~~~

### Installation of Git for Windows

Install Git for Windows from following URL.

<https://git-for-windows.github.io/>
"# Game" 
