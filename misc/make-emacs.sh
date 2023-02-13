#!/bin/sh

sudo xbps-install gtk+3-devel libmagick-devel webkit2gtk-devel libgccjit-devel libXpm-devel gnutls-devel jansson-devel tree-sitter-devel giflib-devel ncurses-devel libwebp-devel librsvg-devel tree-sitter

./autogen.sh
./configure --with-x-toolkit=yes --with-imagemagick --with-json --with-tree-sitter --with-xwidgets --with-native-compilation=yes --prefix=/usr
make -j$(nproc)

sudo rm /bin/emacs
sudo ln -rs src/emacs /bin/emacs

sudo xbps-remove -Ro gtk+3-devel libmagick-devel webkit2gtk-devel libgccjit-devel libXpm-devel gnutls-devel jansson-devel tree-sitter-devel giflib-devel ncurses-devel libwebp-devel librsvg-devel
