# Copyright (C) 2023  CToID

# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

all: setup

setup: setup.lisp
	sbcl --disable-debugger --no-userinit \
	--load ~/quicklisp/setup.lisp \
	--load setup.lisp --eval \
	"(sb-ext:save-lisp-and-die \"setup\" \
	:toplevel #'main :executable t :compression t)"
