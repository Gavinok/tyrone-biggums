LISP ?= sbcl
project = lisp

all: build

deps: ## Install quicklisp dependancies
	qlot install

fast: ## For faster compilation and startup at the cost of binary size
	$(MAKE) args=--disable-compression

build: deps ## Default build format
	qlot exec ros build $(args) $(project).ros
