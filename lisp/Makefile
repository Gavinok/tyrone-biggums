LISP ?= sbcl
project = lisp

all: $(project)

deps: ## Install quicklisp dependancies
	qlot install

fast: ## For faster compilation and startup at the cost of binary size
	$(MAKE) args=--disable-compression

clean:
	$(RM) roswell/lisp

$(project): deps ## Default build format
	qlot exec ros build roswell/$(project).ros $(args)
	mv roswell/$(project) $(project)
