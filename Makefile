


all: haskell

sle: sle-wrapper haskell 

configure:
	cd sle-wrapper/sle-wrapper 
	./waf configure --with-debug-log

configure-debug:
	cd sle-wrapper/sle-wrapper; ./waf configure --debug --with-debug-log

sle-wrapper:
	cd sle-wrapper/sle-wrapper; ./waf 

haskell: 
	stack build






