


all: haskell

sle: sle-wrapper sle-haskell 

.PHONY: clean sle-wrapper

configure:
	cd sle-wrapper/sle-wrapper 
	./waf configure --with-debug-log

configure-debug:
	cd sle-wrapper/sle-wrapper; ./waf configure --debug --with-debug-log

sle-wrapper: 
	echo "building C++ source..."
	cd sle-wrapper/sle-wrapper; ./waf 

sle-haskell: sle-wrapper
	stack --stack-yaml stack_sle.yaml build

haskell: 
	stack build

clean:
	echo "Cleaning sle-wrapper..."
	cd sle-wrapper/sle-wrapper; ./waf clean
	echo "Cleaning Haskell..."
	stack clean 




