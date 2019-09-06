.PHONY: docker-build

docker-build:
	cd docker; \
	docker build -t esa-space-proto-builder --build-arg UID=$(shell id -u) --build-arg GID=$(shell id -g) .

docker-run:
	docker run --rm -it \
		-v $(shell pwd):/home/dev \
		--user=dev \
		-w=/home/dev \
		esa-space-proto-builder \
		/bin/bash

