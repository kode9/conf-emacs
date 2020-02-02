BUILD_ARGS ?=

REPOSITORY	:= abz
COMPONENT		:= emacs-test

NAME		:= ${REPOSITORY}/${COMPONENT}
IMAGE		:= ${NAME}:latest

all:

test: Dockerfile .dockerignore test.el init.el $(wildcard abz/*.el)
	docker build \
		${BUILD_ARGS} \
		-t "${IMAGE}" .
	docker run --rm "${IMAGE}"

.PHONY: all test
