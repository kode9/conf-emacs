BUILD_ARGS ?=

REPOSITORY	:= abz
COMPONENT		:= dotemacs

NAME		:= ${REPOSITORY}/${COMPONENT}
IMAGE		:= ${NAME}:latest

all: test

build: Dockerfile .dockerignore test.el early-init.el init.el $(wildcard abz/*.el) $(wildcard vendor/*.el)
	DOCKER_BUILDKIT=1 docker build \
		--pull \
		--label build.date="$(shell date -u -Iseconds)" \
		${BUILD_ARGS} \
		-t "${IMAGE}" .

test: build
	docker run --rm "${IMAGE}"

.PHONY: all test
