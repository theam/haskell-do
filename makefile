MACHINE_NAME=dev

docker-init:
	docker-machine start ${MACHINE_NAME}
	eval $(docker-machine env ${MACHINE_NAME})

docker:
	docker build -t theam/haskell-do src/docker/
