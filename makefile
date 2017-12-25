MACHINE_NAME=dev
INIT_COMMAND=docker-machine env ${MACHINE_NAME}

IMAGE_NAME=theam/haskell-do

docker-init:
	docker-machine start ${MACHINE_NAME}
	eval ${INIT_COMMAND}

docker:
	docker build -t ${IMAGE_NAME} src/docker/

docker-run:
	docker run -n -p 80:80 -d ${IMAGE_NAME}
