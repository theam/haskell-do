IMAGE_NAME:=theam/haskell-do

docker:
	docker build -t ${IMAGE_NAME} src/docker/

docker-run:
	docker run -p 8080:8080 -d ${IMAGE_NAME}
