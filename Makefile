build-all:
	stack build
	npm run build

build-front:
	npm run build

build-back:
	stack build

deps:
	cd veilig-gui &&\
		npm install &&\
		bower install
