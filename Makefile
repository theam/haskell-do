build-all:
	cd core &&\
		stack build &&\
		cd ../gui &&\
		npm run build &&\
		cd ..

build-front:
	cd gui &&\
		npm run build &&\
		cd ..

build-back:
	cd core &&\
		stack build &&\
		cd ..

deps:
	cd gui &&\
		npm install &&\
		bower install &&\
		cd ..
