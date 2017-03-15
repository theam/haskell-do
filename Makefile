build-all-windows: build-front build-back-windows
build-all-osx: build-front build-back-osx
build-all-linux: build-front build-back-linux

build-front:
	cd gui &&\
		npm run build &&\
		cd ..

build-back-linux:
	cd core &&\
		stack build &&\
		cp `stack path --local-install-root`/bin/haskelldo-core ../gui/dist/bin/haskelldo-core-linux
		cd ..

build-back-osx:
	cd core &&\
		stack build &&\
		cp `stack path --local-install-root`/bin/haskelldo-core ../gui/dist/bin/haskelldo-core-darwin
		cd ..

build-back-windows:
	cd core &&\
		stack build &&\
		cp "`stack path --local-install-root`\bin\haskelldo-core.exe" ../gui/dist/bin/haskelldo-core-w64.exe&&\
		cd ..

deps:
	cd gui &&\
		npm install &&\
		bower install &&\
		cd .. &&\
		cd core &&\
		stack setup

run:
	cd gui &&\
		npm run start
