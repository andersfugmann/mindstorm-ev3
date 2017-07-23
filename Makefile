.DEFAULT: remote
.PHONY: build install remote clean

build:
	jbuilder build

install: build
	jbuilder install

remote:
	jbuilder build src/remote.exe

clean:
	jbuilder clean
