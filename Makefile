.PHONY: build install clean remote input

build:
	jbuilder build --dev

install: build
	jbuilder install

clean:
	jbuilder clean

remote:
	jbuilder exec $@

input:
	jbuilder exec $@
