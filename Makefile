.PHONY: all compile

all: compile docs/user-guide.md

compile:
	@stack build

docs/user-guide.md: app/Tutorial.lhs
	@./build-docs.sh
