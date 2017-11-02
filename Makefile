build:
	stack build

watch:
	stack build --file-watch --fast

ghci:
	stack ghci orgcode:lib
