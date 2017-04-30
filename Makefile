all:
	@echo try make clean

clean:
	find . -name '*.beam' -exec rm -f '{}' ';'
