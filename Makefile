all:
	@echo try make clean

clean:
	find . -name '*.beam' -exec rm -f '{}' ';'
	find . -name 'ttb_last_config' -exec rm -f '{}' ';'
