clean: ## Clean the build files
	scala-cli --power clean .
	rm -rf .bsp
	rm -rf .metals

repl: ## Start a repl including the project
	scala-cli repl .

ide: ## Generate ide configuration files
	scala-cli setup-ide .

help: ## Show help message
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' -e 's/:.*#/: #/' | column -t -s '##'
