.DEFAULT_GOAL := main

file = ../sudokus/sudoku1.txt
dir	 = $(shell realpath "$(file)")

main: ## Run the Sudoku solver
	runhaskell Main.hs "$(dir)"

install: ## Install dependencies
	@sudo apt-get install haskell-platform

view: ## display the Makefile
	@cat Makefile

edit: ## open the Makefile with visual studio code
	@code Makefile

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
