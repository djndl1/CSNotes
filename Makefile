.PHONY: html clean clean-all help

DIR ?= .

# Find all .org files under DIR
ORG_FILES := $(shell find $(DIR) -name "*.org" -type f 2>/dev/null)

# Convert .org paths to .html paths (using subst to preserve spaces in filenames)
HTML_FILES := $(foreach f,$(ORG_FILES),$(subst .org,.html,$(f)))

# Default target - generate all HTML files
html: $(HTML_FILES)
	@echo "HTML generation complete"

# Pattern rule: generate .html from corresponding .org
%.html: %.org
	@mkdir -p $(dir $@)
	@pandoc --mathjax -s --toc -f org -t html -o "$@" "$<" || (echo "Warning: Failed to generate $@ (non-fatal)" && true)
	@echo "Generated $@"

# Remove generated HTML files under DIR
clean:
	@rm -f $(HTML_FILES)
	@echo "Cleaned HTML files in $(DIR)"

# Remove all generated HTML files in the repository
clean-all:
	@find . -name "*.html" -type f -delete
	@echo "Cleaned all HTML files"

help:
	@echo "Usage:"
	@echo "  make html           Generate HTML for current directory"
	@echo "  make html DIR=path  Generate HTML for specified directory"
	@echo "  make clean          Remove HTML files in specified directory"
	@echo "  make clean-all     Remove all HTML files in repository"
	@echo ""
	@echo "Examples:"
	@echo "  make html DIR=os          # Generate HTML for os/ directory"
	@echo "  make html DIR=algorithm   # Generate HTML for algorithm/ directory"
