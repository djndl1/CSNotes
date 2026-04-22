.PHONY: html clean clean-all help

DIR ?= .

# Find all .org files under DIR using wildcard (handles spaces in filenames)
ORG_FILES := $(wildcard $(DIR)/*.org)
ORG_FILES += $(wildcard $(DIR)/*/*.org)
ORG_FILES += $(wildcard $(DIR)/*/*/*.org)
ORG_FILES += $(wildcard $(DIR)/*/*/*/*.org)

# Convert .org paths to .html paths
HTML_FILES := $(foreach f,$(ORG_FILES),$(subst .org,.html,$(f)))

# Default target - generate all HTML files
html: $(HTML_FILES)
	@echo "HTML generation complete"

# Pattern rule: generate .html from corresponding .org
%.html: %.org 
	@mkdir -p $(dir $@)
	@pandoc --css /Notes/CSNotes/sidebar.css --mathjax -s --toc -f org -t html -o "$@" "$<" || (echo "Warning: Failed to generate $@ (non-fatal)" && true)
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
