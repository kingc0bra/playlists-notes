EMACS := emacs
EMACS_OPT_PRE := --eval '(package-initialize)' -l playlists.el --batch
EMACS_OPT_POST := --eval '(org-publish "playlists")'
SOURCES := $(wildcard org/*.org)

.PHONY: build clean help
.DEFAULT_GOAL := help

build: $(SOURCES)  ## build static HTML site
	$(EMACS) $(EMACS_OPT_PRE) $(EMACS_OPT_POST)

clean:  ## clean up all build artifacts
	@rm -rf public_html
	@rm -f ~/.org-timestamps/playlists*.cache

help:  ## print usage information
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)
