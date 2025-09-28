# Makefile for GitHub PR → ITIL Change Request Workflow (Guile Version)
# Generates validated change request documentation from pull requests

# Configuration
SHELL := /bin/sh
.SHELLFLAGS := -eu -c

# Variables
GITHUB_ORG ?= $(shell git remote get-url origin | sed -E 's/.*[:/]([^/]+)\/[^/]+\.git/\1/')
GITHUB_REPO ?= $(shell basename -s .git `git remote get-url origin`)
VALE_STYLES_PATH ?= vale-styles
OUTPUT_DIR ?= change-requests
GUILE := guile3

# These will be checked when needed, not immediately
PR_NUMBER ?=
JIRA_URL ?=
JIRA_PROJECT ?=

# Timestamps (can be overridden)
CHANGE_START ?= $(shell date -u -d "+2 days" '+%Y-%m-%d %H:00:00 UTC')
CHANGE_END ?= $(shell date -u -d "+2 days 2 hours" '+%Y-%m-%d %H:00:00 UTC')

# Colors for output
CYAN := \033[0;36m
GREEN := \033[0;32m
RED := \033[0;31m
NC := \033[0m # No Color

# Default target
.PHONY: all
all: deps

# Install dependencies
.PHONY: deps
deps:
	@printf "$(CYAN)Checking and installing dependencies for FreeBSD...$(NC)\n"
	@printf "$(CYAN)Checking for required commands...$(NC)\n"
	@command -v emacs >/dev/null 2>&1 || { printf "$(RED)Error: emacs is not installed. Run: sudo pkg install emacs$(NC)\n"; exit 1; }
	@command -v git >/dev/null 2>&1 || { printf "$(RED)Error: git is not installed. Run: sudo pkg install git$(NC)\n"; exit 1; }
	@command -v gh >/dev/null 2>&1 || { printf "$(RED)Error: gh is not installed. Run: sudo pkg install gh$(NC)\n"; exit 1; }
	@command -v $(GUILE) >/dev/null 2>&1 || { printf "$(RED)Error: guile3 is not installed. Run: sudo pkg install guile3$(NC)\n"; exit 1; }
	@command -v vale >/dev/null 2>&1 || { printf "$(RED)Error: vale is not installed. Run: sudo pkg install vale$(NC)\n"; exit 1; }
	@command -v jq >/dev/null 2>&1 || { printf "$(RED)Error: jq is not installed. Run: sudo pkg install jq$(NC)\n"; exit 1; }
	@command -v gmake >/dev/null 2>&1 || { printf "$(RED)Error: gmake is not installed. Run: sudo pkg install gmake$(NC)\n"; exit 1; }
	@printf "$(GREEN)✓ All dependencies satisfied$(NC)\n"
	@printf "$(CYAN)Run './check-deps.sh' for detailed dependency verification$(NC)\n"

# Tangle org files to generate scripts
.PHONY: tangle
tangle:
	@printf "$(CYAN)Tangling org files to generate Guile scripts...$(NC)\n"
	@emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"pr2cr-guile.org\")"
	@chmod +x scripts/*.scm
	@printf "$(GREEN)✓ Scripts generated$(NC)\n"

# Main workflow
.PHONY: generate
generate: check-vars tangle check-deps fetch-metadata extract-jira generate-summary finalize-change-request
	@printf "$(GREEN)✓ Change request generated successfully!$(NC)\n"
	@printf "$(CYAN)Output: $(OUTPUT_DIR)/CR-$(PR_NUMBER)-$(shell date +%Y%m%d).md$(NC)\n"

# Check required variables
.PHONY: check-vars
check-vars:
	@test -n "$(PR_NUMBER)" || { printf "$(RED)Error: PR_NUMBER is required$(NC)\n"; exit 1; }
	@test -n "$(JIRA_URL)" || { printf "$(RED)Error: JIRA_URL is required (e.g., https://company.atlassian.net)$(NC)\n"; exit 1; }
	@test -n "$(JIRA_PROJECT)" || { printf "$(RED)Error: JIRA_PROJECT is required (e.g., OPS)$(NC)\n"; exit 1; }

# Check dependencies
.PHONY: check-deps
check-deps:
	@printf "$(CYAN)Checking dependencies...$(NC)\n"
	@command -v gh >/dev/null 2>&1 || { printf "$(RED)Error: GitHub CLI (gh) is not installed$(NC)\n"; exit 1; }
	@command -v jq >/dev/null 2>&1 || { printf "$(RED)Error: jq is not installed$(NC)\n"; exit 1; }
	@command -v vale >/dev/null 2>&1 || { printf "$(RED)Error: Vale is not installed$(NC)\n"; exit 1; }
	@command -v $(GUILE) >/dev/null 2>&1 || { printf "$(RED)Error: Guile 3 is not installed$(NC)\n"; exit 1; }
	@printf "$(GREEN)✓ All dependencies satisfied$(NC)\n"

# Setup Guile load path
export GUILE_LOAD_PATH := $(PWD)/scripts:$(GUILE_LOAD_PATH)

# Fetch GitHub PR metadata
.PHONY: fetch-metadata
fetch-metadata:
	@printf "$(CYAN)Fetching PR #$(PR_NUMBER) metadata...$(NC)\n"
	@mkdir -p $(OUTPUT_DIR)/temp
	@gh pr view $(PR_NUMBER) \
		--repo $(GITHUB_ORG)/$(GITHUB_REPO) \
		--json number,title,body,author,createdAt,updatedAt,labels,files,commits,reviews,checks \
		> $(OUTPUT_DIR)/temp/pr-metadata.json
	@printf "$(GREEN)✓ PR metadata fetched$(NC)\n"

# Extract JIRA ticket from PR
.PHONY: extract-jira
extract-jira: $(OUTPUT_DIR)/temp/pr-metadata.json
	@printf "$(CYAN)Extracting JIRA ticket information...$(NC)\n"
	@$(GUILE) scripts/extract-jira.scm \
		--pr-metadata $(OUTPUT_DIR)/temp/pr-metadata.json \
		--jira-url $(JIRA_URL) \
		--output $(OUTPUT_DIR)/temp/jira-data.json
	@printf "$(GREEN)✓ JIRA data extracted$(NC)\n"

# Generate long-form summary
.PHONY: generate-summary
generate-summary: $(OUTPUT_DIR)/temp/pr-metadata.json $(OUTPUT_DIR)/temp/jira-data.json
	@printf "$(CYAN)Generating change request summary...$(NC)\n"
	@$(GUILE) scripts/generate-summary.scm \
		--pr-metadata $(OUTPUT_DIR)/temp/pr-metadata.json \
		--jira-data $(OUTPUT_DIR)/temp/jira-data.json \
		--output $(OUTPUT_DIR)/temp/change-request-draft.md \
		--start-time "$(CHANGE_START)" \
		--end-time "$(CHANGE_END)"
	@printf "$(GREEN)✓ Summary generated$(NC)\n"

# Finalize change request
.PHONY: finalize-change-request
finalize-change-request: $(OUTPUT_DIR)/temp/change-request-draft.md
	@printf "$(CYAN)Finalizing change request...$(NC)\n"
	@$(GUILE) scripts/finalize-change-request.scm \
		--draft $(OUTPUT_DIR)/temp/change-request-draft.md \
		--output $(OUTPUT_DIR)/CR-$(PR_NUMBER)-$(shell date +%Y%m%d).md \
		--pr-number $(PR_NUMBER)
	@printf "$(GREEN)✓ Change request finalized$(NC)\n"

# Install Vale styles if needed
.PHONY: install-vale-styles
install-vale-styles:
	@if [ ! -d "$(VALE_STYLES_PATH)/ITIL4" ]; then \
		printf "$(CYAN)Installing ITIL 4 Vale styles...$(NC)\n"; \
		mkdir -p $(VALE_STYLES_PATH); \
		cp -r a079757a1dc18b56c36fbf62d66d4582/vale-styles/ITIL4 $(VALE_STYLES_PATH)/ 2>/dev/null || true; \
		printf "$(GREEN)✓ Vale styles installed$(NC)\n"; \
	fi

# Publish to JIRA (optional)
.PHONY: publish
publish: check-vars $(OUTPUT_DIR)/CR-$(PR_NUMBER)-$(shell date +%Y%m%d).md
	@printf "$(CYAN)Publishing to JIRA...$(NC)\n"
	@$(GUILE) scripts/publish-to-jira.scm \
		--change-request $(OUTPUT_DIR)/CR-$(PR_NUMBER)-$(shell date +%Y%m%d).md \
		--jira-url $(JIRA_URL) \
		--jira-project $(JIRA_PROJECT)
	@printf "$(GREEN)✓ Published to JIRA$(NC)\n"

# Clean generated files
.PHONY: clean
clean:
	@printf "$(CYAN)Cleaning generated files...$(NC)\n"
	@rm -rf $(OUTPUT_DIR)/temp
	@rm -f scripts/*.scm
	@printf "$(GREEN)✓ Cleaned$(NC)\n"

# Help
.PHONY: help
help:
	@echo "PR to Change Request Workflow (Guile Version)"
	@echo ""
	@echo "Usage:"
	@echo "  gmake generate PR_NUMBER=123 JIRA_URL=https://company.atlassian.net JIRA_PROJECT=OPS"
	@echo ""
	@echo "Targets:"
	@echo "  generate  - Generate change request from PR"
	@echo "  publish   - Publish change request to JIRA"
	@echo "  clean     - Remove generated files"
	@echo "  help      - Show this help"
	@echo ""
	@echo "Required variables:"
	@echo "  PR_NUMBER    - GitHub PR number"
	@echo "  JIRA_URL     - JIRA instance URL"
	@echo "  JIRA_PROJECT - JIRA project key"
