# Makefile for GitHub PR → ITIL Change Request Workflow (Guile Version)
# Generates validated change request documentation from pull requests

# Configuration
SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c

# Variables
GITHUB_ORG ?= $(shell git remote get-url origin | sed -E 's/.*[:/]([^/]+)\/[^/]+\.git/\1/')
GITHUB_REPO ?= $(shell basename -s .git `git remote get-url origin`)
PR_NUMBER ?= $(error PR_NUMBER is required)
VALE_STYLES_PATH ?= vale-styles
OUTPUT_DIR ?= change-requests
GUILE := guile3

# JIRA Configuration
JIRA_URL ?= $(error JIRA_URL is required, e.g., https://company.atlassian.net)
JIRA_PROJECT ?= $(error JIRA_PROJECT is required, e.g., OPS)

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
all: generate

# Tangle org files to generate scripts
.PHONY: tangle
tangle:
	@echo -e "$(CYAN)Tangling org files to generate Guile scripts...$(NC)"
	@emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"pr2cr-guile.org\")"
	@chmod +x scripts/*.scm
	@echo -e "$(GREEN)✓ Scripts generated$(NC)"

# Main workflow
.PHONY: generate
generate: tangle check-deps fetch-metadata extract-jira generate-summary finalize-change-request
	@echo -e "$(GREEN)✓ Change request generated successfully!$(NC)"
	@echo -e "$(CYAN)Output: $(OUTPUT_DIR)/CR-$(PR_NUMBER)-$(shell date +%Y%m%d).md$(NC)"

# Check dependencies
.PHONY: check-deps
check-deps:
	@echo -e "$(CYAN)Checking dependencies...$(NC)"
	@command -v gh >/dev/null 2>&1 || { echo -e "$(RED)Error: GitHub CLI (gh) is not installed$(NC)"; exit 1; }
	@command -v jq >/dev/null 2>&1 || { echo -e "$(RED)Error: jq is not installed$(NC)"; exit 1; }
	@command -v vale >/dev/null 2>&1 || { echo -e "$(RED)Error: Vale is not installed$(NC)"; exit 1; }
	@command -v $(GUILE) >/dev/null 2>&1 || { echo -e "$(RED)Error: Guile 3 is not installed$(NC)"; exit 1; }
	@echo -e "$(GREEN)✓ All dependencies satisfied$(NC)"

# Setup Guile load path
export GUILE_LOAD_PATH := $(PWD)/scripts:$(GUILE_LOAD_PATH)

# Fetch GitHub PR metadata
.PHONY: fetch-metadata
fetch-metadata:
	@echo -e "$(CYAN)Fetching PR #$(PR_NUMBER) metadata...$(NC)"
	@mkdir -p $(OUTPUT_DIR)/temp
	@gh pr view $(PR_NUMBER) \
		--repo $(GITHUB_ORG)/$(GITHUB_REPO) \
		--json number,title,body,author,createdAt,updatedAt,labels,files,commits,reviews,checks \
		> $(OUTPUT_DIR)/temp/pr-metadata.json
	@echo -e "$(GREEN)✓ PR metadata fetched$(NC)"

# Extract JIRA ticket from PR
.PHONY: extract-jira
extract-jira: $(OUTPUT_DIR)/temp/pr-metadata.json
	@echo -e "$(CYAN)Extracting JIRA ticket information...$(NC)"
	@$(GUILE) scripts/extract-jira.scm \
		--pr-metadata $(OUTPUT_DIR)/temp/pr-metadata.json \
		--jira-url $(JIRA_URL) \
		--output $(OUTPUT_DIR)/temp/jira-data.json
	@echo -e "$(GREEN)✓ JIRA data extracted$(NC)"

# Generate long-form summary
.PHONY: generate-summary
generate-summary: $(OUTPUT_DIR)/temp/pr-metadata.json $(OUTPUT_DIR)/temp/jira-data.json
	@echo -e "$(CYAN)Generating change request summary...$(NC)"
	@$(GUILE) scripts/generate-summary.scm \
		--pr-metadata $(OUTPUT_DIR)/temp/pr-metadata.json \
		--jira-data $(OUTPUT_DIR)/temp/jira-data.json \
		--output $(OUTPUT_DIR)/temp/change-request-draft.md \
		--start-time "$(CHANGE_START)" \
		--end-time "$(CHANGE_END)"
	@echo -e "$(GREEN)✓ Summary generated$(NC)"

# Finalize change request
.PHONY: finalize-change-request
finalize-change-request: $(OUTPUT_DIR)/temp/change-request-draft.md
	@echo -e "$(CYAN)Finalizing change request...$(NC)"
	@$(GUILE) scripts/finalize-change-request.scm \
		--draft $(OUTPUT_DIR)/temp/change-request-draft.md \
		--output $(OUTPUT_DIR)/CR-$(PR_NUMBER)-$(shell date +%Y%m%d).md \
		--pr-number $(PR_NUMBER)
	@echo -e "$(GREEN)✓ Change request finalized$(NC)"

# Install Vale styles if needed
.PHONY: install-vale-styles
install-vale-styles:
	@if [ ! -d "$(VALE_STYLES_PATH)/ITIL4" ]; then \
		echo -e "$(CYAN)Installing ITIL 4 Vale styles...$(NC)"; \
		mkdir -p $(VALE_STYLES_PATH); \
		cp -r a079757a1dc18b56c36fbf62d66d4582/vale-styles/ITIL4 $(VALE_STYLES_PATH)/ 2>/dev/null || true; \
		echo -e "$(GREEN)✓ Vale styles installed$(NC)"; \
	fi

# Publish to JIRA (optional)
.PHONY: publish
publish: $(OUTPUT_DIR)/CR-$(PR_NUMBER)-$(shell date +%Y%m%d).md
	@echo -e "$(CYAN)Publishing to JIRA...$(NC)"
	@$(GUILE) scripts/publish-to-jira.scm \
		--change-request $(OUTPUT_DIR)/CR-$(PR_NUMBER)-$(shell date +%Y%m%d).md \
		--jira-url $(JIRA_URL) \
		--jira-project $(JIRA_PROJECT)
	@echo -e "$(GREEN)✓ Published to JIRA$(NC)"

# Clean generated files
.PHONY: clean
clean:
	@echo -e "$(CYAN)Cleaning generated files...$(NC)"
	@rm -rf $(OUTPUT_DIR)/temp
	@rm -f scripts/*.scm
	@echo -e "$(GREEN)✓ Cleaned$(NC)"

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
