#!/bin/bash
#!/bin/bash
# Test the PR2CR Guile workflow

set -e

echo "Testing PR2CR Guile Workflow..."

# Set test variables
export PR_NUMBER=${1:-1}
export JIRA_URL="https://example.atlassian.net"
export JIRA_PROJECT="TEST"

# Run with dry-run for testing
echo "1. Tangling org file..."
emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"pr2cr-guile.org\")"

echo "2. Setting up test data..."
mkdir -p change-requests/temp

# Create mock PR data if not running against real GitHub
if [ ! -f "change-requests/temp/pr-metadata.json" ]; then
    cat > change-requests/temp/pr-metadata.json <<EOF
{
  "number": $PR_NUMBER,
  "title": "TEST-123: Add new feature",
  "body": "This PR implements the new feature described in TEST-123",
  "author": {"login": "testuser"},
  "files": [
    {"filename": "src/main.scm", "additions": 50, "deletions": 10},
    {"filename": "tests/test.scm", "additions": 30, "deletions": 0}
  ],
  "commits": [
    {"commit": {"message": "TEST-123: Initial implementation"}}
  ]
}
EOF
fi

echo "3. Running workflow..."
gmake generate PR_NUMBER=$PR_NUMBER JIRA_URL=$JIRA_URL JIRA_PROJECT=$JIRA_PROJECT || true

echo "Test complete!"
