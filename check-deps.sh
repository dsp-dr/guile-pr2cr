#!/bin/sh
# FreeBSD dependency checker for PR2CR workflow

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m'

echo "${CYAN}Checking dependencies for FreeBSD...${NC}"

# Check OS
if [ "$(uname -s)" != "FreeBSD" ]; then
    echo "${RED}Warning: This script is optimized for FreeBSD${NC}"
fi

# Track if all deps are satisfied
DEPS_OK=1

# Function to check command
check_command() {
    cmd=$1
    pkg=$2

    if command -v "$cmd" >/dev/null 2>&1; then
        echo "${GREEN}✓${NC} $cmd is installed"
    else
        echo "${RED}✗${NC} $cmd is NOT installed"
        echo "  Install with: sudo pkg install $pkg"
        DEPS_OK=0
    fi
}

# Function to check Guile module
check_guile_module() {
    module=$1
    pkg=$2

    if guile3 -c "(use-modules $module)" 2>/dev/null; then
        echo "${GREEN}✓${NC} Guile module $module is available"
    else
        echo "${RED}✗${NC} Guile module $module is NOT available"
        echo "  Install with: sudo pkg install $pkg"
        DEPS_OK=0
    fi
}

echo ""
echo "Core dependencies:"
check_command "emacs" "emacs"
check_command "git" "git"
check_command "gh" "gh"
check_command "guile3" "guile3"
check_command "vale" "vale"
check_command "jq" "jq"
check_command "gmake" "gmake"

echo ""
echo "Guile modules:"
check_guile_module "(json)" "guile-json"
check_guile_module "(web client)" "guile3"
check_guile_module "(ice-9 getopt-long)" "guile3"

# Check Vale styles
echo ""
echo "Vale configuration:"
if [ -f ".vale-itil.ini" ]; then
    echo "${GREEN}✓${NC} Vale ITIL config found"
else
    echo "${RED}✗${NC} Vale ITIL config missing"
    echo "  Will be created from gist"
    DEPS_OK=0
fi

if [ -d "vale-styles/ITIL4" ]; then
    echo "${GREEN}✓${NC} Vale ITIL4 styles found"
else
    echo "${RED}✗${NC} Vale ITIL4 styles missing"
    echo "  Will be copied from gist"
fi

echo ""
if [ $DEPS_OK -eq 1 ]; then
    echo "${GREEN}All dependencies satisfied!${NC}"
    exit 0
else
    echo "${RED}Some dependencies are missing. Please install them.${NC}"
    echo ""
    echo "Quick install for FreeBSD:"
    echo "sudo pkg install emacs git gh guile3 vale jq gmake guile-json"
    exit 1
fi