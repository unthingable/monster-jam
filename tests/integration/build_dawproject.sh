#!/usr/bin/env bash
# Build test-project.dawproject from the editable XML sources.
# Run this after editing dawproject/project.xml or metadata.xml.
set -euo pipefail
cd "$(dirname "$0")/dawproject"
zip -r ../test-project.dawproject project.xml metadata.xml
echo "Built test-project.dawproject"
