#!/bin/bash
VERSION=$1
if [ -z "$VERSION" ]; then
  echo "Usage: $0 version"
  exit 1
fi

TAG="v$VERSION"

# 1. Update .egg file version
sed -i "s/(version \".*\")/(version \"$VERSION\")/" srfi-170.egg

# 2. Add version to .release-info (before the last closing parenthesis)
# This assumes a specific format; it's often easier to just edit manually
# but here is a basic append logic:
echo "(release \"$VERSION\")" >> srfi-170.release-info

# 3. Git dance
git add srfi-170.egg srfi-170.release-info
git commit -m "Release $VERSION"
git tag -a $TAG -m "Version $VERSION"
git push origin master --tags

echo "Release $VERSION pushed to GitHub!"
