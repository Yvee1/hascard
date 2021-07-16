set -o errexit -o verbose

if test ! "$RELEASE_VERSION"
then
  echo 'This is not a release build.'
else
  echo "Attaching binary for $RUNNER_OS to $RELEASE_VERSION..."
  OWNER="$(echo "$GITHUB_REPOSITORY" | cut -f1 -d/)"
  REPO="$(echo "$GITHUB_REPOSITORY" | cut -f2 -d/)"
  BIN="$(stack path --local-install-root)/bin/$REPO"
  BUNDLE_NAME="$REPO-$RELEASE_VERSION-$RUNNER_OS.tar.gz"
  cp "$BIN" "./$REPO"
  chmod +x "./$REPO"
  tar -czf "$BUNDLE_NAME" "$REPO"
  echo "SHA256:"
  shasum -a 256 "$BUNDLE_NAME"
  ghr -t "${{ secrets.GITHUB_TOKEN }}" -u "$OWNER" -r "$REPO" --replace "$(git describe --tags)" "$BUNDLE_NAME"
fi
