set -o errexit -o verbose

if test ! "$GITHUB_REF"
then
  echo 'This is not a release build.'
else
  echo "Attaching binary for $RUNNER_OS to $GITHUB_REF..."
  OWNER="$(echo "$GITHUB_REPOSITORY" | cut -f1 -d/)"
  REPO="$(echo "$GITHUB_REPOSITORY" | cut -f2 -d/)"
  BIN="$(stack path --local-install-root)/bin/$REPO"
  BUNDLE_NAME="$REPO-$GITHUB_REF-$RUNNER_OS.tar.gz"
  cp "$BIN" "./$REPO"
  chmod +x "./$REPO"
  tar -czf "$BUNDLE_NAME" "$REPO"
  echo "SHA256:"
  shasum -a 256 "$BUNDLE_NAME"
  ghr -t "${{ secrets.GITHUB_TOKEN }}" -u "$OWNER" -r "$REPO" --replace "$(git describe --tags)" "$BUNDLE_NAME"
fi
