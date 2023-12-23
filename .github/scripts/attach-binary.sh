set -o errexit -o verbose

if test ! "$RELEASE_VERSION"
then
  echo 'This is not a release build.'
else
  echo "Attaching binary for $RUNNER_OS to $RELEASE_VERSION..."
  if [ "$RUNNER_OS" = "Linux" ] 
  then
      ARCH="linux"
  elif [ "$RUNNER_OS" = "Windows" ]
  then
      ARCH="windows"
  else
      ARCH="osx"
  fi
  OWNER="$(echo "$GITHUB_REPOSITORY" | cut -f1 -d/)"
  REPO="$(echo "$GITHUB_REPOSITORY" | cut -f2 -d/)"
  if [ "$RUNNER_OS" = "Windows" ]
  then
    BIN="$(stack path --local-install-root)/bin/$REPO.exe"
    cp "$BIN" "./$REPO.exe"
    chmod +x "./$REPO.exe"
    BUNDLE_NAME="$REPO-$RELEASE_VERSION-$ARCH.zip"
    powershell Compress-Archive -Path "$REPO.exe" -DestinationPath "$BUNDLE_NAME"
    $HOME/ghr/ghr.exe -t "$GITHUB_TOKEN" -u "$OWNER" -r "$REPO" --replace "$(git describe --tags)" "$BUNDLE_NAME"
  else
    BIN="$(stack path --local-install-root)/bin/$REPO"
    BUNDLE_NAME="$REPO-$RELEASE_VERSION-$ARCH.tar.gz"
    cp "$BIN" "./$REPO"
    chmod +x "./$REPO"
    tar -czf "$BUNDLE_NAME" "$REPO"
    echo "SHA256:"
    shasum -a 256 "$BUNDLE_NAME"
    ls $HOME/.local/bin/
    echo $PATH
    $HOME/.local/bin/ghr -t "$GITHUB_TOKEN" -u "$OWNER" -r "$REPO" --replace "$(git describe --tags)" "$BUNDLE_NAME"
  fi
fi
