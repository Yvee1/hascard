set -o errexit -o verbose

if test ! "$RELEASE_VERSION"
then
  echo 'This is not a release build.'
else
    if [ "$RUNNER_OS" = "Linux" ] 
    then
        ARCH="linux"
    else
        ARCH="darwin"
    fi
  echo "Installing ghr"
  URL="https://github.com/tcnksm/ghr/releases/download/v0.5.4/ghr_v0.5.4_${ARCH}_386.zip"
  curl -L ${URL} > ghr.zip
  mkdir -p "$HOME/.local/bin"
  unzip ghr.zip -d "$HOME/.local/bin/"
  rm ghr.zip
fi
