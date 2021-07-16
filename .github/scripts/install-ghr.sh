set -o errexit -o verbose

if test ! "$GITHUB_REF"
then
  echo 'This is not a release build.'
else
    if [ "$RUNNER_OS" = "linux" ] 
    then
        ARCH="linux"
    else
        ARCH="darwin"
    fi
  echo "Installing ghr"
  URL="https://github.com/tcnksm/ghr/releases/download/v0.5.4/ghr_v0.5.4_${ARCH}_386.zip"
  curl -L ${URL} > ghr.zip
  mkdir -p "$HOME/bin"
  export PATH="$HOME/bin:$PATH"
  unzip ghr.zip -d "$HOME/bin"
  rm ghr.zip
fi
