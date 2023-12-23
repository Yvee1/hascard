set -o errexit -o verbose

if test ! "$RELEASE_VERSION"
then
  echo 'This is not a release build.'
else
  mkdir -p "$HOME/.local/bin"
  echo "Installing ghr for $RUNNER_OS..."
  if [ "$RUNNER_OS" = "Linux" ] 
  then
    ARCH="linux"
    URL="https://github.com/tcnksm/ghr/releases/download/v0.14.0/ghr_v0.14.0_${ARCH}_amd64.tar.gz"
    curl -L ${URL} > ghr.tar.gz
    gunzip ghr.tar.gz
    tar -x -f ghr.tar --strip-components 1
    mv ghr "$HOME/.local/bin/"
    rm ghr.tar
  elif [ "$RUNNER_OS" = "Windows" ]
  then
    ARCH="windows"
    URL="https://github.com/tcnksm/ghr/releases/download/v0.14.0/ghr_v0.14.0_${ARCH}_amd64.zip"
    curl -L ${URL} > ghr.zip
    unzip ghr.zip
    GHR_DIR="ghr_v0.14.0_${ARCH}_amd64"
    mkdir "$HOME/ghr"
    mv $GHR_DIR/ghr.exe "$HOME/ghr"
    export PATH=$PATH:"$HOME/ghr"
    rm ghr.zip
    rm -r $GHR_DIR
  else
    ARCH="darwin"
    URL="https://github.com/tcnksm/ghr/releases/download/v0.14.0/ghr_v0.14.0_${ARCH}_amd64.zip"
    curl -L ${URL} > ghr.zip
    unzip ghr.zip
    GHR_DIR="ghr_v0.14.0_${ARCH}_amd64"
    mv $GHR_DIR/ghr "$HOME/.local/bin/"
    rm ghr.zip
    rm -r $GHR_DIR
  fi
  ls "$HOME/.local/bin/"
fi
