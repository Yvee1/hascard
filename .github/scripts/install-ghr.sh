set -o errexit -o verbose

if test ! "$RELEASE_VERSION"
then
  echo 'This is not a release build.'
else
  mkdir -p "$HOME/.local/bin"
  echo "Installing ghr"
    if [ "$RUNNER_OS" = "Linux" ] 
    then
        ARCH="linux"
        URL="https://github.com/tcnksm/ghr/releases/download/v0.14.0/ghr_v0.14.0_${ARCH}_amd64.tar.gz"
        curl -L ${URL} > ghr.tar.gz
        gunzip ghr.tar.gz
        tar -x -f ghr.tar --strip-components 1
        mv ghr "$HOME/.local/bin/"
        rm ghr.tar
    else
        ARCH="darwin"
        URL="https://github.com/tcnksm/ghr/releases/download/v0.14.0/ghr_v0.14.0_${ARCH}_amd64.zip"
        curl -L ${URL} > ghr.zip
        unzip ghr.zip
        GHR_DIR="ghr_v0.14.0_${ARCH}_amd64"
        mv GHR_DIR/ghr "$HOME/.local/bin/"
        rm ghr.zip
        rm GHR_DIR -r
    fi
  ls "$HOME/.local/bin/"
fi
