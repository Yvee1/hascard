set -o errexit -o verbose

if test -f "$HOME/.local/bin/stack"
then
  echo 'Stack is already installed.'
else
  echo "Installing Stack for $RUNNER_OS..."
  if [ "$RUNNER_OS" = "Linux" ] 
  then
      ARCH="linux"
  elif [ "$RUNNER_OS" = "Windows" ]
  then
      ARCH="windows"
  else
      ARCH="osx"
  fi
  URL="https://www.stackage.org/stack/$ARCH-x86_64"
  curl --location "$URL" > stack.tar.gz
  gunzip stack.tar.gz
  tar -x -f stack.tar --strip-components 1
  mkdir -p "$HOME/.local/bin"
  if [ "$RUNNER_OS" = "Windows" ]
  then
      mkdir "$HOME/stack"
      mv stack.exe "$HOME/stack"
      export PATH=$PATH:"$HOME/stack"
  else
      mv stack "$HOME/.local/bin/"
  fi
  rm stack.tar
fi

stack --version
