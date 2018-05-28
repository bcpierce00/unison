## basic OCaml and opam installation

full_apt_version () {
  package=$1
  version=$2
  case "${version}" in
      latest) echo -n "${package}" ;;
      *) echo -n "${package}="
         apt-cache show "$package" \
             | sed -n "s/^Version: \(${version}\)/\1/p" \
             | head -1
  esac
}

set -ue

# the ocaml version to test
OCAML_VERSION=${OCAML_VERSION:-latest}
OPAM_VERSION=${OPAM_VERSION:-1.2.2}
OPAM_INIT=${OPAM_INIT:-true}
OPAM_SWITCH=${OPAM_SWITCH:-system}

# the base opam repository to use for bootstrapping and catch-all namespace
BASE_REMOTE=${BASE_REMOTE:-git://github.com/ocaml/opam-repository}

# whether we need a new gcc and binutils
UPDATE_GCC_BINUTILS=${UPDATE_GCC_BINUTILS:-"0"}

# Install Trusty remotes
UBUNTU_TRUSTY=${UBUNTU_TRUSTY:-"0"}

case "$OCAML_VERSION" in
    latest) OCAML_VERSION=4.02;;
esac

install_on_linux () {
  case "$OCAML_VERSION,$OPAM_VERSION" in
    3.12,1.2.2) ppa=avsm/ocaml312+opam12 ;;
    4.00,1.2.2) ppa=avsm/ocaml40+opam12 ;;
    4.01,1.2.2) ppa=avsm/ocaml41+opam12 ;;
    4.02,1.1.2) OPAM_SWITCH=4.02.3; ppa=avsm/ocaml42+opam11 ;;
    4.02,1.2.0) OPAM_SWITCH=4.02.3; ppa=avsm/ocaml42+opam120 ;;
    4.02,1.2.1) OPAM_SWITCH=4.02.3; ppa=avsm/ocaml42+opam121 ;;
    4.02,1.2.2) ppa=avsm/ocaml42+opam12 ;;
    4.03,1.2.2)
       OCAML_VERSION=4.02; OPAM_SWITCH="4.03.0+trunk";
       ppa=avsm/ocaml42+opam12 ;;
    *) echo "Unknown OCAML_VERSION=$OCAML_VERSION OPAM_VERSION=$OPAM_VERSION"
       exit 1 ;;
  esac

  sudo add-apt-repository --yes ppa:${ppa}
  sudo apt-get update -qq
  sudo apt-get install -y \
     "$(full_apt_version ocaml $OCAML_VERSION)" \
     "$(full_apt_version ocaml-base $OCAML_VERSION)" \
     "$(full_apt_version ocaml-native-compilers $OCAML_VERSION)" \
     "$(full_apt_version ocaml-compiler-libs $OCAML_VERSION)" \
     "$(full_apt_version ocaml-interp $OCAML_VERSION)" \
     "$(full_apt_version ocaml-base-nox $OCAML_VERSION)" \
     "$(full_apt_version ocaml-nox $OCAML_VERSION)" \
     "$(full_apt_version camlp4 $OCAML_VERSION)" \
     "$(full_apt_version camlp4-extra $OCAML_VERSION)" \
     opam

  TRUSTY="deb mirror://mirrors.ubuntu.com/mirrors.txt trusty main restricted universe"

  if [ "$UPDATE_GCC_BINUTILS" != "0" ] ; then
    echo "installing a recent gcc and binutils (mainly to get mirage-entropy-xen working!)"
    sudo add-apt-repository "${TRUSTY}"
    sudo add-apt-repository --yes ppa:ubuntu-toolchain-r/test
    sudo apt-get -qq update
    sudo apt-get install -y gcc-4.8
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-4.8 90
    wget http://mirrors.kernel.org/ubuntu/pool/main/b/binutils/binutils_2.24-5ubuntu3.1_amd64.deb
    sudo dpkg -i binutils_2.24-5ubuntu3.1_amd64.deb
    sudo add-apt-repository -r "${TRUSTY}"
  fi

  if [ "$UBUNTU_TRUSTY" != "0" ] ; then
    echo "Adding Ubuntu Trusty mirrors"
    sudo add-apt-repository "${TRUSTY}"
    sudo apt-get -qq update
  fi

}

install_on_osx () {
  # curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
  # sudo hdiutil attach XQuartz-2.7.6.dmg
  # sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
  brew update >/dev/null
  brew upgrade python
  case "$OCAML_VERSION,$OPAM_VERSION" in
    3.12,1.2.2) OPAM_SWITCH=3.12.1; brew install opam ;;
    4.00,1.2.2) OPAM_SWITCH=4.00.1; brew install opam ;;
    4.01,1.2.2) OPAM_SWITCH=4.01.0; brew install opam ;;
    4.02,1.2.2) brew install ocaml; brew install opam ;;
    4.02,1.3.0) brew install ocaml; brew install opam --HEAD ;;
    4.03,1.2.2) brew install ocaml --HEAD; brew install opam ;;
    *) echo "Unknown OCAML_VERSION=$OCAML_VERSION OPAM_VERSION=$OPAM_VERSION"
       exit 1 ;;
  esac
}

case $TRAVIS_OS_NAME in
    osx) install_on_osx ;;
    linux) install_on_linux ;;
esac

export OPAMYES=1

case $OPAM_INIT in
  true)
      opam init -a "$BASE_REMOTE" --comp="$OPAM_SWITCH"
      eval $(opam config env)
      ;;
esac

cat >.travis-ocaml.env <<EOM
OCAML_VERSION=$OCAML_VERSION
OPAM_SWITCH=$OPAM_SWITCH
EOM

ocaml -version
opam --version
opam --git-version
