#!/usr/bin/env bash

. ~/bin/versions.sh
. ~/bin/utils.sh
. ~/bin/install/install.sh
. ~/.env

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)" 
cd "$DIR"

install() {
    local uri ver

    if ! hash eclipse 2>/dev/null ; then
        _log "Installing eclipse"
        _install_eclipse
    fi

    if ! ls "$ECLIPSE_HOME" 2>/dev/null | grep eclim ; then 
        _log "Installing eclim"
        _install_eclim
    fi
}

install_jdk() {
    if ! hash javac 2>/dev/null ; then
        _log "Installing java"
        
        # oracle
        sudo add-apt-repository -y ppa:webupd8team/java
        sudo apt-get update
        yes | sudo apt-get install -y oracle-java8-installer

        # sudo update-alternatives --config java
        sudo apt-get install -y oracle-java8-set-default
    fi

    # maven
    if ! hash mvn 2>/dev/null ; then
        sudo apt-get install -y maven
    fi
}

install_gradle() {
    if ! hash gradle 2>/dev/null ; then
        asdf plugin-add gradle
        local ver=$(asdf list-all gradle | tail -n1)
        asdf install gradle $ver
        asdf global gradle $ver
    else
        _log "Using gradle verion: $(asdf current gradle)"
    fi
}

# Note: requires JAVA_HOME to have been set correctly
install_hadoop() {
    local uri="http://mirror.reverse.net/pub/apache/hadoop/common/current"
    if ! hash hadoop 2>/dev/null ; then
        ver=$(get_latest_version $uri 3 "hadoop-\K[0-9]+\.[0-9]+\.[0-9]+")
        if [ -n "$ver" ]; then
            (
                cd /tmp
                _log "Downloading $uri/hadoop-$ver.tar.gz"
                wget "$uri/hadoop-$ver.tar.gz"
                tar xvzf "hadoop-$ver.tar.gz"
                if [ -n "$HADOOP_HOME" ]; then
                    mv "hadoop-$ver" "$HADOOP_HOME"
                    _log "Hadoop resides in $HADOOP_HOME"
                else 
                    mv "hadoop-$ver" "~/.local/hadoop"
                    _log "Hadoop resides in ~/.local/hadoop"
                fi
            )
        else
            _error_exit "Failed to download hadoop"
        fi
    fi
}

_install_eclim() {
    uri="https://github.com/ervandew/eclim/releases"
    ver=$(get_latest_version $uri 3 "eclim_\K[0-9]+\.[0-9]+\.[0-9]+")
    # ver="2.7.0"
    if [ -n $ver ]; then
        uri="$uri/download/$ver/eclim_$ver.jar"
        _log "Downloading: eclim version - $ver"
        _log "from $uri"
        (
            cd /tmp
            wget $uri
                    
            # install eclim
            if [ -f eclim_$ver.jar ]; then
                # -Dvim.files=$HOME/.vim
                java -Dvim.skip=true -Declipse.home=$ECLIPSE_HOME                 \
                     -jar eclim_$ver.jar install
                
            else
                _error_exit "Failed to download eclim"
            fi
        )
    fi
    
    # dots
    cp ~/dotfiles/eclipse/_eclimrc ~/.eclimrc
}

# https://github.com/budhash/install-eclipse/blob/master/install-eclipse
_install_eclipse() {
    # local ver uri
    # ver="${1-neon}"
    # uri="http://www.eclipse.org/downloads/index-developer.php"
    # ver=$(curl -Ls -A "Mozilla/5.0 (X11; Linux x86_64)" $uri 2>/dev/null |
    #           grep -Poi --max-count=1                                           \
    #                "/download.*linux\-gtk\-x86_64\.tar\.gz")
    # if [ -z "$ver" ]; then
    #     _error_exit "Failed to get eclipse version.. peace"
    # fi
    # uri="http://www.eclipse.org/$ver"

    # ver=$(curl -Ls -A "Mozilla/5.0 (X11; Linux x86_64)" $uri 2>/dev/null |
    #           grep -Poi --max-count=1 "download.*\.tar\.gz")
    # uri="http://www.eclipse.org/downloads/$ver"

    # printf "Downloading eclipse from: $uri\n"
    # cd "$DEVEL"
    # curl -L -O "$uri"

    # install eclipse
    export ECLIPSE_HOME="$HOME/.local/eclipse"
    bash <( curl -sk https://raw.githubusercontent.com/budhash/install-eclipse/master/install-eclipse ) -c ./git-java-mvn.cfg ~/.local/eclipse
    # ./install_eclipse.sh -p "http://download.eclipse.org/releases/kepler,org.eclipse.jdt.feature.group"

    # dots
    # sed -i -e "s;export ECLIPSE_HOME.*$;export ECLIPSE_HOME=\"$ECLIPSE_HOME\";" \
    #         ~/dotfiles/_env
    # cp ~/dotfiles/_env ~/.env
}

"$@"
