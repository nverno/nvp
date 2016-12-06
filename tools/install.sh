#!/usr/bin/env bash

install_zeal() {
    sudo add-apt-repository -y ppa:zeal-developers/ppa
    sudo apt-get -qq update
    sudo apt-get install -y zeal
}

"$@"
