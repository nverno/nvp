#!/usr/bin/env bash

# See:
# https://github.com/budhash/install-eclipse

set -e
. ~/bin/utils.sh

readonly __SUPPORTED_OS=(LINUX-DEBIAN)
readonly __SUPPORTED_ARCH=(x86_64 x86)
readonly __DEBUG=TRUE

##
# @info     load all common application related variables
# @param    na
# @return   na
##
_load_config() {
    _ECLIPSE_VERSION=4.6
    _ECLIPSE_DNLD_MIRROR=http://mirror.cc.columbia.edu/pub/software/eclipse/eclipse/downloads
    _ECLIPSE_DOWNLOAD_URL=http://download.eclipse.org/eclipse/downloads/
}

install() {
    _check_install
    _load_config

    local _force_install=false
    local _plugins_only=false
    local _optimize_install=false
    local _plugin_config=
    local _plugin_name=
    local _download_url=

    while getopts "hfnop:c:d:" o
    do case "$o" in
           h) _usage; exit 0;;
           n) _plugins_only=true;;
           f) _force_install=true;;
           o) _optimize_install=true;;
           c) _plugin_config=$OPTARG;;
           d) _download_url=$OPTARG;;
           p) 
               _plugin_name=$OPTARG
               _exists=false
               for p in "${_plugins[@]}"; do 
                   [ "$p" == "$_plugin_name"] && _exists=true
                   break
               done
               [ ! "$_exists" == "true" ] && _plugins=("${_plugins[@]}" $_plugin_name)
               ;;
           [?]) _usage error;;
       esac
    done
    shift $(($OPTIND - 1))

    # check installation directory
    [[ -z "$1" ]] && local _install_dir=~/.local/eclipse ||
            local _install_dir=$1
    
    _log "installing to $install_dir"
    if [ "$_plugins_only" == "false" ]; then
        if [ -d $_install_dir ]; then
            if [ "$_force_install" == "true" ]; then
                _log "removing existing folder [$_install_dir]"
                rm -rf "$_install_dir"
            else
                _error_exit "install folder already exists"
            fi
        fi
    fi
    
    # install
    install_app "$_install_dir" "$_download_url"
}

##
# @info     starts the installation process
# @param    na
# @return   na
##
install_app() {
    _log "installing eclipse..."
    [[ -z "$1" ]] && local _install_dir="$HOME./local/eclipse" ||
            local _install_dir=$1
    local _download_url=$2
    
    [ -d $_install_dir ] && _error_exit "destination folder already exists"
    
    if [ "$_download_url" == "" ] || [ "$_download_url" == "-" ]; then
        _download_url=$(get_download_url $__OS $__ARCH)
        [ "$_download_url" == "" ] &&
            _error_exit "unable to find download url for os=$__OS arch=$__ARCH"
    fi
    
    _download_extract "$_download_url" "$_install_dir"
}

##
# @info		fix $ECLIPSE_HOME environment variable
# @param	install_dir string
# @return	na
##
fix_env () {
    [[ -z "$1" ]] && _error_exit "install_dir not specified"
    local re="s;ECLIPSE_HOME=.*;ECLIPSE_HOME=\"$1\";"
    sed -i $re ~/dotfiles/_env 
}

##
# @info     extracts and returns the download url for given os/architecture
# @param    os enum
# @param    cpu architecture enum
# @return   download url
##
get_download_url() {
    [[ -z "$1" ]] && _error_exit "os not specified" || local _os=$1
    [ -z $2 ] && _error_exit "arch not specified" || local _arch=$2
    local _tmp_url_a=$(_get_url $_ECLIPSE_DOWNLOAD_URL | 
                           grep -A1 "Latest Release" | 
                           grep -Eo "drop.*/\"" | awk -F\" '{print $1}')
    local _dnld_url=
    
    case "$_os" in
        MAC) 
            local _tmp_url_b=$(_get_url $_ECLIPSE_DOWNLOAD_URL$_tmp_url_a | 
                                   grep eclipse-platform | 
                                   grep `_lowercase $_os` |
                                   grep $_arch |
                                   awk -F"href=\"download.php" '{print $2}' | 
                                   awk -F\" '{print $1}' | awk -F= '{print $2}')
            #echo http://www.eclipse.org/downloads/download.php?file=/eclipse/downloads/drops4/R-4.3-201306052000/eclipse-platform-4.3-macosx-cocoa-x86_64.tar.gz\&mirror_id=454
            _dnld_url="$_ECLIPSE_DNLD_MIRROR/${_tmp_url_a}${_tmp_url_b}"
            ;;
        LINUX*)
            local _tmp_url_b=$(_get_url $_ECLIPSE_DOWNLOAD_URL$_tmp_url_a | 
                                   grep eclipse-platform | 
                                   grep linux | 
                                   grep $_arch |
                                   awk -F"href=\"download.php" '{print $2}' | 
                                   awk -F\" '{print $1}' | awk -F= '{print $2}')
            #http://download.eclipse.org/eclipse/downloads/drops4/R-4.3-201306052000/download.php?dropFile=eclipse-SDK-4.3-linux-gtk-x86_64.tar.gz
            _dnld_url="$_ECLIPSE_DNLD_MIRROR/${_tmp_url_a}${_tmp_url_b}"
            ;;
        *) _error_exit "unsupported os/architecture";;
    esac

    _log "extracted download url [$_dnld_url]"
    echo "$_dnld_url"    
}

# check OS and ARCH
_check_install() {
    readonly __BASEDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
    readonly __CFGFILE="$( basename "${BASH_SOURCE[0]}" )".ini
    readonly __LOGFILE=./"$( basename "${BASH_SOURCE[0]}" )".log
    if [ -f "$__LOGFILE" ]; then
        rm -rf "$__LOGFILE"
        (
            echo "----------------------------------------"
            echo "`date`"
            echo "----------------------------------------"
        ) >> "$__LOGFILE"
    fi

    # check os
    readonly __OS=$(_get_os)
    local _supported=false
    for a in "${__SUPPORTED_OS[@]}"; do [[ "$a" == "$__OS" ]] && _supported=true; done
    [ "$_supported" != "true" ] && 
        _error_exit "script doesn't support $(_get_os). supported: $__SUPPORTED_OS"
    
    # check arch
    readonly __ARCH=$(_get_arch)
    local _supported=false
    for a in "${__SUPPORTED_ARCH[@]}"; do [[ "$a" == "$__ARCH" ]] && _supported=true; done
    [ "$_supported" != "true" ] &&
        _error_exit "script doesn't support $(_get_arch). supported: $__SUPPORTED_ARCH"

    _log "OS/ARCH are supported"
}

##
# @info     usage information
# @param    na
# @return   na
##
_usage() {
    if [ "$1" != "error" ]; then
        echo "$__APPNAME $__APPVERSION, non-interactive eclipse installer"
    fi
    cat << EOF
Usage: $__APPNAME [OPTIONS]... install_folder

Options:
-------
    -h                          
        show this message
        
    -d <download_url>           
        download url to use. if this is not specified, 
        the download url is extracted from download site
        
    -p <"repository,plugin_id">   
        information about plugin to be installed. 
        it should be in the format "repository,plugin_id"
        
    -c <config_file>            
        config file containing plugin information. 
        it should be in the format "repository,plugin_id" per line
        
    -f                          
        force remove existing install_folder, if it exists
        
    -o                      
        optimize the eclipse.ini file [EXPERIMENTAL]

Examples:
--------
    -install latest version without any plugins into "eclipse" folder
    $__APPNAME eclipse
    
    -install eclipse and jdt plugin
    $__APPNAME -p "http://download.eclipse.org/releases/kepler,org.eclipse.jdt.feature.group" eclipse
    
    -install eclipse, jdt plugin and testng plugin
    $__APPNAME -p "http://download.eclipse.org/releases/kepler,org.eclipse.jdt.feature.group" -p "http://beust.com/eclipse/,org.testng.eclipse" eclipse
    
    -install eclipse along with plugins specified in a config file
    $__APPNAME -c ./plugins.cfg eclipse
    
    -install eclipse along with plugins specified in a remote config file
    $__APPNAME -c http://127.0.0.1:8000/plugins.cfg  eclipse
    
    -install plugins specified without instastalling (in an existing installation)
    $__APPNAME -n -c http://127.0.0.1:8000/plugins.cfg eclipse
    
    -install eclipse and remove existing destination folder if it exists
    $__APPNAME -f eclipse
    
    -install eclipse and optimize eclipse.ini file [EXPERIMENTAL]
    $__APPNAME -f -o eclipse
    
    -misc commands
    $__APPNAME -o -f -c http://myserver/common/plugins/java_plugins.cfg -d http://myserver/common/binaries/eclipse-platform-4.3-macosx-cocoa-x86_64.tar.gz

EOF
    if [ "$1" == "error" ]; then
        exit 1
    fi
}

##
# @info     returns the current cpu architure
# @param    na
# @param    installation directory
# @param    config file containing plugin details
# @return   na
##
function optimize_install(){
    _log "optimizing installation"
    #[[ -z "$1" ]] && _error_exit "os not specified" || local _os=$1
    [[ -z "$1" ]] && _error_exit "installation directory not specified" || 
            local _install_dir=$1
    local _eclipse_ini=
    
    case "$__OS" in
        MAC) 
	    if [ -f "$_install_dir"/Contents/MacOS/eclipse ]; then
		_eclipse_ini="$_install_dir"/Contents/MacOS/eclipse.ini
	    else
		_eclipse_ini="$_install_dir"/Eclipse.app/Contents/MacOS/eclipse.ini	
	    fi
	    ;;
        LINUX*) _eclipse_ini="$_install_dir/eclipse.ini" ;;
        *)      _error_exit "unsupported os/architecture";;
    esac

    [ -f "$_eclipse_ini" ] || _error_exit "eclipse.ini file not found at [$_eclipse_ini]"
    
    if [ ! -f "$_eclipse_ini.orig" ]; then
        _debug "backing up existing eclipse.ini to eclipse.ini.orig"
        cp "$_eclipse_ini" "${_eclipse_ini}".orig
    fi
    
    echo -Xverify:none >> "$_eclipse_ini"
    echo -Xincgc >> "$_eclipse_ini"
    echo -Xss4m >> "$_eclipse_ini"
    echo -Xms128m >> "$_eclipse_ini"
    echo -Xmx1024m >> "$_eclipse_ini"
    echo -XX:PermSize=256m >> "$_eclipse_ini"
    echo -XX:MaxPermSize=512m >> "$_eclipse_ini"
    echo -XX:MaxGCPauseMillis=10 >> "$_eclipse_ini"
    echo -XX:MaxHeapFreeRatio=70 >> "$_eclipse_ini"
    echo -XX:+UseConcMarkSweepGC >> "$_eclipse_ini"
    echo -XX:+CMSIncrementalMode >> "$_eclipse_ini"
    echo -XX:+CMSIncrementalPacing >> "$_eclipse_ini"
    echo -XX:+UseFastAccessorMethods >> "$_eclipse_ini"
    echo -XX:+UseFastAccessorMethods >> "$_eclipse_ini"
    echo -XX:+UnlockExperimentalVMOptions >> "$_eclipse_ini"
    echo -XX:+AggressiveOpts >> "$_eclipse_ini"
    echo -XX:+DoEscapeAnalysis >> "$_eclipse_ini"
    echo -XX:+UseCompressedOops >> "$_eclipse_ini"
    echo -XX:+ExplicitGCInvokesConcurrentAndUnloadsClasses >> "$_eclipse_ini"
}

##
# @info     installs plugins(s) specified in a configuration file
# @param    na
# @param    installation directory
# @param    config file containing plugin details
# @return   na
##
function install_plugins(){
    [[ -z "$1" ]] && _error_exit "plugin config file not specified" || 
            local _plugin_config=$1
    [[ -z "$2" ]] && _error_exit "installation directory not specified" || 
            local _install_dir=$2
    local _cmd=
    if [ $(echo $_plugin_config | grep -Eoc '^(http|https)://.*') -gt 0 ]; then
        #check to see if the download file exists
        [ "$(_get_responsecode $_plugin_config)" == "200" ] || 
            _error_exit "plugin config file not found at [$_plugin_config]"
        _cmd=_get_url
    else
        [ -f "$_plugin_config" ] || 
            _error_exit "plugin config file not found at [$_plugin_config]"
        _cmd=cat
    fi

    _log "installing plugins from [$_plugin_config]"
    
    $_cmd "$_plugin_config" | \
        while read line; do
            install_plugin "$_install_dir" "$line"
        done
    
    #remove temp dir if it exists
    [ -d "$_tmpdir" ] && rm -rf "$_tmpdir"
}

##
# @info     installs the specified plugin
# @param    na
# @param    installation directory
# @param    repository,plugin_id (in a single line)
# @return   na
##
function install_plugin(){
    local _install_dir=$1
    shift 1

    local line="$@" # get all args
    local line=$(echo "$@" | sed 's/^[ \t]*//')
    local first_char=`echo ${line} | awk '{ print substr( $0, 0, 1 ) }'`
    [ "${first_char}" == "#" ] && return 1    
    local _repo=`echo "${line}" | awk -F, '{print $1}'`
    local _plugin=`echo "${line}" | awk -F, '{print $2}'`

    _log "installing plugin: $_plugin"

    #handle os specific folder structures
    local _eclipse_exec="$_install_dir"/eclipse
    if [ -f "$_install_dir"/Contents/MacOS/eclipse ]; then
	_eclipse_exec="$_install_dir"/Contents/MacOS/eclipse
    fi

    local _success=$($_eclipse_exec -nosplash 
                     -application org.eclipse.equinox.p2.director
                     -repository "$_repo" -installIU "$_plugin" 2>>$__LOGFILE | 
                         grep -Eoc '^Operation completed in')
    [ $_success -eq 0 ] && 
        _error "plugin [$_plugin] installation failed. see [$__LOGFILE] for more details"
}

_log "$@"
install "$@"
