<# 
.SYNOPSIS
  Functions to elevate scripts to admin role.
.LINK
  https://www.autoitscript.com/forum/topic/174609-powershell-script-to-self-elevate/
#>

# Test if running as admin
function Test-IsAdmin () {
<# 
.SYNOPSIS
  Test if script is being run in admin role.
#>
  # Get current ID and its security principal
  $windowsID = [System.Security.Principal.WindowsIdentity]::GetCurrent()
  $windowsPrincipal = New-Object System.Security.Principal.WindowsPrincipal($windowsID)
  
  # Get the admin role security principal
  $adminRole = [System.Security.Principal.WindowsBuiltInRole]::Administrator
  
  # Are we an admin role?
  if ($windowsPrincipal.IsInRole($adminRole)) {
    $true
  } else {
    $false
  }
}

function Get-UNCFromPath {
<# 
.SYNOPSIS
  Return UNC path from mapped drive.
#>
  Param(
    [Parameter(Position=0, Mandatory=$true, ValueFromPipeline=$true)]
    [String]$Path)

  if ($Path.Contains([io.path]::VolumeSeparatorChar)) {
    $psdrive = Get-PSDrive -Name $Path.Substring(0, 1) -PSProvider 'FileSystem'

    # Is it a mapped drive?
    if ($psdrive.DisplayRoot) {
      $Path = $Path.Replace(
        $psdrive.Name + [io.path]::VolumeSeparatorChar, $psdrive.DisplayRoot)
    }
  }
  
  return $Path
}

# Relaunch the script if not admin
function Invoke-RequireAdmin {
<# 
.SYNOPSOS
  If script is not running in admin role, then relaunch in admin role.
#>
  Param(
    [Parameter(Position=0, Mandatory=$true, ValueFromPipeline=$true)]
    [System.Management.Automation.InvocationInfo]
    $MyInvocation)

  if (-not (Test-IsAdmin)) {
    # Get the script path
    $scriptPath = $MyInvocation.MyCommand.Path
    $scriptPath = Get-UNCFromPath -Path $scriptPath

    # Need to quote the paths in case of spaces
    $scriptPath = '"' + $scriptPath + '"'

    # Build base arguments for powershell.exe
    [string[]]$argList = @('-NoLogo -NoProfile', `
      '-ExecutionPolicy Bypass', `
      '-File', $scriptPath)

    # Add 
    $argList += $MyInvocation.BoundParameters.GetEnumerator() | %{
      "-$($_.Key)", "$($_.Value)"
    }
    $argList += $MyInvocation.UnboundArguments

    try {    
      $process = Start-Process PowerShell.exe -PassThru `
        -Verb Runas -Wait `
        -ErrorAction SilentlyContinue -WorkingDirectory $pwd `
        -ArgumentList $argList
      exit $process.ExitCode
    } catch {}

    # Generic failure code
    exit 1 
  }
}

# Relaunch if not admin
# Invoke-RequireAdmin $SCRIPT:MyInvocation

# Running as admin if get this far
# $wshell=  New-Object -ComObject Wscript.Shell
# $wshell.Popup("Script is running as admin", 0, "Done", 0x1) | Out-Null

# ------------------------------------------------------------
#* NOTE: this isn't working correctly

function Start-NoPrompt {
<# 
.SYNOPSIS
  Run script without prompts/window popus. With argument $asAdmin, read
  password from ~/password.bin and run script elevated.
#>
  Param([switch]$asAdmin,
        $params)

  if ($asAdmin) {
    $pdir = (Split-Path $SCRIPT:MyInvocation.MyCommand.Path -parent)
    $hdir = $env:HOME
    
    # check that password.bin exists
    $pswdpath = (Join-Path $env:HOME "password.bin")
    if (![System.IO.File]::Exists($pswdpath)) {
      $pscript = (Join-Path $pdir "Store-Password.ps1")
      Start-Process powershell -Wait -ArgumentList '-File', $pscript
    }
    $encpwd = Get-Content $pswdpath
    $passwd = ConvertTo-SecureString $encpwd
    $user = $env:UserName
    $cred = new-object System.Management.Automation.PSCredential $user,$passwd

    Write-Host Running elevated
    # Start-Process powershell -WindowStyle hidden -Credential $cred
    # -ArgumentList $params
    Start-Process powershell -WindowStyle hidden -Verb runas -ArgumentList $params
  } else {
    Start-Process powershell -WindowStyle hidden -ArgumentList $params
  }
}
