<# 
.SYNOPSIS
  Update database of executables.
.PARAMETER searchPath
  List of directories to index.
.PARAMETER store
  Location of index file, defaults to $env:APPDATA\exe-index
.PARAMETER clear
  If true, delete the index, otherwise append to it. If searchPaths is
  "all", the index is deleted regardless.
#>

Param([string[]]$searchPath,
      $store="$env:APPDATA\exe-index",
      $clear=$false)

$parent = (Split-Path -Parent $MyInvocation.MyCommand.Definition)
$elevate = Join-Path $parent "Invoke-RequireAdmin.ps1"
. $elevate
Invoke-RequireAdmin $SCRIPT:MyInvocation

Test-IsAdmin
Write-Host $searchPath, $store, $clear

if ($clear -and $(Test-Path $store)) {
  Remove-Item $store 
}

if ($searchPath -eq $null) {
  if ((-not $clear) -and $(Test-Path $store)) {
    Remove-Item $store
  }
  Get-ChildItem -Force -Path ${env:ProgramFiles},${env:ProgramFiles(x86)},`
    ${env:ChocolateyToolsLocation},${env:DEVEL},`
    ${env:MSYS_HOME},${env:CYGWIN_HOME},`
    ${env:ChocolateyInstall},${env:HOME},C:\strawberry,`
    ${env:USERPROFILE},${env:WINDIR} `
    -Recurse -Include *.exe,*.bat,*.ps1,*.psm1,*.cmd |
    %{$_.FullName} | Add-Content $store
  # $proc = Start-Process -PassThru $(Join-Path $parent "makedbs.bat")
  # Write-Host $proc.ProcessName: $proc.ID
} else {
  foreach ($p in $searchPath) {
    Get-ChildItem $p -Recurse -Force -Include *.exe,*.bat,*.ps1,*.psm1,*.cmd | 
      %{$_.FullName} | Add-Content $store
  }
}
