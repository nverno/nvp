<# 
.SYNOPSIS
  Set user environment variable.
#>

param (
  [string] $envvar,
  [string] $value,
  [switch] $clobber = $false
)

$var = [Environment]::GetEnvironmentVariable($envvar, "User")

if ($var -eq $null -or $clobber) {
  # doesn't exist yet or clobber it if it does
  [Environment]::SetEnvironmentVariable($envvar, $value, "User")
} elseif ($var -notlike "*"+$value+"*") {
  # only add if its not there already
  [Environment]::SetEnvironmentVariable($envvar, "$var;$value", "User")
}
