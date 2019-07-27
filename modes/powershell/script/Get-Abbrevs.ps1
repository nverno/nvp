<# 
.SYNOPSIS
  Create abbrevs.
#>

Param($outfile,
      $builtin = $true,
      $minLen = 5)

# pre-definined aliases
function Get-Builtins() {
  $ht = @{}
  Get-Alias | %{
    # don't add special character aliases
    if ($_.Name.Length -gt 1) {
      $ht.Add($_.Name, $_.Definition)
    }
  }
  $ht
}

# just use the capitalized letters as abbrev
function Format-Abbrev($name) {
  $($name -creplace '[:$%._a-z0-9-]', '').toLower()
}

# make abbrevs, use predefined if $builtin is $true
# only make abbrevs for names at least $minLen
function Get-Abbrevs($builtin = $true, $minLen = 5) {
  $ht = if ($builtin -eq $false) { @{} } else { Get-Builtins }
  Get-Command | %{
    if ($_.Name.Length -gt $minLen -and $_.Name -notIn $ht.Values) {
      $abbr = Format-Abbrev($_.Name)
      if ($abbr -ne "" -and $abbr -NotIn $ht.Keys) {
        $ht.Add($abbr, $_.Name)
      }
    }
  }
  $ht
}

# write abbrevs to file in lisp format
function Write-Abbrevs($outfile = $null, $builtin = $true, $minLen=5) {

  if ($outfile -eq $null) {
    $outfile = [System.IO.Path]::GetTempFileName()
  } else {
    $outfile = [System.IO.Path]::GetFullPath("$outfile")
  }
  
  $abbr = Get-Abbrevs $builtin, $minLen
  
  # define abbrev table
  "(define-abbrev-table 'powershell-mode-abbrev-table`n  '(" | 
    Add-Content $outfile -Encoding "UTF8"
  $abbr.keys | Sort-Object | %{
    "    (""$_"" ""$($abbr[$_])"" nil :system t)"
  } | Add-Content $outfile -Encoding "UTF8"
  $("  )`n  ""Powershell abbrevs.""`n" +
    "  :parents (list global-abbrev-table " +
    "fundamental-mode-abbrev-table))") | 
      Add-Content $outfile -Encoding "UTF8"
}

Write-Abbrevs $outfile $builtin $minLen
