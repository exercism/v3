<#
.SYNOPSIS
    Format all code.
.DESCRIPTION
    Format all code.
.EXAMPLE
    The example below will format the code of all exercises
    PS C:\> ./format-all.ps1
#>

dotnet tool restore

dotnet fantomas --recurse "exercises"