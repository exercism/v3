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

$conceptExercisesDir = Join-Path "exercises" "concept"
Get-ChildItem -Directory $conceptExercisesDir | ForEach { dotnet format -f $_.FullName }

$practiceExercisesDir = Join-Path "exercises" "practice"
Get-ChildItem -Directory $practiceExercisesDir | ForEach { dotnet format -f $_.FullName }
