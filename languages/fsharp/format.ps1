<#
.SYNOPSIS
    Format the code of an exercise.
.DESCRIPTION
    Format the code. of an exercise
.PARAMETER Slug
    The slug of the exercise to format (optional)
.EXAMPLE
    The example below will format the code of the "basics" exercise
    PS C:\> ./format.ps1 basics
#>

param (
    [Parameter(Position = 0, Mandatory = $true)]
    [string]$Slug
)

dotnet tool restore

$conceptExerciseDir = Join-Path "exercises" "concept" $slug
$practiceExerciseDIr = Join-Path "exercises" "practice" $slug

if (Test-Path $conceptExerciseDir -PathType Container) {
    dotnet fantomas $conceptExerciseDir
} elseif (Test-Path $practiceExerciseDIr -PathType Container) {
    dotnet fantomas $practiceExerciseDIr
} else {
    Write-Error "Unknown slug"
}

