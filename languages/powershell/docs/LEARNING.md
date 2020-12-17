# Learning PowerShell

A great place to start learning PowerShell is at the PowerShell Github repository where it has a [learning section](https://github.com/PowerShell/PowerShell/tree/master/docs/learning-powershell).  This has a huge amount of information and links to a wide range of PowerShell topics.

PowerShell also has a very complete self documenting help system.  You can explore much of what PowerShell offers using the `Get-Help` cmdlet.  From within PowerShell, try:

``` PowerShell
Get-Help about
```

You will see about 3 pages of topics that can be explored.

Two further functions that will be of great use are:

* Get-Command
* Get-Member

Wanting to know what these do? Try `Get-Help Get-Command` and `Get-Help Get-Member` of course.

## PowerShell Verbs and Nouns

PowerShell follows quite a strict Verb-Noun for the naming of its cmdlets.  Initially this can be a jarring experience to seasoned programmers.  Whilst you can name your functions anything you like, becoming familiar with and adopting the standard verbs will improve your PowerShell coding.

To see a list of all the approved verbs, simply run:

``` PowerShell
Get-Verb
```

## More Topics 
https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/?view=powershell-6
