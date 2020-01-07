
param(
    [int]$SampleSize = 500,
    [string[]]$Options = @(),
    [string[]]$Files = @(".\instance-generation\instance-generator.lp"),
    [string]$OutFile = ".\instance.lp"
)

clingo $SampleSize --outf=2 --warn none @Options @Files `
    | python.exe .\instance-generation\random-instance.py `
    | Out-File -Encoding ascii $OutFile
