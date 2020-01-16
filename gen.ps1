
param(
    [int]$Days = 5,
    [int]$TuPerDay = 8,
    [int]$SampleSize = 500,
    [string[]]$Options = @(),
    [string[]]$Files = @(".\instance-generation\instance-generator.lp"),
    [string]$OutFile = ".\instance.lp"
)

# Write constants to the output file
@(
    "#const days=$Days.",
    "#const tu_per_day=$TuPerDay."
) -join "`n" | Out-File -Encoding ascii $OutFile
# Write facts to the output file by getting a random model from the instance
# generator program
clingo $SampleSize  -c days=$Days -c tu_per_day=8 --outf=2 --warn none @Options @Files `
    | python.exe .\instance-generation\random-instance.py `
    | Out-File -Append -Encoding ascii $OutFile
