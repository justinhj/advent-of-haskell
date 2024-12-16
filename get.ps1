param(
    [Parameter(Mandatory=$true)]
    [int]$Day
)

# Read year and cookie from files
$Year = Get-Content ".year"
$Cookie = Get-Content ".cookie"

$InputDir = "examples/$Day"
New-Item -ItemType Directory -Force -Path $InputDir

# Create a web request session
$session = New-Object Microsoft.PowerShell.Commands.WebRequestSession
$session.Cookies.Add((New-Object System.Net.Cookie("session", $Cookie, "/", "adventofcode.com")))

$url = "https://adventofcode.com/$Year/day/$Day/input"

$OutputFile = "$InputDir/input"
$response = Invoke-WebRequest -UseBasicParsing -Uri $url -WebSession $session -OutFile $OutputFile
