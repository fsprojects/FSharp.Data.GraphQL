$curr_path = (Get-Item (Split-Path -parent $MyInvocation.MyCommand.Definition)).Parent.FullName
$db_path = $curr_path + "\ml-latest"
Write-Host "Checking if path ""$db_path"" exists..."
If (!(Test-Path $db_path))
{
    Write-Host "Path not found. Downloading database files..."
    $clnt = new-object System.Net.WebClient
    $url = "http://files.grouplens.org/datasets/movielens/ml-latest.zip"
    $file = $curr_path + "\ml-latest.zip"
    $clnt.DownloadFile($url,$file)

    Write-Host "Download finished. Unzipping database files..."
    $shell_app=new-object -com shell.application 
    $zip_file = $shell_app.namespace($file)
    $destination = $shell_app.namespace($curr_path) 
    $destination.CopyHere($zip_file.items())

    Write-Host "Unzipping done. Removing zip file..."
    Remove-Item -Path $file
    Write-Host "Database files successfuly downloaded."
}
Else
{
    Write-Host "Path already exists. Skipping download of database."
}