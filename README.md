# google-drive-folder-uploader

## Build

Use F#. Install the [.NET SDK](https://dotnet.microsoft.com/download).

```
dotnet fsi init.fsx
```

Fill in the `UserData.fs` with the Google Drive API data.

```
dotnet publish -r win-x64 -p:PublishSingleFile=true --self-contained false
```
