System.IO.File.WriteAllText(
    @".\UserData.fs",
    """[<AutoOpen>]
module UserData

let ClientId     = ""
let ClientSecret = ""
let RefreshToken = ""
"""
)
