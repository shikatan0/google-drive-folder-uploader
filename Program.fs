/// 実行ファイルのディレクトリの絶対パス
let Location = System.AppDomain.CurrentDomain.BaseDirectory.TrimEnd('\\')
/// 対象フォルダ一覧
let TargetList = new ResizeArray<string>()
/// 除外ファイル一覧
let IgnoreList = new ResizeArray<string>()
/// 現在の最終更新日を表すTSV一覧
let CurrentLastupdatedContent = new ResizeArray<string>()
/// 辞書: 最終更新日時 の値
type LastupdatedValue = {
    Src : string
    Id  : string
    Time: string
}
/// 辞書: 最終更新日時
let LastupdatedDictionary = new System.Collections.Generic.Dictionary<string, LastupdatedValue>()

/// 絶対パス-GoogleDriveID-最終更新日時
let CreateLastupdatedLine (fileFullPath: string) (googleDriveId: string) : string =
    let time = System.IO.File.GetLastWriteTime(fileFullPath).ToString()
    $"{fileFullPath}\t{googleDriveId}\t{time}"

module Pattern =
    /// 正規表現における特殊文字をエスケープする
    let private escape (text: string) : string =
        text
            .Replace("\\", "\\\\")
            .Replace("/", "\\\\")
            .Replace(".", "\\.")

    /// パターンにおける特殊文字を正規表現に置換する
    let private replace (text: string) : string =
        text
            .Replace("*", "[^\\\\]*?")

    /// パターンを表す正規表現の文字列に変換する
    let private convert (text: string) : string =
        $"^.*\\\\{text}$" //「^.*\\xx$」

    /// 文字列から正規表現を生成する
    let private regex (text: string) : System.Text.RegularExpressions.Regex =
        System.Text.RegularExpressions.Regex(
            text,
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    /// パターン文字列から正規表現を生成する
    let ToRegex = escape >> replace >> convert >> regex

module Ignore =
    /// 除外ファイルを表す正規表現の一覧
    let private ignoreRegexList = IgnoreList |> Seq.map Pattern.ToRegex

    /// regex.f(path) → f path regex
    let private ignoreIsMatch (path: string) (regex: System.Text.RegularExpressions.Regex) =
        regex.IsMatch(path)

    /// 除外ファイル一覧に存在するか
    let Has (path: string) =
        (ignoreRegexList |> Seq.tryFind (ignoreIsMatch path)).IsSome

/// パスから名前を取得する
let PathGetName (path: string) : string =
    let splinters = path.Split('\\')
    splinters.[splinters.Length - 1]

module Backoff =
    let private randomNumberGenerator = System.Random()

    let Exponential (retryCount: int) =
        let currentWaitMilliseconds =
            min
            <| (2. ** float retryCount) * 1000.
            <| 32000.
        let jitterMilliseconds = int currentWaitMilliseconds + randomNumberGenerator.Next(1, 1001)
        System.Threading.Tasks.Task.Delay(jitterMilliseconds).Wait()

module Http =
    let private client = new System.Net.Http.HttpClient()
    client.Timeout <- System.Threading.Timeout.InfiniteTimeSpan

    let Send (request: System.Net.Http.HttpRequestMessage) =
        client.SendAsync(request).Result

module StatusCode =
    let private retryList =
        Set [
            System.Net.HttpStatusCode.TooManyRequests
            System.Net.HttpStatusCode.InternalServerError
            System.Net.HttpStatusCode.BadGateway
            System.Net.HttpStatusCode.ServiceUnavailable
            System.Net.HttpStatusCode.GatewayTimeout
        ]

    let IsRetry (statusCode: System.Net.HttpStatusCode) : bool =
        retryList |> Set.contains statusCode

module AccessToken =
    let private regex =
        System.Text.RegularExpressions.Regex(
            "\"access_token\" *: *\"(.*?)\"",
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    let private getExecute () : System.Net.Http.HttpResponseMessage =
        let request =
            new System.Net.Http.HttpRequestMessage(
                System.Net.Http.HttpMethod.Post,
                "https://www.googleapis.com/oauth2/v4/token",
                Content =
                    new System.Net.Http.StringContent(
                        $"""{{"client_id":"{ClientId}","client_secret":"{ClientSecret}","grant_type":"refresh_token","refresh_token":"{RefreshToken}"}}""",
                        System.Text.Encoding.UTF8,
                        @"application/json"
                    )
            )
        Http.Send request

    let rec GetTry (retryCount: int) : string =
        let res = getExecute()
        // 成功
        if res.IsSuccessStatusCode then
            regex.Match(res.Content.ReadAsStringAsync().Result).Groups.[1].Value
        // バックオフで解決できる可能性のある失敗
        elif res.StatusCode |> StatusCode.IsRetry then
            match retryCount with
            | 9 -> failwith "試行回数上限 AccessToken.GetTry Backoff"
            | _ -> Backoff.Exponential retryCount
            GetTry (retryCount + 1)
        // 失敗
        else
            failwith "AccessToken.GetTry"

    /// アクセストークン
    let mutable Value : string = GetTry 0

    /// アクセストークンを更新する
    let RefreshTry () =
        Value <- GetTry 0

module GetId =
    let private execute () =
        let request =
            new System.Net.Http.HttpRequestMessage(
                System.Net.Http.HttpMethod.Get,
                "https://www.googleapis.com/drive/v3/files/generateIds?count=1&fields=ids"
            )
        request.Headers.Add("Authorization", $"Bearer {AccessToken.Value}")
        Http.Send request

    let private regex =
        System.Text.RegularExpressions.Regex(
            "\"ids\" *: *\\[[ \\r\\n]*\"(.*?)\"",
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    let rec Try (retryCount: int) : string =
        let res = execute()
        // 成功
        if res.IsSuccessStatusCode then
            let json = res.Content.ReadAsStringAsync().Result
            regex.Match(json).Groups.[1].Value
        // 無効なアクセストークン
        elif res.StatusCode = System.Net.HttpStatusCode.Unauthorized then
            AccessToken.RefreshTry()
            Try retryCount
        // バックオフで解決できる可能性のある失敗
        elif res.StatusCode |> StatusCode.IsRetry then
            match retryCount with
            | 9 -> failwith "試行回数上限 GoogleDriveGetIdTryBackoff"
            | _ -> Backoff.Exponential retryCount
            Try (retryCount + 1)
        // 失敗
        else
            failwith "GoogleDriveGetIdTry"

module Dir =
    let private create (id: string) (parentId: string) (dirFullPath: string) =
        let dirName = PathGetName dirFullPath
        let metadata =
            new System.Net.Http.StringContent(
                $"""{{"id":"{id}","mimeType":"application/vnd.google-apps.folder","name":"{dirName}","parents":["{parentId}"]}}""",
                System.Text.Encoding.UTF8,
                @"application/json"
            )
        let request =
            new System.Net.Http.HttpRequestMessage(
                System.Net.Http.HttpMethod.Post,
                "https://www.googleapis.com/drive/v3/files?uploadType=multipart",
                Content = metadata
            )
        request.Headers.Add("Authorization", $"Bearer {AccessToken.Value}")
        Http.Send request

    let rec CreateTry (id: string) (parentId: string) (dirFullPath: string) (retryCount: int) : unit =
        let res = create id parentId dirFullPath
        // 成功
        if res.IsSuccessStatusCode then
            ()
        // 無効なアクセストークン
        elif res.StatusCode = System.Net.HttpStatusCode.Unauthorized then
            AccessToken.RefreshTry()
            CreateTry id parentId dirFullPath retryCount
        // バックオフで解決できる可能性のある失敗
        elif res.StatusCode |> StatusCode.IsRetry then
            match retryCount with
            | 9 -> failwith "試行回数上限 Dir.CreateTry Backoff"
            | _ -> Backoff.Exponential retryCount
            CreateTry id parentId dirFullPath (retryCount + 1)
        // 失敗
        else
            failwith "Dir.CreateTry"

module File =
    let private create (id: string) (parentId: string) (fileFullPath: string) =
        let fileName = PathGetName fileFullPath
        let metadata =
            new System.Net.Http.StringContent(
                $"""{{"id":"{id}","name":"{fileName}","parents":["{parentId}"]}}""",
                System.Text.Encoding.UTF8,
                @"application/json"
            )
        use stream = new System.IO.FileStream(fileFullPath, System.IO.FileMode.Open)
        let media = new System.Net.Http.StreamContent(stream)
        let content = new System.Net.Http.MultipartFormDataContent()
        content.Add(metadata, "Metadata")
        content.Add(media, "Media")
        let request =
            new System.Net.Http.HttpRequestMessage(
                System.Net.Http.HttpMethod.Post,
                "https://www.googleapis.com/upload/drive/v3/files?uploadType=multipart",
                Content = content
            )
        request.Headers.Add("Authorization", $"Bearer {AccessToken.Value}")
        Http.Send request

    let rec CreateTry (id: string) (parentId: string) (fileFullPath: string) (retryCount: int) : unit =
        let res = create id parentId fileFullPath
        // 成功
        if res.IsSuccessStatusCode then
            ()
        // 無効なアクセストークン
        elif res.StatusCode = System.Net.HttpStatusCode.Unauthorized then
            AccessToken.RefreshTry()
            CreateTry id parentId fileFullPath retryCount
        // バックオフで解決できる可能性のある失敗
        elif res.StatusCode |> StatusCode.IsRetry then
            match retryCount with
            | 9 -> failwith "試行回数上限 File.CreateTry Backoff"
            | _ -> Backoff.Exponential retryCount
            CreateTry id parentId fileFullPath (retryCount + 1)
        // 失敗
        else
            failwith "File.CreateTry"

    let private update (id: string) (fileFullPath: string) =
        let metadata =
            new System.Net.Http.StringContent(
                "{}",
                System.Text.Encoding.UTF8,
                @"application/json"
            )
        use stream = new System.IO.FileStream(fileFullPath, System.IO.FileMode.Open)
        let media = new System.Net.Http.StreamContent(stream)
        let content = new System.Net.Http.MultipartFormDataContent()
        content.Add(metadata, "Metadata")
        content.Add(media, "Media")
        let request =
            new System.Net.Http.HttpRequestMessage(
                System.Net.Http.HttpMethod.Patch,
                $"https://www.googleapis.com/upload/drive/v3/files/{id}?uploadType=multipart",
                Content = content
            )
        request.Headers.Add("Authorization", $"Bearer {AccessToken.Value}")
        Http.Send request

    let rec UpdateTry (id: string) (fileFullPath: string) (retryCount: int) : unit =
        let res = update id fileFullPath
        // 成功
        if res.IsSuccessStatusCode then
            ()
        // 無効なアクセストークン
        elif res.StatusCode = System.Net.HttpStatusCode.Unauthorized then
            AccessToken.RefreshTry()
            UpdateTry id fileFullPath retryCount
        // バックオフで解決できる可能性のある失敗
        elif res.StatusCode |> StatusCode.IsRetry then
            match retryCount with
            | 9 -> failwith "試行回数上限 File.UpdateTry"
            | _ -> Backoff.Exponential retryCount
            UpdateTry id fileFullPath (retryCount + 1)
        // 失敗
        else
            failwith "File.UpdateTry"

    let private delete (id: string) =
        let request =
            new System.Net.Http.HttpRequestMessage(
                System.Net.Http.HttpMethod.Delete,
                $"https://www.googleapis.com/drive/v3/files/{id}"
            )
        request.Headers.Add("Authorization", $"Bearer {AccessToken.Value}")
        Http.Send request

    let rec DeleteTry (id: string) (retryCount: int) : unit =
        let res = delete id
        // 成功
        if res.StatusCode = System.Net.HttpStatusCode.NoContent then
            ()
        // 無効なアクセストークン
        elif res.StatusCode = System.Net.HttpStatusCode.Unauthorized then
            AccessToken.RefreshTry()
            DeleteTry id retryCount
        // バックオフで解決できる可能性のある失敗
        elif res.StatusCode |> StatusCode.IsRetry then
            match retryCount with
            | 9 -> failwith "試行回数上限 File.DeleteTry Backoff"
            | _ -> Backoff.Exponential retryCount
            DeleteTry id (retryCount + 1)
        // 失敗
        else
            failwith "File.DeleteTry"

module Upload =
    let private fileExecute (dirId: string) (fileFullPath: string) : unit =
        // ファイルが除外ファイルに含まれていない場合、
        if not <| Ignore.Has fileFullPath then
            // ファイルが前回のバックアップ時に存在する場合、
            if LastupdatedDictionary.ContainsKey(fileFullPath) then
                // ファイルが更新されている場合、
                if LastupdatedDictionary.[fileFullPath].Time <> System.IO.File.GetLastWriteTime(fileFullPath).ToString() then
                    let fileId = LastupdatedDictionary.[fileFullPath].Id
                    File.UpdateTry fileId fileFullPath 0
                    CurrentLastupdatedContent.Add(CreateLastupdatedLine fileFullPath fileId)
                // ファイルが更新されていない場合、
                else
                    let fileData = LastupdatedDictionary.[fileFullPath]
                    CurrentLastupdatedContent.Add(fileData.Src)
            // ファイルが前回のバックアップ時に存在しない場合、
            else
                let fileId = GetId.Try 0
                File.CreateTry fileId dirId fileFullPath 0
                CurrentLastupdatedContent.Add(CreateLastupdatedLine fileFullPath fileId)
            // 完了
            ignore <| LastupdatedDictionary.Remove(fileFullPath)

    let rec private dirExecute (parentId: string) (dirFullPath: string) : unit =
        // ディレクトリが除外ファイルに含まれていない場合、
        if not <| Ignore.Has dirFullPath then
            /// GoogleDrive: ディレクトリのID
            let dirId =
                // ディレクトリが前回のバックアップ時に存在する場合、
                if LastupdatedDictionary.ContainsKey(dirFullPath) then
                    let dirData = LastupdatedDictionary.[dirFullPath]
                    let id = dirData.Id
                    CurrentLastupdatedContent.Add(dirData.Src)
                    id
                // ディレクトリが前回のバックアップ時に存在しない場合、
                else
                    let id = GetId.Try 0
                    Dir.CreateTry id parentId dirFullPath 0
                    CurrentLastupdatedContent.Add(CreateLastupdatedLine dirFullPath id)
                    id
            // ファイル
            let fileFullPaths = System.IO.Directory.EnumerateFiles dirFullPath
            for fp in fileFullPaths do
                fileExecute dirId fp
            // フォルダ
            let dirFullPaths = System.IO.Directory.EnumerateDirectories dirFullPath
            for dp in dirFullPaths do
                dirExecute dirId dp
            // 完了
            ignore <| LastupdatedDictionary.Remove(dirFullPath)

    /// バックアップを実行する
    let Execute () =
        TargetList |> Seq.iter (dirExecute "root")

/// 設定ファイルの内容を読み込む
let ImportSettings () =
    // 対象ディレクトリ
    let targetStreamReader = new System.IO.StreamReader($@"{Location}\settings\target")
    while not targetStreamReader.EndOfStream do
        let lineText = targetStreamReader.ReadLine()
        if lineText <> "" then
            TargetList.Add(lineText)
    targetStreamReader.Dispose()
    // 除外ファイル
    let ignoretargetStreamReader = new System.IO.StreamReader($@"{Location}\settings\ignore")
    while not ignoretargetStreamReader.EndOfStream do
        let lineText = ignoretargetStreamReader.ReadLine()
        if lineText <> "" then
            IgnoreList.Add(lineText)
    ignoretargetStreamReader.Dispose()

module Lastupdated =
    let private filePath = $@"{Location}\lastupdated.tsv"

    let private lineRegex =
        System.Text.RegularExpressions.Regex(
            "^(.*?)\t(.*?)\t(.*?)$",
            System.Text.RegularExpressions.RegexOptions.Compiled
        )

    /// テキストファイルの内容を辞書に変換する
    let Import () =
        if System.IO.File.Exists(filePath) then
            let streamReader = new System.IO.StreamReader(filePath)
            while not streamReader.EndOfStream do
                let line = streamReader.ReadLine()
                if line <> "" then
                    let ms = lineRegex.Matches line
                    for m in ms do
                        let path = m.Groups.[1].Value
                        let id   = m.Groups.[2].Value
                        let time = m.Groups.[3].Value
                        LastupdatedDictionary.Add(path, {Src = line; Id = id; Time = time})
            streamReader.Dispose()

    /// 最終更新日を記録したファイルを作成する
    let Export () : unit =
        let text = System.String.Join("\r\n", CurrentLastupdatedContent)
        System.IO.File.WriteAllText(filePath, text)

/// 前回のバックアップ後に削除(名前変更)されたファイルをGoogleDriveから削除する
let DeleteNonexistent () : unit =
    for data in LastupdatedDictionary do
        File.DeleteTry data.Value.Id 0

/// スリープ
[<System.Runtime.InteropServices.DllImport("PowrProf.dll", SetLastError = true)>]
extern bool SetSuspendState(bool bHibernate, bool bForce, bool bWakeupEventsDisabled)

[<EntryPointAttribute>]
do
    // 前
    ImportSettings()
    Lastupdated.Import()
    // 本体
    Upload.Execute()
    // 後
    Lastupdated.Export()
    DeleteNonexistent()
    // 終了
    SetSuspendState(false, false, false) |> ignore
