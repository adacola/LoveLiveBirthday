module Adacola.LoveLiveBirthday.BirthCount

open System
open System.Text.RegularExpressions

/// e-Statの「人口動態統計 確定数 保管統計表（報告書非掲載表） 出生」の年次一覧ページから目的の年次のページのURIを取得
let tryGetYearUri (baseUri : Uri) content year =
    match Regex.Match(content, sprintf """<a\s+href="(?<uri>[^"]+)"[^>]*>\s*%d年\s*</a>""" year, RegexOptions.Singleline) with
    | m when m.Success -> Uri(baseUri, m.Groups.["uri"].Value) |> Some
    | _ -> None

/// e-Statの「人口動態統計 確定数 保管統計表（報告書非掲載表） 出生」の年次ページから出生年月日時のCSVのURIを取得
let tryGetDataUri (baseUri : Uri) content =
    match Regex.Match(content, """出生数，出生年月日時・出生の場所別.*?<a\s+href="(?<uri>[^"]+)"[^>]*>""", RegexOptions.Singleline) with
    | m when m.Success -> Uri(baseUri, m.Groups.["uri"].Value) |> Some
    | _ -> None
