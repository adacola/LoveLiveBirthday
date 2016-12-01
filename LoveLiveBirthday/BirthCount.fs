module Adacola.LoveLiveBirthday.BirthCount

open System
open System.Text.RegularExpressions

/// e-Stat�́u�l�����ԓ��v �m�萔 �ۊǓ��v�\�i�񍐏���f�ڕ\�j �o���v�̔N���ꗗ�y�[�W����ړI�̔N���̃y�[�W��URI���擾
let tryGetYearUri (baseUri : Uri) content year =
    match Regex.Match(content, sprintf """<a\s+href="(?<uri>[^"]+)"[^>]*>\s*%d�N\s*</a>""" year, RegexOptions.Singleline) with
    | m when m.Success -> Uri(baseUri, m.Groups.["uri"].Value) |> Some
    | _ -> None

/// e-Stat�́u�l�����ԓ��v �m�萔 �ۊǓ��v�\�i�񍐏���f�ڕ\�j �o���v�̔N���y�[�W����o���N��������CSV��URI���擾
let tryGetDataUri (baseUri : Uri) content =
    match Regex.Match(content, """�o�����C�o���N�������E�o���̏ꏊ��.*?<a\s+href="(?<uri>[^"]+)"[^>]*>""", RegexOptions.Singleline) with
    | m when m.Success -> Uri(baseUri, m.Groups.["uri"].Value) |> Some
    | _ -> None
