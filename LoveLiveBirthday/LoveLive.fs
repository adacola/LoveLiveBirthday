module Adacola.LoveLiveBirthday.LoveLive

let μ's1stGradeYear = 1998<年度>
let μ's2ndGradeYear = 1997<年度>
let μ's3rdGradeYear = 1996<年度>
let aqours1stGradeYear = 2003<年度>
let aqours2ndGradeYear = 2002<年度>
let aqours3rdGradeYear = 2001<年度>

/// ラブライブ！のキャラと中の人の誕生日をランダムに取得
let getRandomBirthdays getμ'sActorBirthday getμ's1stGradeBirthday getμ's2ndGradeBirthday getμ's3rdGradeBirthday getAqoursActorBirthday getAqours1stGradeBirthday getAqours2ndGradeBirthday getAqours3rdGradeBirthday random =
    let getCharacterBirthdays allBirthdays random getBirthdayFuncs =
        ((allBirthdays, random), getBirthdayFuncs) ||> Seq.fold (fun (allBirthdays, random) getBirthdayFunc ->
            let birthdays, random = Birthday.generateCharactorRandomBirthdays getBirthdayFunc random 3 allBirthdays
            birthdays @ allBirthdays, random)
        
    let allBirthdays = []
    let μ'sActorBirtydays, random = Birthday.generateActorRandomBirthdays getμ'sActorBirthday random 9
    let allBirthdays = μ'sActorBirtydays @ allBirthdays
    let allBirthdays, random = [getμ's1stGradeBirthday; getμ's2ndGradeBirthday; getμ's3rdGradeBirthday] |> getCharacterBirthdays allBirthdays random
    let aqoursActorBirthdays, random = Birthday.generateActorRandomBirthdays getAqoursActorBirthday random 9
    let allBirthdays = aqoursActorBirthdays @ allBirthdays
    [getAqours1stGradeBirthday; getAqours2ndGradeBirthday; getAqours3rdGradeBirthday] |> getCharacterBirthdays allBirthdays random
