let %memo rec hodstadterFemale n =
    if n = 0
    then 1
    else n - hodstadterMale (hodstadterFemale (n - 1))
    and hodstadterMale n =
    if n = 0
    then 0
    else n - hodstadterFemale (hodstadterMale (n - 1))
