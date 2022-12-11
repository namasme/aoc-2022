mkdir "src/Day${1}"
gsed "s/{N}/${1}/g" day-template/src/First.hs > "src/Day${1}/First.hs"
gsed "s/{N}/${1}/g" day-template/src/Second.hs > "src/Day${1}/Second.hs"

mkdir "test/Day${1}"
gsed "s/{N}/${1}/g" day-template/test/FirstSpec.hs > "test/Day${1}/FirstSpec.hs"
gsed "s/{N}/${1}/g" day-template/test/SecondSpec.hs > "test/Day${1}/SecondSpec.hs"

mkdir "data/Day${1}"
