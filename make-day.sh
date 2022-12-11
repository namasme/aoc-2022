DAY_NUMBER="${1}"

# Create files witht the right structure

mkdir "src/Day${DAY_NUMBER}"
gsed "s/{N}/${DAY_NUMBER}/g" day-template/src/First.hs > "src/Day${DAY_NUMBER}/First.hs"
gsed "s/{N}/${DAY_NUMBER}/g" day-template/src/Second.hs > "src/Day${DAY_NUMBER}/Second.hs"

mkdir "test/Day${DAY_NUMBER}"
gsed "s/{N}/${DAY_NUMBER}/g" day-template/test/FirstSpec.hs > "test/Day${DAY_NUMBER}/FirstSpec.hs"
gsed "s/{N}/${DAY_NUMBER}/g" day-template/test/SecondSpec.hs > "test/Day${DAY_NUMBER}/SecondSpec.hs"

mkdir "data/Day${DAY_NUMBER}"

# Download input file (shout-out to smores56 for this)

SESSION_TOKEN="$(cat .session)"
YEAR="2022"
URL="https://adventofcode.com/${YEAR}/day/${DAY_NUMBER}/input"

curl "${URL}" --cookie "session=${SESSION_TOKEN}" \
    --silent --max-time 10 > "data/Day${DAY_NUMBER}/input"
