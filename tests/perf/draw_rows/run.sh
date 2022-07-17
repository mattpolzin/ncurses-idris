#! /bin/bash

rm -rf ./lowlevel/build
rm -rf ./control/build

(cd lowlevel && idris2 --build lowlevel_rows.ipkg)
(cd control && idris2 --build control_rows.ipkg)

function run() {
  for i in {1..5}
  do
    cd "$1" && ./build/exec/$1 | sed 's/^.*\(0\..*\)$/\1/g' && cd ..
  done
}

lowlevel_perf="$(run lowlevel)"
control_perf="$(run control)"

lowlevel_avg=$(echo "$lowlevel_perf" | awk '{ total += $1; count++ } END { print total/count }')
control_avg=$(echo "$control_perf" | awk '{ total += $1; count++ } END { print total/count }')

echo "lowlevel avg: ${lowlevel_avg}s"
echo "control avg: ${control_avg}s"

