#! /usr/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

set -ueo pipefail

syntax() {
   echo "Compare vanilla vs goto_def Cxx Glass snpashots"
   echo "  compare-snapshot [OPTIONS] -- revision file"
   echo ""
   echo "OPTIONS"
   echo "  --compare-with-prod=DB : compare with the snapshot produced for a given prod DB"
   echo "  --declaration-workflow-id=ID : use the given skycastle workflow id on the decl side"
   echo "  --definition-workflow-id=ID : use the given skycastle workflow id on the def side"
   echo ""
   echo "example"
   echo "  compare-snapshot e6043b38bfbf4d0ce4c6915df36198129b0a79a4 fbcode/monitoring/events/event_store/decoration/event_operation_consumer/EventOperationHandler.cpp"
   exit 1
}

trap syntax EXIT
options=$(getopt -q -o h --long help,compare-with-prod:,definition-workflow-id:,declaration-workflow-id: -- "$@")
eval set -- "$options";

COMPARE_WITH_PROD=0
PROD_DB=fbsource/$(glean db-latest fbsource)
gotoDefId=0
gotoDeclId=0

while [ $# -gt 0 ]; do
  case $1 in
    -h | --help) syntax ;;
    --compare-with-prod) COMPARE_WITH_PROD=1; PROD_DB=$2; shift 2;;
    --definition-workflow-id) gotoDefId=$2; shift 2;;
    --declaration-workflow-id) gotoDeclId=$2; shift 2;;
    --) shift; break;;
  esac
done
trap '' EXIT

REV=$1
shift
PREV=$(hg id -r ${REV}^1)
FILE=$1
shift
arc=/usr/local/bin/arc

scheduleJob() {
$arc skycastle schedule //tools/skycastle/workflows3/glean/diff_sketch.sky:snapshot-clang \
--repository-state=fbsource=$REV \
'--flag' "//tools/skycastle/lib2/glean/diff/core.sky:base_rev=$PREV" \
'--flag' '//tools/skycastle/lib2/glean/flags.sky:local=true'   \
 --flag "//tools/skycastle/lib2/glean/diff/glass.sky:upload_glass_snapshot=False" \
'--flag' '//tools/skycastle/lib2/glean/diff/core.sky:repo=fbsource'  \
'--flag' '//tools/skycastle/lib2/glean/diff/fbsource_cxx.sky:goto_def=$1' \
--result-format=json 2>&1 | \
sed -n 's/^.*"workflow_run_id": "\([0-9]*\)".*$/\1/p'
}

compute_from_prod() {
    tempFolder=$(mktemp -d)
    buck2 run @mode/opt glean/glass:glass-snapshot -- --db "$PROD_DB" --output "$tempFolder" fbsource/$FILE
    echo "$tempFolder"
}

allWorkflows=()
if [[ $gotoDefId -eq 0 ]]; then
    gotoDefId=$(scheduleJob True)
    allWorkflows+=( "$gotoDefId" )
fi
if [[ $gotoDeclId -eq 0 && $COMPARE_WITH_PROD -eq 0 ]]; then
    gotoDeclId=$(scheduleJob False)
    allWorkflows+=( "$gotoDeclId" )
fi

for workflow in "${allWorkflows[@]}" ; do
    while :; do
        status=$($arc skycastle workflow-run get-status "$workflow" --log-level=ERROR 2>&1 \
                  | grep -o -e "PENDING\|IN_PROGRESS\|SUCCESS\|FAILURE")
        echo "$workflow: $status"
        case $status in
        PENDING)
            sleep 10;;
        IN_PROGRESS)
            sleep 10;;
        *)
            break;;
        esac
    done
done

filter_and_pastry() {
    # if $1 is prod then do one thing, else do another
  buck2 run @mode/opt glean/glass:glass-snapshot -- --print-snapshot $1 /$FILE \
  | jq '.queries | .[0] | {"request": .request, "result": {"references": .result.references | map({"sym":.sym, "range": .range, "target": .target.filepath}) | sort_by(.range.lineBegin, .range.columnBegin) }}' \
  | pastry \
  | grep -oP "(?<=^P)[0-9]*"
}

retrieve_cas_snapshot(){
    rm -rf /tmp/$1

    $arc skycastle workflow-run get-action-artifact \
        --workflow-run-id "$1" -r "Build and store glass snapshot: after"  \
        -o --output-dir /tmp/"$1" \
        2> /dev/null
    find /tmp/$1 -name snapshots
}
gotoDefOutputs=$(retrieve_cas_snapshot $gotoDefId)

if [[ "$COMPARE_WITH_PROD" -eq 0 ]]; then
    gotoDeclOutputs=$(retrieve_cas_snapshot $gotoDeclId)
else
    gotoDeclOutputs=$(compute_from_prod)
fi

gotoDefPastry=$(filter_and_pastry $gotoDefOutputs/$FILE)
gotoDeclPastry=$(filter_and_pastry $gotoDeclOutputs/$FILE)

diff -u <(pastry P"$gotoDeclPastry") <(pastry P"$gotoDefPastry") || true

echo "https://www.internalfb.com/intern/diffing/?before_paste_number=$gotoDeclPastry&after_paste_number=$gotoDefPastry"
