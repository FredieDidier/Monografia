#!/bin/zsh
# Progress of the full rebuild. Run from the repository root: ./status.sh
cd "$(dirname "$0")"
DB=~/Library/CloudStorage/Dropbox/Education_and_Job_Loss_During_COVID_19
LOG=analysis/output/logs/run_all.log
[ -s analysis/output/logs/run_from07.log ] && LOG=analysis/output/logs/run_from07.log

echo "=============================================================="
date "+  %H:%M:%S"
echo "=============================================================="

echo "\n[1] DOWNLOAD          $(ls $DB/build/input/pnadc_quarters 2>/dev/null | wc -l | tr -d ' ')/57 quarters"

np=$(ls $DB/build/input/pnadc_panels/Panel_*.parquet 2>/dev/null | wc -l | tr -d ' ')
echo "[2] STAGE-3 PANELS    $np/13 rotation groups"
grep -E "^  group " $LOG 2>/dev/null | tail -1 | sed 's/^/      now: /'

[ -f $DB/build/output/main_data.parquet ] \
  && echo "[3] MAIN_DATA         done ($(du -h $DB/build/output/main_data.parquet | cut -f1))" \
  || echo "[3] MAIN_DATA         pending"

echo "\n[4] ANALYSIS"
if grep -q "STEP 4" $LOG 2>/dev/null; then
  grep -E "^\[.*\] (RUN|OK) " $LOG 2>/dev/null | tail -6 | sed 's/^/      /'
else
  echo "      not started yet"
fi

nfig=$(ls analysis/output/figures/ 2>/dev/null | grep -c "\.pdf$")
ntab=$(ls analysis/output/tables/ 2>/dev/null | grep -c "\.tex$")
echo "\n[5] TABLES  $ntab   FIGURES  $nfig/7   PDF  $([ -f latex/paper.pdf ] && echo yes || echo no)"

grep -E "ABORT|ALL DONE" $LOG 2>/dev/null | sed 's/^/\n>>> /'
