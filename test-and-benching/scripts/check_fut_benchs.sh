#!/bin/bash

OLD_FUTHARK="./futhark-old"
NEW_FUTHARK="./futhark-new_refactor"
BENCHMARK_DIR="./futhark-benchmarks"
OUTPUT_DIR="./.check_fut_benchs_out/typecheck_logs_$(date +%Y%m%d_%H%M%S)"

echo "Checking configuration..."
if [ ! -x "$OLD_FUTHARK" ]; then
    echo "Error: Old Futhark binary not found or not executable at '$OLD_FUTHARK'"
    exit 1
fi
if [ ! -x "$NEW_FUTHARK" ]; then
    echo "Error: New Futhark binary not found or not executable at '$NEW_FUTHARK'"
    exit 1
fi
if [ ! -d "$BENCHMARK_DIR" ]; then
    echo "Error: Benchmark directory not found at '$BENCHMARK_DIR'"
    exit 1
fi
echo "Configuration OK."
echo "Old Futhark: $OLD_FUTHARK"
echo "New Futhark: $NEW_FUTHARK"
echo "Benchmarks:  $BENCHMARK_DIR"
echo "Output logs: $OUTPUT_DIR"
echo "----------------------------------------"


mkdir -p "$OUTPUT_DIR"
if [ $? -ne 0 ]; then
    echo "Error: Could not create output directory '$OUTPUT_DIR'"
    exit 1
fi
echo "Created output directory: $OUTPUT_DIR"

diff_count=0
processed_count=0
error_count=0


find "$BENCHMARK_DIR" -name "*.fut" -type f -print0 | while IFS= read -r -d $'\0' fut_file; do
    ((processed_count++))
    printf "\r\033[KProcessing [%s]: %s" "$processed_count" "$fut_file" # Use \r for progress overwrite and \033[K for clearing line

    relative_path="${fut_file#$BENCHMARK_DIR/}"
    relative_path="${relative_path#./}"
    safe_filename="${relative_path//\//_}"
    safe_filename="${safe_filename%.fut}"

    output_file_old="${OUTPUT_DIR}/${safe_filename}.old.log"
    output_file_new="${OUTPUT_DIR}/${safe_filename}.new.log"

    FUTHARK_COMPILER_DEBUGGING=0 FUTHARK_LOG_TYSOLVE=0 "$OLD_FUTHARK" check "$fut_file" > "$output_file_old" 2>&1
    old_exit_code=$?

    FUTHARK_COMPILER_DEBUGGING=0 FUTHARK_LOG_TYSOLVE=0 "$NEW_FUTHARK" check "$fut_file" > "$output_file_new" 2>&1
    new_exit_code=$?

    if [ "$old_exit_code" -ne 0 ] || [ "$new_exit_code" -ne 0 ]; then
        if [ "$old_exit_code" -ne "$new_exit_code" ]; then
             echo -e "\n -> Exit code mismatch for: $fut_file"
             echo "     Old exit: $old_exit_code, New exit: $new_exit_code"
             echo "     Logs: $output_file_old , $output_file_new"
             ((diff_count++))
        fi
    else
        diff -q "$output_file_old" "$output_file_new" > /dev/null 2>&1
        diff_exit_code=$?

        if [ "$diff_exit_code" -ne 0 ]; then
            echo -e "\n -> Output difference detected for: $fut_file"
            echo "     (Both exited successfully)"
            echo "     Compare outputs:"
            echo "       diff \"$output_file_old\" \"$output_file_new\""
            ((diff_count++))
        else
            # remove log files if they are identical
            rm "$output_file_old" "$output_file_new"
        fi
    fi

done

echo

echo "============================================"
echo "Comparison finished."
echo "Processed $processed_count files."
if [ "$diff_count" -eq 0 ]; then
    echo "Success: All processed files produced identical type checker output and exit codes."
else
    echo "Warning: Found $diff_count file(s) with differences in output or exit code."
    echo "Please review the differences noted above."
    echo "All log files are stored in: $OUTPUT_DIR"
    echo "You can use 'diff <file>.old.log <file>.new.log' to inspect content differences."
fi
echo "============================================"

exit $diff_count