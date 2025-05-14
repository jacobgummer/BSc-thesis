#!/bin/bash

# --- Configuration ---
# Path to the old Futhark binary
OLD_FUTHARK="./futhark-old"
# Path to the new Futhark binary you are testing
NEW_FUTHARK="./futhark-new_refactor"
# Path to the directory containing the .fut benchmark files
BENCHMARK_DIR="./futhark-benchmarks"
# Directory where the output logs will be stored
OUTPUT_DIR="./.check_fut_benchs_out/typecheck_logs_$(date +%Y%m%d_%H%M%S)" # Unique dir per run
# --- End Configuration ---

# --- Sanity Checks ---
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

# --- Setup ---
mkdir -p "$OUTPUT_DIR"
if [ $? -ne 0 ]; then
    echo "Error: Could not create output directory '$OUTPUT_DIR'"
    exit 1
fi
echo "Created output directory: $OUTPUT_DIR"

diff_count=0
processed_count=0
error_count=0

# --- Main Processing Loop ---
# Use find with -print0 and read -d '' for robust handling of filenames
# containing spaces or special characters.
find "$BENCHMARK_DIR" -name "*.fut" -type f -print0 | while IFS= read -r -d $'\0' fut_file; do
    ((processed_count++))
    printf "\r\033[KProcessing [%s]: %s" "$processed_count" "$fut_file" # Use \r for progress overwrite and \033[K for clearing line

    # Create a safe filename for the log files based on the relative path
    # Remove the benchmark directory prefix potentially starting with ./
    relative_path="${fut_file#$BENCHMARK_DIR/}"
    relative_path="${relative_path#./}" # Handle case where BENCHMARK_DIR is '.'
    # Replace slashes with underscores to create a flat structure in OUTPUT_DIR
    safe_filename="${relative_path//\//_}"
    safe_filename="${safe_filename%.fut}" # Remove .fut extension for clarity

    output_file_old="${OUTPUT_DIR}/${safe_filename}.old.log"
    output_file_new="${OUTPUT_DIR}/${safe_filename}.new.log"

    # Run 'futhark check' with the OLD binary
    # Redirect both stdout and stderr (2>&1) to the log file
    FUTHARK_COMPILER_DEBUGGING=0 FUTHARK_LOG_TYSOLVE=0 "$OLD_FUTHARK" check "$fut_file" > "$output_file_old" 2>&1
    old_exit_code=$?

    # Run 'futhark check' with the NEW binary
    FUTHARK_COMPILER_DEBUGGING=0 FUTHARK_LOG_TYSOLVE=0 "$NEW_FUTHARK" check "$fut_file" > "$output_file_new" 2>&1
    new_exit_code=$?

    # Check if Futhark itself reported an error (non-zero exit code)
    if [ "$old_exit_code" -ne 0 ] || [ "$new_exit_code" -ne 0 ]; then
        # Check if ONLY one of them failed (potential regression/fix)
        if [ "$old_exit_code" -ne "$new_exit_code" ]; then
             echo -e "\n -> Exit code mismatch for: $fut_file" # Newline needed after \r
             echo "     Old exit: $old_exit_code, New exit: $new_exit_code"
             echo "     Logs: $output_file_old , $output_file_new"
             ((diff_count++))
        # Optional: Track files that failed with both versions if desired
        # else
        #    echo -e "\n -> Both versions failed for: $fut_file (Old: $old_exit_code, New: $new_exit_code)"
        fi
    # Both versions succeeded (exit code 0), now compare output
    else
        # Compare the output files using diff
        # diff -q returns 0 if files are identical, 1 if different, >1 on error.
        # We redirect diff's own output to /dev/null as we only care about the exit code.
        diff -q "$output_file_old" "$output_file_new" > /dev/null 2>&1
        diff_exit_code=$?

        if [ "$diff_exit_code" -ne 0 ]; then
            echo -e "\n -> Output difference detected for: $fut_file" # Newline needed after \r
            echo "     (Both exited successfully)"
            echo "     Compare outputs:"
            echo "       diff \"$output_file_old\" \"$output_file_new\""
            ((diff_count++))
        else
            # remove log files if they are identical to save space
            rm "$output_file_old" "$output_file_new"
        fi
    fi

done

# --- Clean up progress line ---
echo # Print a final newline to move past the progress indicator

# --- Summary ---
echo "============================================"
echo "Comparison finished."
echo "Processed $processed_count files."
if [ "$diff_count" -eq 0 ]; then
    echo "Success: All processed files produced identical type checker output and exit codes."
    # Optional: uncomment to remove the whole log directory if everything matched
    # echo "Removing empty log directory: $OUTPUT_DIR"
    # rm -rf "$OUTPUT_DIR"
else
    echo "Warning: Found $diff_count file(s) with differences in output or exit code."
    echo "Please review the differences noted above."
    echo "All log files are stored in: $OUTPUT_DIR"
    echo "You can use 'diff <file>.old.log <file>.new.log' to inspect content differences."
fi
echo "============================================"

exit $diff_count # Exit with 0 if no diffs, non-zero otherwise