      
#!/bin/bash

# Exit on error, treat unset variables as an error, and ensure pipe failures are caught.
# 'set -e' means the script will exit immediately if a command exits with a non-zero status.
# 'set -u' means it will exit if it tries to use an uninitialized variable.
# 'set -o pipefail' means a pipeline will exit with the status of the last command to exit with a non-zero status,
# or zero if all commands in the pipeline exit successfully.
#set -e
#set -u
#set -o pipefail

# --- Configuration ---
# These variables can be overridden by setting them as environment variables before running the script.
# Example: NUM_DRY_RUNS=5 ./your_script_name.sh

# Number of dry (warmup) runs for hyperfine
NUM_DRY_RUNS=${NUM_DRY_RUNS:-3}
# Number of actual (measured) runs for hyperfine
NUM_ACTUAL_RUNS=${NUM_ACTUAL_RUNS:-15}
# Location of the 'new' Futhark binary
NEW_FUTHARK_LOC=${NEW_FUTHARK_LOC:-"../binaries/futhark-new-2.0"}
# Location of the 'old' Futhark binary
OLD_FUTHARK_LOC=${OLD_FUTHARK_LOC:-"../binaries/futhark-automap-commit"}
# Location of the hyperfine binary
HYPERFINE_LOC=${HYPERFINE_LOC:-"hyperfine"} # Assumed to be in PATH by default
# User-specified part of the output directory path.
# Example: USER_SPECIFIED_OUTPUT_DIR_PREFIX="./my_project_benchmarks"
# This will result in "./my_project_benchmarks/benchmark_out_YYYYMMDD_HHMMSS"
# Default: "." results in "./benchmark_out_YYYYMMDD_HHMMSS"
USER_SPECIFIED_OUTPUT_DIR_PREFIX=${USER_SPECIFIED_OUTPUT_DIR_PREFIX:-"../results/hyperfine_benching_out"}
# Directory containing the .fut files to be tested
TEST_FILES_DIR=${TEST_FILES_DIR:-"./hyperfine_benching_in"}

# --- Dynamic Configuration & Script Globals ---
# These are determined at runtime or accumulate results during the script's execution.

# Output directory with a unique timestamp to avoid overwriting previous results
OUTPUT_DIR="${USER_SPECIFIED_OUTPUT_DIR_PREFIX}/benchmark_out_$(date +%Y%m%d_%H%M%S)"
# Overall log file for the entire script run, stored within the unique OUTPUT_DIR
OUTPUT_LOG="${OUTPUT_DIR}/benchmark_result.log"

# Stores the value of the 'longest' run of a given hyperfine run.
# This is the maximum of (max(mean_new_futhark_time, mean_old_futhark_time)) across all files.
MAX_TIME=0.0
# The .fut file that produced the MAX_TIME
MAX_TIME_FILE=""
# Counter for wins by the new Futhark binary
NEW_FUTHARK_WINS=0
# Counter for wins by the old Futhark binary
OLD_FUTHARK_WINS=0
# Count of .fut files processed (or attempted)
PROCESSED_FILES_COUNT=0
# Count of benchmarks that failed (e.g., hyperfine error or issues parsing its results)
FAILED_BENCHMARK_COUNT=0
# Total .fut files found to benchmark
TOTAL_FILES_FOUND=0

# --- Helper Functions ---

# log_message: Prints a message to both the console (stdout) and the main log file.
# Console messages are prepended with a timestamp for better traceability during long runs.
# Usage: log_message "Your informative message here"
log_message() {
    local message="$1"
    # Print to console with a timestamp
    echo "$(date +'%Y-%m-%d %H:%M:%S') - ${message}"
    # Append raw message to the log file
    echo "${message}" >> "${OUTPUT_LOG}"
}

# log_error: Prints an error message to both the console (stderr) and the main log file.
# Console messages are prepended with "ERROR:" and a timestamp.
# Usage: log_error "A description of the error that occurred"
log_error() {
    local error_message="$1"
    # Print to console (stderr) with a timestamp and ERROR prefix
    echo "$(date +'%Y-%m-%d %H:%M:%S') - ERROR: ${error_message}" >&2
    # Append ERROR prefix and message to the log file
    echo "ERROR: ${error_message}" >> "${OUTPUT_LOG}"
}

# --- Setup and Sanity Checks ---
# This function prepares the environment, checks for necessary tools and paths,
# and initializes the logging system.
setup() {
    # Create the main output directory. OUTPUT_LOG will reside here.
    # mkdir -p creates parent directories as needed and doesn't fail if the directory already exists.
    mkdir -p "${OUTPUT_DIR}"
    # Since set -e is active, the script would exit if mkdir fails.
    # An explicit check could be added if more custom error handling for this step is needed.

    # Initialize the main log file.
    # This ensures it's fresh for each run (or created if it's the first message).
    echo "Benchmark Run Log - $(date)" > "${OUTPUT_LOG}"
    echo "==================================================" >> "${OUTPUT_LOG}"

    log_message "Starting benchmark script..."
    log_message "--- Configuration ---"
    # Using realpath (if available) to log canonical paths for clarity.
    log_message "NUM_DRY_RUNS: ${NUM_DRY_RUNS}"
    log_message "NUM_ACTUAL_RUNS: ${NUM_ACTUAL_RUNS}"
    log_message "NEW_FUTHARK_LOC: $(realpath "${NEW_FUTHARK_LOC}" 2>/dev/null || echo "${NEW_FUTHARK_LOC}")"
    log_message "OLD_FUTHARK_LOC: $(realpath "${OLD_FUTHARK_LOC}" 2>/dev/null || echo "${OLD_FUTHARK_LOC}")"
    log_message "HYPERFINE_LOC: ${HYPERFINE_LOC}"
    log_message "TEST_FILES_DIR: $(realpath "${TEST_FILES_DIR}" 2>/dev/null || echo "${TEST_FILES_DIR}")"
    log_message "USER_SPECIFIED_OUTPUT_DIR_PREFIX: $(realpath "${USER_SPECIFIED_OUTPUT_DIR_PREFIX}" 2>/dev/null || echo "${USER_SPECIFIED_OUTPUT_DIR_PREFIX}")"
    log_message "OUTPUT_DIR: $(realpath "${OUTPUT_DIR}" 2>/dev/null || echo "${OUTPUT_DIR}")"
    log_message "OUTPUT_LOG: $(realpath "${OUTPUT_LOG}" 2>/dev/null || echo "${OUTPUT_LOG}")"
    log_message "----------------------"

    # Check for hyperfine executable
    if ! command -v "${HYPERFINE_LOC}" &> /dev/null; then
        log_error "'${HYPERFINE_LOC}' could not be found. Please install hyperfine or ensure HYPERFINE_LOC is correct."
        exit 1
    fi
    log_message "Found hyperfine: $(command -v "${HYPERFINE_LOC}")"

    # Check for jq (JSON processor)
    if ! command -v jq &> /dev/null; then
        log_error "'jq' could not be found. Please install jq, as it is required for parsing hyperfine's JSON output."
        exit 1
    fi
    log_message "Found jq: $(command -v jq)"

    # Check Futhark binaries' existence and executability
    if [ ! -x "${NEW_FUTHARK_LOC}" ]; then
        log_error "New Futhark binary not found or not executable at '${NEW_FUTHARK_LOC}'."
        exit 1
    fi
    if [ ! -x "${OLD_FUTHARK_LOC}" ]; then
        log_error "Old Futhark binary not found or not executable at '${OLD_FUTHARK_LOC}'."
        exit 1
    fi
     # Check test files directory existence
    if [ ! -d "${TEST_FILES_DIR}" ]; then
        log_error "Test files directory not found at '${TEST_FILES_DIR}'."
        exit 1
    fi

    log_message "Setup complete. All checks passed and environment is ready."
    echo "==================================================" >> "${OUTPUT_LOG}" # Separator in log file
}

# --- Main Processing Logic ---
# This function iterates through the .fut files, runs benchmarks,
# parses results, and updates statistics.
main_processing() {
    # 2. Identify all the .fut files in TEST_FILES_DIR
    local fut_files_list=() # Initialize an empty array to store file paths
    # Use find to locate .fut files and read them into the array.
    # -print0 and read -d $'\0' handle filenames with spaces or special characters.
    while IFS= read -r -d $'\0' file; do
        fut_files_list+=("$file")
    done < <(find "${TEST_FILES_DIR}" -name "*.fut" -type f -print0)

    TOTAL_FILES_FOUND=${#fut_files_list[@]} # Store the total count of found files

    if [ "${TOTAL_FILES_FOUND}" -eq 0 ]; then
        log_message "No .fut files found in '${TEST_FILES_DIR}'. Nothing to benchmark."
        return # Exit main_processing, script will proceed to summarize.
    fi
    log_message "Found ${TOTAL_FILES_FOUND} .fut files to benchmark in '${TEST_FILES_DIR}'."

    # 3. Benchmark loop: Iterate over each found .fut file
    for test_file_full_path in "${fut_files_list[@]}"; do
        ((PROCESSED_FILES_COUNT++)) # Increment counter for attempted files
        local test_file_basename
        test_file_basename=$(basename "${test_file_full_path}")
        # Construct a prefix for output files related to this specific .fut file
        local output_file_prefix="${OUTPUT_DIR}/${test_file_basename%.fut}" # Removes .fut extension

        # 3.5. Log the file being benchmarked
        log_message "" # Adds a little spacing in the log for readability
        log_message "Benchmarking [${PROCESSED_FILES_COUNT}/${TOTAL_FILES_FOUND}]: ${test_file_full_path}"

        # Define the commands to be benchmarked
        # Environment variables are set for Futhark to control debugging/logging verbosity.
        local cmd_new="'${NEW_FUTHARK_LOC}' check '${test_file_full_path}'"
        local cmd_old="'${OLD_FUTHARK_LOC}' check '${test_file_full_path}'"
        
        # Define paths for hyperfine's output files for this test case
        local hyperfine_json_output="${output_file_prefix}.check.hyperfine.json"
        local hyperfine_md_output="${output_file_prefix}.check.hyperfine.md" # For human-readable summary

        # Run hyperfine
        # The --command-name arguments are important as they are used by jq to parse the JSON output.
        # We capture hyperfine's stderr to a temporary file to log it in case of failure.
        local hyperfine_stderr_tmp
        hyperfine_stderr_tmp=$(mktemp)

        if ! "${HYPERFINE_LOC}" --warmup "${NUM_DRY_RUNS}" --runs "${NUM_ACTUAL_RUNS}" \
            --command-name "NewFUTHARK" "${cmd_new}" \
            --command-name "OldFUTHARK" "${cmd_old}" \
            --export-json "${hyperfine_json_output}" \
            --export-markdown "${hyperfine_md_output}" 2> "${hyperfine_stderr_tmp}"; then # Capture stderr
            
            log_error "Hyperfine failed for '${test_file_full_path}'."
            if [ -s "${hyperfine_stderr_tmp}" ]; then # Check if stderr temp file has content
                log_error "Hyperfine stderr: $(cat "${hyperfine_stderr_tmp}")"
            fi
            rm -f "${hyperfine_stderr_tmp}" # Clean up temp file
            ((FAILED_BENCHMARK_COUNT++))
            continue # Skip to the next file
        fi
        rm -f "${hyperfine_stderr_tmp}" # Clean up temp file on success too

        # Verify that hyperfine produced the JSON output file
        if [ ! -f "${hyperfine_json_output}" ]; then
            log_error "Hyperfine JSON output not found at '${hyperfine_json_output}' for '${test_file_full_path}', though hyperfine reported success."
            ((FAILED_BENCHMARK_COUNT++))
            continue
        fi

        # Parse hyperfine JSON output for 'futhark check'
        local mean_check_new_str mean_check_old_str
        mean_check_new_str=$(jq -r '.results[] | select(.command == "NewFUTHARK") | .mean' "${hyperfine_json_output}")
        mean_check_old_str=$(jq -r '.results[] | select(.command == "OldFUTHARK") | .mean' "${hyperfine_json_output}")

        # Validate parsed mean times for 'futhark check'
        if [ -z "${mean_check_new_str}" ] || [ "${mean_check_new_str}" == "null" ]; then
            log_error "Could not parse mean time for New Futhark ('check') for '${test_file_full_path}' from '${hyperfine_json_output}'."
            ((FAILED_BENCHMARK_COUNT++))
            continue
        fi
        if [ -z "${mean_check_old_str}" ] || [ "${mean_check_old_str}" == "null" ]; then
            log_error "Could not parse mean time for Old Futhark ('check') for '${test_file_full_path}' from '${hyperfine_json_output}'."
            ((FAILED_BENCHMARK_COUNT++))
            continue
        fi
        
        log_message "  New Futhark 'check' mean time: ${mean_check_new_str} s"
        log_message "  Old Futhark 'check' mean time: ${mean_check_old_str} s"
        log_message ""

        # --- Benchmark 'futhark check-syntax' ---
        log_message "  Benchmarking 'futhark check-syntax' for ${test_file_basename}..."
        local cmd_syntax_new="'${NEW_FUTHARK_LOC}' check-syntax '${test_file_full_path}'"
        local cmd_syntax_old="'${OLD_FUTHARK_LOC}' check-syntax '${test_file_full_path}'"
        
        local hyperfine_syntax_json_output="${output_file_prefix}.syntax.hyperfine.json"
        local hyperfine_syntax_md_output="${output_file_prefix}.syntax.hyperfine.md"
        local hyperfine_syntax_stderr_tmp
        hyperfine_syntax_stderr_tmp=$(mktemp)

        if ! "${HYPERFINE_LOC}" --warmup "${NUM_DRY_RUNS}" --runs "${NUM_ACTUAL_RUNS}" \
            --command-name "NewFUTHARK_Syntax" "${cmd_syntax_new}" \
            --command-name "OldFUTHARK_Syntax" "${cmd_syntax_old}" \
            --export-json "${hyperfine_syntax_json_output}" \
            --export-markdown "${hyperfine_syntax_md_output}" 2> "${hyperfine_syntax_stderr_tmp}"; then
            
            log_error "Hyperfine for 'futhark check-syntax' failed for '${test_file_full_path}'."
            if [ -s "${hyperfine_syntax_stderr_tmp}" ]; then
                log_error "Hyperfine (syntax) stderr: $(cat "${hyperfine_syntax_stderr_tmp}")"
            fi
            rm -f "${hyperfine_syntax_stderr_tmp}"
            ((FAILED_BENCHMARK_COUNT++)) # This benchmark (for adjusted results) failed
            log_message "  Skipping adjusted metrics for ${test_file_basename} due to 'check-syntax' benchmark failure."
            continue # Skip to the next file
        fi
        rm -f "${hyperfine_syntax_stderr_tmp}"

        if [ ! -f "${hyperfine_syntax_json_output}" ]; then
            log_error "Hyperfine JSON output for 'futhark check-syntax' not found at '${hyperfine_syntax_json_output}' for '${test_file_full_path}'."
            ((FAILED_BENCHMARK_COUNT++))
            log_message "  Skipping adjusted metrics for ${test_file_basename} due to missing 'check-syntax' JSON."
            continue
        fi

        # Parse 'futhark check-syntax' hyperfine JSON output
        local mean_syntax_new_str mean_syntax_old_str
        mean_syntax_new_str=$(jq -r '.results[] | select(.command == "NewFUTHARK_Syntax") | .mean' "${hyperfine_syntax_json_output}")
        mean_syntax_old_str=$(jq -r '.results[] | select(.command == "OldFUTHARK_Syntax") | .mean' "${hyperfine_syntax_json_output}")

        if [ -z "${mean_syntax_new_str}" ] || [ "${mean_syntax_new_str}" == "null" ]; then
            log_error "Could not parse mean syntax time for New Futhark for '${test_file_full_path}' from '${hyperfine_syntax_json_output}'."
            ((FAILED_BENCHMARK_COUNT++))
            log_message "  Skipping adjusted metrics for ${test_file_basename} due to 'check-syntax' parsing error (New)."
            continue
        fi
        if [ -z "${mean_syntax_old_str}" ] || [ "${mean_syntax_old_str}" == "null" ]; then
            log_error "Could not parse mean syntax time for Old Futhark for '${test_file_full_path}' from '${hyperfine_syntax_json_output}'."
            ((FAILED_BENCHMARK_COUNT++))
            log_message "  Skipping adjusted metrics for ${test_file_basename} due to 'check-syntax' parsing error (Old)."
            continue
        fi

        log_message "  New Futhark 'check-syntax' mean time: ${mean_syntax_new_str} s"
        log_message "  Old Futhark 'check-syntax' mean time: ${mean_syntax_old_str} s"
        log_message ""

        # --- Calculate Adjusted Times and Generate Reports ---
        local adjusted_mean_new_str adjusted_mean_old_str
        adjusted_mean_new_str=$(echo "${mean_check_new_str} - ${mean_syntax_new_str}" | bc -l)
        adjusted_mean_old_str=$(echo "${mean_check_old_str} - ${mean_syntax_old_str}" | bc -l)

        # Handle potential negative results from subtraction (e.g., if syntax check was slower, or precision issues)
        # Using "0.0" for negative results to prevent issues with comparisons and keep logic simple.
        if (( $(echo "${adjusted_mean_new_str} < 0" | bc -l) )); then
            log_message "  Warning: Adjusted New Futhark mean time is negative (${adjusted_mean_new_str}s). Using 0.0s for this file's adjusted metrics."
            adjusted_mean_new_str="0.0"
        fi
        if (( $(echo "${adjusted_mean_old_str} < 0" | bc -l) )); then
            log_message "  Warning: Adjusted Old Futhark mean time is negative (${adjusted_mean_old_str}s). Using 0.0s for this file's adjusted metrics."
            adjusted_mean_old_str="0.0"
        fi

        log_message "  Adjusted New Futhark mean time (typechecking): ${adjusted_mean_new_str} s"
        log_message "  Adjusted Old Futhark mean time (typechecking): ${adjusted_mean_old_str} s"

        # Create new JSON and Markdown files for adjusted results
        local adjusted_json_output="${output_file_prefix}.adjusted.hyperfine.json"
        local adjusted_md_output="${output_file_prefix}.adjusted.hyperfine.md"

        jq -n \
        --arg file_basename "${test_file_basename}" \
        --argjson check_new_mean "${mean_check_new_str:-null}" \
        --argjson check_old_mean "${mean_check_old_str:-null}" \
        --argjson syntax_new_mean "${mean_syntax_new_str:-null}" \
        --argjson syntax_old_mean "${mean_syntax_old_str:-null}" \
        --argjson adj_new_mean "${adjusted_mean_new_str:-null}" \
        --argjson adj_old_mean "${adjusted_mean_old_str:-null}" \
        '{
            "file": $file_basename,
            "check_means": {
            "new_futhark": $check_new_mean,
            "old_futhark": $check_old_mean
            },
            "syntax_check_means": {
            "new_futhark": $syntax_new_mean,
            "old_futhark": $syntax_old_mean
            },
            "adjusted_means_typechecking": {
            "new_futhark": $adj_new_mean,
            "old_futhark": $adj_old_mean
            }
        }' > "${adjusted_json_output}"
        log_message "  Adjusted JSON results (typechecking focus) saved to: ${adjusted_json_output}"

        # Winner determination based on adjusted times
        local winner_adjusted="Tie or indeterminable (adjusted)"
        if (( $(echo "${adjusted_mean_new_str} < ${adjusted_mean_old_str}" | bc -l) )); then
            winner_adjusted="New Futhark (adjusted)"
            ((NEW_FUTHARK_WINS++)) # Counter now reflects wins based on adjusted times
        elif (( $(echo "${adjusted_mean_old_str} < ${adjusted_mean_new_str}" | bc -l) )); then
            winner_adjusted="Old Futhark (adjusted)"
            ((OLD_FUTHARK_WINS++)) # Counter now reflects wins based on adjusted times
        else # Adjusted times are equal
            winner_adjusted="Tie (adjusted times are equal)"
        fi

        # Create adjusted Markdown
        {
            echo "## Adjusted Benchmark Results for ${test_file_basename} (Typechecking Focus)"
            echo ""
            echo "| Metric                             | New Futhark (${NEW_FUTHARK_LOC##*/}) | Old Futhark (${OLD_FUTHARK_LOC##*/}) | Unit |"
            echo "|------------------------------------|---------------------------------|---------------------------------|------|"
            echo "| Mean 'futhark check' time          | ${mean_check_new_str}           | ${mean_check_old_str}           | s    |"
            echo "| Mean 'futhark check-syntax' time   | ${mean_syntax_new_str}        | ${mean_syntax_old_str}        | s    |"
            echo "| **Adjusted Mean Time (Typechecking)** | **${adjusted_mean_new_str}**    | **${adjusted_mean_old_str}**    | s    |"
            echo ""
            echo "Winner (based on adjusted typechecking time): ${winner_adjusted}"
            echo ""
            # Relative paths for links within the output directory
            local rel_check_md_path="./$(basename "${hyperfine_md_output}")" 
            local rel_syntax_md_path="./$(basename "${hyperfine_syntax_md_output}")"
            echo "Raw 'futhark check' hyperfine report: [${test_file_basename%.fut}.hyperfine.md](${rel_check_md_path})"
            echo ""
            echo "Raw 'futhark check-syntax' hyperfine report: [${test_file_basename%.fut}.syntax.hyperfine.md](${rel_syntax_md_path})"
        } > "${adjusted_md_output}"
        log_message "  Adjusted Markdown report (typechecking focus) saved to: ${adjusted_md_output}"

        log_message "  Winner for ${test_file_basename} (based on adjusted typechecking times): ${winner_adjusted}"

        # Update MAX_TIME logic based on adjusted times
        local current_file_longest_adjusted_run_component
        if (( $(echo "${adjusted_mean_new_str} > ${adjusted_mean_old_str}" | bc -l) )); then
            current_file_longest_adjusted_run_component="${adjusted_mean_new_str}"
        else
            current_file_longest_adjusted_run_component="${adjusted_mean_old_str}"
        fi

        if (( $(echo "${current_file_longest_adjusted_run_component} > ${MAX_TIME}" | bc -l) )); then
            MAX_TIME="${current_file_longest_adjusted_run_component}"
            MAX_TIME_FILE="${test_file_full_path}"
            log_message "  New overall MAX_TIME (adjusted for typechecking): ${MAX_TIME} s (from file ${MAX_TIME_FILE})"
        fi
    done
}

# --- Summary ---
# This function prints the aggregated results and final statistics of the benchmark run.
summarize_results() {
    log_message "" # Spacing for readability
    log_message "--- Benchmark Summary ---"
    log_message "Total .fut files found: ${TOTAL_FILES_FOUND}"
    log_message "Total .fut files processed/attempted: ${PROCESSED_FILES_COUNT}"
    
    local successful_benchmarks=$((PROCESSED_FILES_COUNT - FAILED_BENCHMARK_COUNT))
    log_message "Successfully benchmarked files (hyperfine completed and results parsed): ${successful_benchmarks}"
    log_message "Failed benchmarks (hyperfine error or result parsing issue): ${FAILED_BENCHMARK_COUNT}"
    log_message ""

    log_message "New Futhark wins: ${NEW_FUTHARK_WINS}"
    log_message "Old Futhark wins: ${OLD_FUTHARK_WINS}"
    # Ties are files successfully benchmarked where neither was strictly faster, or times were equal.
    local ties=$((successful_benchmarks - NEW_FUTHARK_WINS - OLD_FUTHARK_WINS))
    # Ensure ties is not negative if something went wrong with counters (e.g. if wins are counted for failed runs)
    if [ $ties -lt 0 ]; then ties=0; fi 
    log_message "Ties (or equal performance): ${ties}"
    log_message ""

    if [ "${successful_benchmarks}" -gt 0 ]; then
        if [ -n "${MAX_TIME_FILE}" ]; then # Check if MAX_TIME_FILE was set
            log_message "Longest benchmark component time (MAX_TIME): ${MAX_TIME} s"
            log_message "File associated with MAX_TIME: ${MAX_TIME_FILE}"
        else
            # This case might occur if all runs had 0.0s time or MAX_TIME remained 0.0
            log_message "MAX_TIME: ${MAX_TIME} s (MAX_TIME_FILE not set, check individual logs if unexpected)"
        fi
    else
         log_message "MAX_TIME: N/A (No successful benchmarks to determine MAX_TIME)"
    fi

    log_message "--- End of Summary ---"
    log_message ""
    log_message "Individual hyperfine JSON and Markdown reports are located in: ${OUTPUT_DIR}"
    log_message "This overall summary and detailed logs are in: ${OUTPUT_LOG}"
}


# --- Script Execution ---

# Call setup function to initialize and check environment
setup

# Call main processing function to run benchmarks
main_processing

# Call summary function to print final results
summarize_results

log_message "Benchmark script finished."

# Exit code: 0 if no benchmarks failed, or the number of failed benchmarks otherwise.
# This can be useful for CI/CD systems or automated scripting.
if [ "${FAILED_BENCHMARK_COUNT}" -gt 0 ]; then
    exit "${FAILED_BENCHMARK_COUNT}" # Exit with the count of failures
else
    exit 0 # Success
fi

    
