#!/bin/bash

NUM_DRY_RUNS=${NUM_DRY_RUNS:-3}
NUM_ACTUAL_RUNS=${NUM_ACTUAL_RUNS:-15}
NEW_FUTHARK_LOC=${NEW_FUTHARK_LOC:-"../binaries/futhark-new-2.0"}
OLD_FUTHARK_LOC=${OLD_FUTHARK_LOC:-"../binaries/futhark-automap-commit"}
HYPERFINE_LOC=${HYPERFINE_LOC:-"hyperfine"} # Assumed to be in PATH
USER_SPECIFIED_OUTPUT_DIR_PREFIX=${USER_SPECIFIED_OUTPUT_DIR_PREFIX:-"../results/hyperfine_benching_out"}
TEST_FILES_DIR=${TEST_FILES_DIR:-"./hyperfine_benching_in"}

OUTPUT_DIR="${USER_SPECIFIED_OUTPUT_DIR_PREFIX}/benchmark_out_$(date +%Y%m%d_%H%M%S)"
OUTPUT_LOG="${OUTPUT_DIR}/benchmark_result.log"

MAX_TIME=0.0
MAX_TIME_FILE=""

NEW_FUTHARK_WINS=0
OLD_FUTHARK_WINS=0

PROCESSED_FILES_COUNT=0
FAILED_BENCHMARK_COUNT=0
TOTAL_FILES_FOUND=0

log_message() {
    local message="$1"
    echo "$(date +'%Y-%m-%d %H:%M:%S') - ${message}"
    echo "${message}" >> "${OUTPUT_LOG}"
}

log_error() {
    local error_message="$1"
    echo "$(date +'%Y-%m-%d %H:%M:%S') - ERROR: ${error_message}" >&2
    echo "ERROR: ${error_message}" >> "${OUTPUT_LOG}"
}

setup() {
    mkdir -p "${OUTPUT_DIR}"

    echo "Benchmark Run Log - $(date)" > "${OUTPUT_LOG}"
    echo "==================================================" >> "${OUTPUT_LOG}"

    log_message "Starting benchmark script..."
    log_message "--- Configuration ---"
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
    echo "==================================================" >> "${OUTPUT_LOG}"
}


main_processing() {
    local fut_files_list=()
    while IFS= read -r -d $'\0' file; do
        fut_files_list+=("$file")
    done < <(find "${TEST_FILES_DIR}" -name "*.fut" -type f -print0)

    TOTAL_FILES_FOUND=${#fut_files_list[@]}

    if [ "${TOTAL_FILES_FOUND}" -eq 0 ]; then
        log_message "No .fut files found in '${TEST_FILES_DIR}'. Nothing to benchmark."
        return
    fi
    log_message "Found ${TOTAL_FILES_FOUND} .fut files to benchmark in '${TEST_FILES_DIR}'."

    for test_file_full_path in "${fut_files_list[@]}"; do
        ((PROCESSED_FILES_COUNT++))
        local test_file_basename
        test_file_basename=$(basename "${test_file_full_path}")

        local output_file_prefix="${OUTPUT_DIR}/${test_file_basename%.fut}"

        log_message ""
        log_message "Benchmarking [${PROCESSED_FILES_COUNT}/${TOTAL_FILES_FOUND}]: ${test_file_full_path}"

        local cmd_new="'${NEW_FUTHARK_LOC}' check '${test_file_full_path}'"
        local cmd_old="'${OLD_FUTHARK_LOC}' check '${test_file_full_path}'"
        
        local hyperfine_json_output="${output_file_prefix}.check.hyperfine.json"
        local hyperfine_md_output="${output_file_prefix}.check.hyperfine.md"

        local hyperfine_stderr_tmp
        hyperfine_stderr_tmp=$(mktemp)

        if ! "${HYPERFINE_LOC}" --warmup "${NUM_DRY_RUNS}" --runs "${NUM_ACTUAL_RUNS}" \
            --command-name "NewFUTHARK" "${cmd_new}" \
            --command-name "OldFUTHARK" "${cmd_old}" \
            --export-json "${hyperfine_json_output}" \
            --export-markdown "${hyperfine_md_output}" 2> "${hyperfine_stderr_tmp}"; then
            
            log_error "Hyperfine failed for '${test_file_full_path}'."
            if [ -s "${hyperfine_stderr_tmp}" ]; then
                log_error "Hyperfine stderr: $(cat "${hyperfine_stderr_tmp}")"
            fi
            rm -f "${hyperfine_stderr_tmp}"
            ((FAILED_BENCHMARK_COUNT++))
            continue
        fi
        rm -f "${hyperfine_stderr_tmp}"

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

        # Benchmark 'futhark check-syntax'
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
            ((FAILED_BENCHMARK_COUNT++))
            log_message "  Skipping adjusted metrics for ${test_file_basename} due to 'check-syntax' benchmark failure."
            continue
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

        local adjusted_mean_new_str adjusted_mean_old_str
        adjusted_mean_new_str=$(echo "${mean_check_new_str} - ${mean_syntax_new_str}" | bc -l)
        adjusted_mean_old_str=$(echo "${mean_check_old_str} - ${mean_syntax_old_str}" | bc -l)

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

        local winner_adjusted="Tie or indeterminable (adjusted)"
        if (( $(echo "${adjusted_mean_new_str} < ${adjusted_mean_old_str}" | bc -l) )); then
            winner_adjusted="New Futhark (adjusted)"
            ((NEW_FUTHARK_WINS++))
        elif (( $(echo "${adjusted_mean_old_str} < ${adjusted_mean_new_str}" | bc -l) )); then
            winner_adjusted="Old Futhark (adjusted)"
            ((OLD_FUTHARK_WINS++))
        else
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
  
            local rel_check_md_path="./$(basename "${hyperfine_md_output}")" 
            local rel_syntax_md_path="./$(basename "${hyperfine_syntax_md_output}")"
            echo "Raw 'futhark check' hyperfine report: [${test_file_basename%.fut}.hyperfine.md](${rel_check_md_path})"
            echo ""
            echo "Raw 'futhark check-syntax' hyperfine report: [${test_file_basename%.fut}.syntax.hyperfine.md](${rel_syntax_md_path})"
        } > "${adjusted_md_output}"
        log_message "  Adjusted Markdown report (typechecking focus) saved to: ${adjusted_md_output}"

        log_message "  Winner for ${test_file_basename} (based on adjusted typechecking times): ${winner_adjusted}"

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


summarize_results() {
    log_message ""
    log_message "--- Benchmark Summary ---"
    log_message "Total .fut files found: ${TOTAL_FILES_FOUND}"
    log_message "Total .fut files processed/attempted: ${PROCESSED_FILES_COUNT}"
    
    local successful_benchmarks=$((PROCESSED_FILES_COUNT - FAILED_BENCHMARK_COUNT))
    log_message "Successfully benchmarked files (hyperfine completed and results parsed): ${successful_benchmarks}"
    log_message "Failed benchmarks (hyperfine error or result parsing issue): ${FAILED_BENCHMARK_COUNT}"
    log_message ""

    log_message "New Futhark wins: ${NEW_FUTHARK_WINS}"
    log_message "Old Futhark wins: ${OLD_FUTHARK_WINS}"

    local ties=$((successful_benchmarks - NEW_FUTHARK_WINS - OLD_FUTHARK_WINS))

    if [ $ties -lt 0 ]; then ties=0; fi 
    log_message "Ties (or equal performance): ${ties}"
    log_message ""

    if [ "${successful_benchmarks}" -gt 0 ]; then
        if [ -n "${MAX_TIME_FILE}" ]; then
            log_message "Longest benchmark component time (MAX_TIME): ${MAX_TIME} s"
            log_message "File associated with MAX_TIME: ${MAX_TIME_FILE}"
        else
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


setup

main_processing

summarize_results

log_message "Benchmark script finished."

if [ "${FAILED_BENCHMARK_COUNT}" -gt 0 ]; then
    exit "${FAILED_BENCHMARK_COUNT}"
else
    exit 0
fi

    
