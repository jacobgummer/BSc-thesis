import pandas as pd
import matplotlib.pyplot as plt
import re
import numpy as np
import os
import argparse

# Sentinel value for argparse when a flag is given without a number (meaning "all")
PLOT_ALL_SENTINEL = "__PLOT_ALL__"

# Custom type for argparse: allows an integer or the specific sentinel string
def int_or_sentinel_type(value_str):
    """
    Custom type for argparse.
    Allows an integer or the PLOT_ALL_SENTINEL string.
    Argparse calls this with the string from the command line or the `const` value.
    """
    if value_str == PLOT_ALL_SENTINEL:
        return PLOT_ALL_SENTINEL
    try:
        return int(value_str)
    except ValueError:
        # This message will be shown by argparse if conversion fails
        raise argparse.ArgumentTypeError(f"invalid integer value: '{value_str}'. Must be an integer.")

# Helper function to extract the (Cons: <X>) value from the 'Name' column.
def extract_cons_value(name_str):
    match = re.search(r'\(Cons: (\d+)\)', name_str)
    if match:
        return int(match.group(1))
    return None

# Helper function to extract a common benchmark identifier from the 'Name' column.
def extract_benchmark_name(name_str):
    processed_name = name_str
    match_cons_part = re.match(r'(.*?)\s*\(Cons: \d+\)', name_str)
    if match_cons_part:
        processed_name = match_cons_part.group(1).strip()
    else:
        processed_name = name_str.strip()
    
    match_prefix = re.match(r'^[^/]+/(.*)', processed_name)
    if match_prefix:
        processed_name = match_prefix.group(1)
    return processed_name.strip()

# Function to load data from a CSV file and preprocess it.
def load_data(csv_path):
    try:
        df = pd.read_csv(csv_path)
    except FileNotFoundError:
        print(f"Error: The file {csv_path} was not found. Please check the path.")
        return None
    except Exception as e:
        print(f"Error reading or parsing CSV file {csv_path}: {e}")
        return None

    if 'Name' not in df.columns or 'Mean' not in df.columns:
        print(f"Error: CSV file {csv_path} must contain 'Name' and 'Mean' columns.")
        return None
        
    df['Cons'] = df['Name'].apply(extract_cons_value)
    df['BenchmarkName'] = df['Name'].apply(extract_benchmark_name)
    df['Mean'] = pd.to_numeric(df['Mean'], errors='coerce')
    df['Mean'] = df['Mean'] * 1_000_000_000  # Convert seconds to nanoseconds

    if df['Mean'].isnull().any():
        print(f"Warning: Some 'Mean' values in {csv_path} were not valid numbers and have been ignored (treated as NaN).")
    
    if not df.empty:
        num_null_cons = df['Cons'].isnull().sum()
        if num_null_cons == len(df):
            print(f"Warning: Could not extract any 'Cons' values from 'Name' entries in {csv_path} for the 'Cons vs. Mean' plot.")
        elif num_null_cons > 0:
            print(f"Warning: Failed to extract 'Cons' for {num_null_cons}/{len(df)} entries in {csv_path}.")

    return df

# Function to create the 'Cons vs. Mean Time' plot.
def plot_cons_vs_mean(df_old, df_new, max_cons_filter, plot_title, output_path):
    plt.figure(figsize=(12, 8))
    
    df_old_plot = df_old.copy().dropna(subset=['Cons', 'Mean'])
    df_new_plot = df_new.copy().dropna(subset=['Cons', 'Mean'])

    if max_cons_filter is not None: # max_cons_filter will be an int or None
        df_old_plot = df_old_plot[df_old_plot['Cons'] < max_cons_filter]
        df_new_plot = df_new_plot[df_new_plot['Cons'] < max_cons_filter]

    df_old_plot = df_old_plot.sort_values(by='Cons')
    df_new_plot = df_new_plot.sort_values(by='Cons')

    if df_old_plot.empty and df_new_plot.empty:
        print(f"No data available for '{plot_title}' plot after filtering. Skipping plot generation: {output_path}")
        plt.close()
        return

    if not df_old_plot.empty:
        plt.plot(df_old_plot['Cons'], df_old_plot['Mean'], marker='o', linestyle='-', label='solveOld', alpha=0.8)
    if not df_new_plot.empty:
        plt.plot(df_new_plot['Cons'], df_new_plot['Mean'], marker='s', linestyle='-', label='solveNew', alpha=0.8)

    plt.xlabel('Number of constraints')
    plt.ylabel('Mean Time (nanoseconds)')
    plt.title(plot_title)
    if not df_old_plot.empty or not df_new_plot.empty:
        plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    
    try:
        plt.savefig(output_path)
        print(f"Successfully saved '{plot_title}' plot to: {output_path}")
    except Exception as e:
        print(f"Error saving plot {output_path}: {e}")
    plt.close()

# Function to create the comparison bar chart.
def plot_comparison_bar_chart(display_df, plot_title, output_path):
    if display_df.empty:
        print(f"No data to display for '{plot_title}'. Skipping bar chart generation: {output_path}")
        return

    n_benchmarks = len(display_df['BenchmarkName'])
    y_pos = np.arange(n_benchmarks) 
    bar_height = 0.35 

    fig_height = max(8, n_benchmarks * 0.45) 
    fig_width = 12 
    fig, ax = plt.subplots(figsize=(fig_width, fig_height))
    
    ax.barh(y_pos - bar_height/2, display_df['Mean_old'], bar_height, label='solveOld', color='tab:orange', alpha=0.8)
    ax.barh(y_pos + bar_height/2, display_df['Mean_new'], bar_height, label='solveNew', color='tab:blue', alpha=0.8)

    ax.set_ylabel(f'Benchmark Identifier ({len(display_df)})')
    ax.set_xlabel('Mean Time (nanoseconds)')
    ax.set_title(plot_title)
    ax.set_yticks(y_pos) 
    ax.set_yticklabels(display_df['BenchmarkName'], fontsize=9) 
    ax.legend() 
    ax.xaxis.grid(True, linestyle='--', alpha=0.7) 
    ax.invert_yaxis() 
    fig.tight_layout(pad=1.5) 
    
    try:
        plt.savefig(output_path)
        print(f"Successfully saved '{plot_title}' plot to: {output_path}")
    except Exception as e:
        print(f"Error saving plot {output_path}: {e}")
    plt.close()

def main():
    parser = argparse.ArgumentParser(
        description="Generate benchmark comparison plots from Futhack type checker CSV results.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument(
        "--old-csv-file", 
        required=True, 
        help="Path to the CSV file for the old implementation benchmarks."
    )
    parser.add_argument(
        "--new-csv-file", 
        required=True, 
        help="Path to the CSV file for the new implementation benchmarks."
    )
    parser.add_argument(
        "--output-dir", 
        default=".", 
        help="Directory to save the output plots (default: current directory)."
    )
    parser.add_argument(
        "--cons",
        action='append',
        nargs='?',
        type=int_or_sentinel_type, # MODIFIED: Use custom type
        const=PLOT_ALL_SENTINEL,
        metavar="MAX_CONS",
        help=(
            "Generate 'Constraint Count vs. Mean Time' plot.\n"
            "If set without a number, all benchmarks are included.\n"
            "If a MAX_CONS number is given, only benchmarks with Cons < MAX_CONS are included."
        )
    )
    parser.add_argument(
        "--slowest-old",
        action='append',
        nargs='?',
        type=int_or_sentinel_type, # MODIFIED: Use custom type
        const=PLOT_ALL_SENTINEL,
        metavar="N",
        help=(
            "Generate bar chart: N slowest benchmarks from OLD implementation (sorted by old mean time).\n"
            "If N is not given, all common benchmarks are shown, sorted by old mean time."
        )
    )
    parser.add_argument(
        "--slowest-new",
        action='append',
        nargs='?',
        type=int_or_sentinel_type, # MODIFIED: Use custom type
        const=PLOT_ALL_SENTINEL,
        metavar="N",
        help=(
            "Generate bar chart: N slowest benchmarks from NEW implementation (sorted by new mean time).\n"
            "If N is not given, all common benchmarks are shown, sorted by new mean time."
        )
    )
    parser.add_argument(
        "--filter-old-faster",
        action='append',
        nargs='?',
        type=int_or_sentinel_type, # MODIFIED: Use custom type
        const=PLOT_ALL_SENTINEL,
        metavar="N",
        help=(
            "Generate bar chart: N benchmarks where OLD is faster (Mean_old < Mean_new), sorted by win amount.\n"
            "If N is not given, all benchmarks where old is faster are shown."
        )
    )
    parser.add_argument(
        "--filter-new-faster",
        action='append',
        nargs='?',
        type=int_or_sentinel_type, # MODIFIED: Use custom type
        const=PLOT_ALL_SENTINEL,
        metavar="N",
        help=(
            "Generate bar chart: N benchmarks where NEW is faster (Mean_new < Mean_old), sorted by win amount.\n"
            "If N is not given, all benchmarks where new is faster are shown."
        )
    )
    parser.add_argument(
        "--ascending",
        action="store_true",
        help="Sort applicable bar charts in ascending order (e.g., fastest first) instead of descending (slowest/biggest win first)."
    )

    args = parser.parse_args()

    try:
        os.makedirs(args.output_dir, exist_ok=True)
        print(f"Output directory: {os.path.abspath(args.output_dir)}")
    except OSError as e:
        print(f"Error creating output directory {args.output_dir}: {e}")
        return

    print(f"\nLoading old implementation data from: {args.old_csv_file}")
    df_old_raw = load_data(args.old_csv_file)
    print(f"\nLoading new implementation data from: {args.new_csv_file}")
    df_new_raw = load_data(args.new_csv_file)
    print("")

    if df_old_raw is None or df_new_raw is None:
        print("Script aborted due to errors in loading one or both data files.")
        return

    sort_ascending = args.ascending
    sort_order_str = "ascending" if sort_ascending else "descending"

    # --- Generate '--cons' plots ---
    if args.cons:
        for cons_limit_arg_val in args.cons: # cons_limit_arg_val is int or PLOT_ALL_SENTINEL
            max_cons_val = None
            limit_str = "all"
            
            if cons_limit_arg_val == PLOT_ALL_SENTINEL:
                pass # max_cons_val remains None, limit_str remains "all"
            elif isinstance(cons_limit_arg_val, int):
                if cons_limit_arg_val <= 0:
                    print(f"Warning: --cons value {cons_limit_arg_val} is not positive. Interpreting as all constraints for this plot instance.")
                else:
                    max_cons_val = cons_limit_arg_val
                    limit_str = f"lt_{max_cons_val}"
            # No 'else' needed here as custom type converter handles bad user input.
            
            plot_title = f"Number of Constraints vs. Mean Time (Max Cons: {limit_str.replace('_', ' ')})"
            filename = f"cons_vs_mean_max_cons_{limit_str}.png"
            output_path = os.path.join(args.output_dir, filename)
            
            print(f"\nGenerating: {plot_title}")
            plot_cons_vs_mean(df_old_raw, df_new_raw, max_cons_val, plot_title, output_path)

    df_old_agg = df_old_raw.dropna(subset=['BenchmarkName', 'Mean'])[['BenchmarkName', 'Mean']]
    df_new_agg = df_new_raw.dropna(subset=['BenchmarkName', 'Mean'])[['BenchmarkName', 'Mean']]
    merged_df_base = pd.merge(df_old_agg, df_new_agg, on='BenchmarkName', suffixes=('_old', '_new'))

    if merged_df_base.empty and (args.slowest_old or args.slowest_new or args.filter_old_faster or args.filter_new_faster):
        print("\nNo common benchmarks found. Bar charts requiring merged data cannot be generated.")
    
    # --- Generate '--slowest-old' plots ---
    if args.slowest_old and not merged_df_base.empty:
        for n_val_arg in args.slowest_old: # n_val_arg is int or PLOT_ALL_SENTINEL
            display_df = merged_df_base.copy()
            top_n_str = "all"
            
            display_df = display_df.sort_values(by='Mean_old', ascending=sort_ascending)
            
            if n_val_arg == PLOT_ALL_SENTINEL:
                pass # Show all (already sorted)
            elif isinstance(n_val_arg, int):
                if n_val_arg <= 0:
                    print(f"Warning: --slowest-old value {n_val_arg} must be positive. Showing all benchmarks for this plot instance.")
                else:
                    display_df = display_df.head(n_val_arg)
                    top_n_str = f"top_{n_val_arg}"
            
            desc_asc_label = "Fastest" if sort_ascending else "Slowest"
            plot_title = f"{desc_asc_label} Benchmarks by solveOld Mean Time ({top_n_str.replace('_', ' ')})"
            filename = f"bar_chart_slowest_old_{top_n_str}_{sort_order_str}.png"
            output_path = os.path.join(args.output_dir, filename)

            print(f"\nGenerating: {plot_title}")
            plot_comparison_bar_chart(display_df, plot_title, output_path)

    # --- Generate '--slowest-new' plots ---
    if args.slowest_new and not merged_df_base.empty:
        for n_val_arg in args.slowest_new:
            display_df = merged_df_base.copy()
            top_n_str = "all"

            display_df = display_df.sort_values(by='Mean_new', ascending=sort_ascending)

            if n_val_arg == PLOT_ALL_SENTINEL:
                pass
            elif isinstance(n_val_arg, int):
                if n_val_arg <= 0:
                    print(f"Warning: --slowest-new value {n_val_arg} must be positive. Showing all benchmarks for this plot.")
                else:
                    display_df = display_df.head(n_val_arg)
                    top_n_str = f"top_{n_val_arg}"
            
            desc_asc_label = "Fastest" if sort_ascending else "Slowest"
            plot_title = f"{desc_asc_label} Benchmarks by solveNew Mean Time ({top_n_str.replace('_', ' ')})"
            filename = f"bar_chart_slowest_new_{top_n_str}_{sort_order_str}.png"
            output_path = os.path.join(args.output_dir, filename)

            print(f"\nGenerating: {plot_title}")
            plot_comparison_bar_chart(display_df, plot_title, output_path)

    # --- Generate '--filter-old-faster' plots ---
    if args.filter_old_faster and not merged_df_base.empty:
        for n_val_arg in args.filter_old_faster:
            display_df = merged_df_base[merged_df_base['Mean_old'] < merged_df_base['Mean_new']].copy()
            if display_df.empty:
                print(f"\nNo benchmarks where old is faster. Skipping relevant --filter-old-faster plot.")
                continue
            
            display_df['Win_Amount_Old'] = display_df['Mean_new'] - display_df['Mean_old']
            display_df = display_df.sort_values(by='Win_Amount_Old', ascending=sort_ascending) 
            top_n_str = "all"

            if n_val_arg == PLOT_ALL_SENTINEL:
                pass
            elif isinstance(n_val_arg, int):
                if n_val_arg <= 0:
                    print(f"Warning: --filter-old-faster value {n_val_arg} must be positive. Showing all matches for this plot.")
                else:
                    display_df = display_df.head(n_val_arg)
                    top_n_str = f"top_{n_val_arg}"
            
            win_sort_label = "Smallest Wins" if sort_ascending else "Biggest Wins"
            plot_title = f"solveOld Faster ({win_sort_label} for solveOld, {top_n_str.replace('_', ' ')})"
            filename = f"bar_chart_filter_old_faster_{top_n_str}_{sort_order_str}.png"
            output_path = os.path.join(args.output_dir, filename)

            print(f"\nGenerating: {plot_title}")
            plot_comparison_bar_chart(display_df, plot_title, output_path)

    # --- Generate '--filter-new-faster' plots ---
    if args.filter_new_faster and not merged_df_base.empty:
        for n_val_arg in args.filter_new_faster:
            display_df = merged_df_base[merged_df_base['Mean_new'] < merged_df_base['Mean_old']].copy()
            if display_df.empty:
                print(f"\nNo benchmarks where new is faster. Skipping relevant --filter-new-faster plot.")
                continue

            display_df['Win_Amount_New'] = display_df['Mean_old'] - display_df['Mean_new']
            display_df = display_df.sort_values(by='Win_Amount_New', ascending=sort_ascending)
            top_n_str = "all"

            if n_val_arg == PLOT_ALL_SENTINEL:
                pass
            elif isinstance(n_val_arg, int):
                if n_val_arg <= 0:
                    print(f"Warning: --filter-new-faster value {n_val_arg} must be positive. Showing all matches for this plot.")
                else:
                    display_df = display_df.head(n_val_arg)
                    top_n_str = f"top_{n_val_arg}"

            win_sort_label = "Smallest Wins" if sort_ascending else "Biggest Wins"
            plot_title = f"solveNew Faster ({win_sort_label} for solveNew, {top_n_str.replace('_', ' ')})"
            filename = f"bar_chart_filter_new_faster_{top_n_str}_{sort_order_str}.png"
            output_path = os.path.join(args.output_dir, filename)

            print(f"\nGenerating: {plot_title}")
            plot_comparison_bar_chart(display_df, plot_title, output_path)
            
    print("\nScript finished generating requested plots.")

if __name__ == "__main__":
    main()
