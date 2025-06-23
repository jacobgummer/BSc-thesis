import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import re
import sys
import argparse

def plot_solver_performance(csv_files):
    plt.figure(figsize=(9,6))
    for csv_file in csv_files:
        try:
            df = pd.read_csv(csv_file)
        except:
            print(f"An error happened while reading the CSV file: {csv_file}")
            sys.exit(1)

        solve_data = {}
        
        variables_pattern = re.compile(r'(\d+)\s*variables')


        
        for index, row in df.iterrows():
            name = row['Name']
            mean = row['Mean'] * 1_000_000
            
            match = variables_pattern.search(name)
            if match:
                num_variables = int(match.group(1))
                solve_data[num_variables] = mean
            else:
                print(f"Warning: Could not extract number of variables from name: '{name}'")
        
        sorted_solve_data = sorted(solve_data.items())
        
        x = [item[0] for item in sorted_solve_data]
        y = [item[1] for item in sorted_solve_data]
        
        label = input(f'Label of {csv_file}: ')

        df_ns = df['Mean'] * 1_000_000
        df_mean = df_ns.mean()
        df_median = df_ns.median()
        df_std = df_ns.std()
        print(f'{label} mean: {df_mean}')
        print(f'{label} median: {df_median}')
        print(f'{label} std: {df_std}')

        # m, b = np.polyfit(x, y, 1)
        # plt.plot(x, m*x+b, label=f'{label}_reg')
        plt.plot(x, y, marker='o', linestyle='-', label=label, markersize=3)

    plt.title('Mean Execution Time vs. Number of Type Variables', fontsize=16)
    plt.xlabel('Number of Type Variables (n)', fontsize=16)
    plt.ylabel('Mean Execution Time (ns)', fontsize=16)

    x_ticks = list(range(0, 1001, 50))
    y_ticks = list(range(0, 8601, 500))
    plt.xticks(x_ticks)
    plt.yticks(y_ticks)
    plt.grid(True)
    plt.legend()
    plt.tight_layout()
    try:
        plt.savefig('graph.png')
        print("Graph saved to graph.png.")
    except:
        print(f'An error happened when saving the figure to ./graph.png')
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Generate Cons vs Time plots from Futhack type checker CSV results.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument("csv_files", nargs='+', help="Paths to one or more CSV files to be plotted")

    args = parser.parse_args()

    plot_solver_performance(args.csv_files)