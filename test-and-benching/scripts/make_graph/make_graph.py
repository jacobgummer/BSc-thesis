import pandas as pd
import matplotlib.pyplot as plt
import re
import sys # Import the sys module
def plot_solver_performance(csv_filepath):
    """
    Reads a CSV file containing solver performance data, extracts the mean
    execution times for 'solveNew' and 'solveOld' based on the number of
    variables, and plots them on a single graph.
    Args:
        csv_filepath (str): The path to the input CSV file.
    """
    try:
        # Load the CSV file into a pandas DataFrame
        df = pd.read_csv(csv_filepath)
    except FileNotFoundError:
        print(f"Error: The file '{csv_filepath}' was not found. Please check the path.")
        sys.exit(1) # Exit with an error code
    except pd.errors.EmptyDataError:
        print(f"Error: The file '{csv_filepath}' is empty.")
        sys.exit(1) # Exit with an error code
    except pd.errors.ParserError:
        print(f"Error: Could not parse '{csv_filepath}'. Please check the CSV format.")
        sys.exit(1) # Exit with an error code
    # Initialize dictionaries to store data for solveNew and solveOld
    # Keys will be the number of variables, values will be the Mean
    solve_new_data = {}
    solve_old_data = {}
    # Regular expression to extract the number of variables
    variables_pattern = re.compile(r'(\d+)\s*variables')
    # Iterate through each row in the DataFrame to parse the 'Name' column
    for index, row in df.iterrows():
        name = row['Name']
        mean = row['Mean']
        # Extract the number of variables using the regex
        match = variables_pattern.search(name)
        if match:
            num_variables = int(match.group(1)) # Convert the extracted string to an integer
            if "solveNew" in name:
                solve_new_data[num_variables] = mean
            elif "solveOld" in name:
                solve_old_data[num_variables] = mean
        else:
            print(f"Warning: Could not extract number of variables from name: '{name}'")
    # Sort the data by the number of variables (keys)
    sorted_solve_new = sorted(solve_new_data.items())
    sorted_solve_old = sorted(solve_old_data.items())
    # Separate keys (number of variables) and values (mean) for plotting
    new_x = [item[0] for item in sorted_solve_new]
    new_y = [item[1] for item in sorted_solve_new]
    old_x = [item[0] for item in sorted_solve_old]
    old_y = [item[1] for item in sorted_solve_old]
    # Create the plot
    # plt.figure(figsize=(10, 6)) # Set the figure size for better readability
    plt.plot(new_x, new_y, marker='o', linestyle='-', label='solveNew')
    plt.plot(old_x, old_y, marker='x', linestyle='--', label='solveOld')
    # Add titles and labels for clarity
    plt.title('Mean Execution Time vs. Number of Variables')
    plt.xlabel('Number of Variables')
    plt.ylabel('Mean Execution Time')
    plt.grid(True) # Add a grid for easier reading of values
    plt.legend() # Display the legend to differentiate the lines
    #plt.xscale('log') # Consider a log scale for x-axis if variable counts vary widely
    #plt.yscale('log') # Consider a log scale for y-axis if mean times vary widely
    # Improve layout and display the plot
    plt.tight_layout()
    plt.show()
if __name__ == "__main__":
    # Check if a CSV file path was provided as a command-line argument
    if len(sys.argv) < 2:
        print("Usage: python plot_performance.py <PATH_TO_CSV>")
        sys.exit(1) # Exit with an error code indicating incorrect usage
    else:
        csv_file = sys.argv[1] # The first argument after the script name is the CSV path
        plot_solver_performance(csv_file)