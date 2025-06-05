# futhark_log_parser.py
import re

class BackslashReplacer:
    def __call__(self, match_obj):
        digits = match_obj.group(1)
        return "_" + digits

def process_line_for_escapes(line_str):
    def transform_quoted_content(match_obj_outer):
        quoted_inner_content = match_obj_outer.group(1)
        replacer_instance = BackslashReplacer()
        transformed_inner_content = re.sub(r"\\(\d+)", replacer_instance, quoted_inner_content)
        return f'"{transformed_inner_content}"'
    return re.sub(r'"([^"]*)"', transform_quoted_content, line_str)

def _format_constraints_and_get_map(constraints_str_list):
    if not constraints_str_list:
        return "[]"
    formatted = ",\n".join([f"    {c}" for c in constraints_str_list])
    formatted = re.sub('f16', 'f32', formatted)
    formatted = re.sub('\(([a-z_]+):([^)]+)\)', r'{\1:\2}', formatted)
    formatted = formatted.replace('})',')}')
    formatted = formatted.replace('#','hashtag_')

    name_mapping = {}
    output_parts = []
    last_end = 0

    for match in re.finditer(r'["\(\[\]]+(?P<word>(?!bool)[a-zA-Z.]+)[,")\]](?!_)', formatted):
        original_name = match.group(1)
        start, end = match.span()
        output_parts.append(formatted[last_end:start])

        new_name_core = f'{original_name}_{get_number()}'

        if original_name not in name_mapping:
            name_mapping[original_name] = new_name_core
        
        output_parts.append(f'"{new_name_core}"')
        last_end = end

    return f"[\n{formatted}\n    ]", name_mapping

# Global counter for unique numbering of type parameters
counter = 0
def get_number():
    global counter
    counter += 1
    return counter
# Note: The 'replacer' function is no longer needed globally as its logic
# will be incorporated into _format_typarams_and_get_map or similar.
def _format_typarams_and_get_map(typarams_str_input: str):
    """
    Formats the type parameters string and captures a mapping from original
    type parameter names to their new numbered names (e.g., 'a' -> 'a_1').
    Uses the global 'counter' via 'get_number()'.
    """
    # Handle empty or "mempty" typarams
    if not typarams_str_input or typarams_str_input == "mempty":
        return "[]", {}  # Return empty list string and empty map
    name_mapping = {}
    
    # Basic unescaping of quotes like \" to "
    current_str = typarams_str_input.replace('"(\\', '(').replace('\\"', '"').replace(')"', ')')
    # The user's regex for identifying type parameter names like "a", "b"
    # was '"([a-z])"'. This captures the 'a' part in group 1.
    # We iterate over matches to build the new string and the map.
    
    output_parts = []
    last_end = 0
    
    for match in re.finditer(r'"([a-z])"', current_str):
        # original_quoted_name = match.group(0)  # e.g., '"a"'
        original_name = match.group(1)         # e.g., 'a' (the part inside quotes)
        start, end = match.span()
        output_parts.append(current_str[last_end:start])
        
        # Generate new name using the global counter
        new_name_core = f"{original_name}_{get_number()}"
        
        # Store mapping: original name (e.g., 'a') to new core name (e.g., 'a_1')
        # This assumes original type parameter names within a single typarams_str are unique.
        if original_name not in name_mapping:
            name_mapping[original_name] = new_name_core
        
        output_parts.append(f'"{new_name_core}"') # Append the new quoted name, e.g., '"a_1"'
        last_end = end
        
    output_parts.append(current_str[last_end:])
    processed_typarams_str = "".join(output_parts)
    
    return processed_typarams_str, name_mapping


def _apply_renaming_to_constraints(constraints_list: list[str], name_map: dict[str, str]) -> list[str]:
    """
    Applies the name_map (e.g., {'a': 'a_1'}) to type variables within constraint strings.
    """
    if not name_map: # No renaming to do
        return constraints_list
    updated_constraints = []
    # Sort original names by length (descending) to handle cases where one name
    # might be a substring of another (e.g., 't' and 'tt') correctly.
    sorted_original_names = sorted(name_map.keys(), key=len, reverse=True)
    for constr_str in constraints_list:
        current_constr = constr_str
        for original_name in sorted_original_names:
            new_name = name_map[original_name]
            # Use regex with word boundaries (\b) to replace only whole occurrences
            # of the original_name. This prevents replacing 'a' in 'array'.
            # Example: if original_name is 'a', new_name is 'a_1':
            # "[]a" becomes "[]a_1"
            # "(a, b)" becomes "(a_1, b)" (if 'b' is processed later)
            current_constr = re.sub(r'\b' + re.escape(original_name) + r'\b', new_name, current_constr)
        updated_constraints.append(current_constr)
    return updated_constraints

def _parse_single_problem_block_to_haskell_tuple_str(problem_block_text: str, min_n_cons: int, top_n_cons: int) -> str:
    """
    Transforms a single Futhark debug output block into a Haskell-like tuple string.
    """
    input_lines = problem_block_text.splitlines()
    
    actual_content_lines = []
    if input_lines:
        first_line_content = input_lines[0].strip()
        if first_line_content == "# TySolve.solve":
            actual_content_lines = input_lines[1:]
        else:
            actual_content_lines = input_lines
    constraints_str_list = []
    # Default typarams_str to "mempty" as in original code if not found.
    # This will be converted to "[]" by _format_typarams_and_get_map.
    typarams_str_list = "mempty" 
    tyvars_str_list = "[]" # Default if not found
    current_section = None
    
    for original_line in actual_content_lines:
        line = original_line.strip()
        # process_line_for_escapes is important for things like \d -> _d in Futhark's output
        line = process_line_for_escapes(line) 
        
        if line.startswith("## "):
            current_section = line[3:].strip().lower()
            continue
        if current_section == "constraints":
            if line:
                constraints_str_list.append(line)
        elif current_section == "typarams":
            if line:
                typarams_str_list = line # Capture the raw line for typarams
        elif current_section == "tyvars":
            if line:
                tyvars_str_list = line
    
    if min_n_cons > len(constraints_str_list) and top_n_cons <= 0:
        return ""
    
    # --- MODIFICATIONS START HERE ---
    global counter
    counter = 0 # Reset global counter for each block's type parameter renaming
    # Process typarams to get the formatted string and the name mapping
    # typarams_str_from_log could be, e.g., '[("a", ...), ("b", ...)]' or "mempty"
    formatted_typarams_for_haskell, typaram_name_map = _format_typarams_and_get_map(typarams_str_list)
    

    # Apply the captured typaram renaming to the constraint strings
    updated_constraints_list = _apply_renaming_to_constraints(constraints_str_list, typaram_name_map)
    _, constraints_name_map = _format_constraints_and_get_map(updated_constraints_list)
    final_constraints_list = _apply_renaming_to_constraints(updated_constraints_list, constraints_name_map)
    formatted_constraints_for_haskell, _ = _format_constraints_and_get_map(final_constraints_list)

    # --- MODIFICATIONS END HERE ---
    # Ensure tyvars_list_str is "[]" if it was empty or just whitespace,
    # as M.fromList expects a list representation.
    if tyvars_str_list.strip() == "[]":
        return ""
    
    tyvars_str_list = tyvars_str_list.replace('fromList','M.fromList').replace('(Name ','(')

    haskell_tuple_string = f"  (    \n    {formatted_constraints_for_haskell},\n    M.fromList {formatted_typarams_for_haskell},\n    M.fromList {tyvars_str_list}\n  )"
    
    return (haskell_tuple_string, len(constraints_str_list))


def parse_log_to_haskell_tuples_list(full_log_text: str, min_n_cons: int, top_n_cons: int) -> list[str]:
    """
    Splits the full log text by lines matching '# TySolve.solve' (preceded by optional newlines)
    and parses each block. Returns a list of Haskell tuple strings.
    """
    if not full_log_text.strip():
        return []

    # This regex splits *before* a line that starts with # TySolve.solve,
    # keeping the delimiter as part of the subsequent string.
    # This matches the behavior of the original script's block splitting.
    potential_blocks = re.split(r'\n*(?=# TySolve\.solve)', full_log_text)
    
    parsed_haskell_tuples = []
    for block_text_content in potential_blocks:
        # Each `block_text_content` here might start with "# TySolve.solve"
        # or it could be the content before the first marker, or if no marker, the whole log.
        stripped_block = block_text_content.strip()
        
        if stripped_block: # Process only if the block is not empty after stripping
            # _parse_single_problem_block_to_haskell_tuple_str will handle
            # ignoring a leading "# TySolve.solve" line within stripped_block.
            tuple_str = _parse_single_problem_block_to_haskell_tuple_str(stripped_block, min_n_cons, top_n_cons)
            if tuple_str == "": continue
            parsed_haskell_tuples.append(tuple_str)
    if top_n_cons > 0:
        sorted_haskell_tuples = sorted(parsed_haskell_tuples, key=lambda item: item[1], reverse=True)
        parsed_haskell_tuples = sorted_haskell_tuples[:top_n_cons]
    
    return parsed_haskell_tuples

if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        input_filename = sys.argv[1]
        try:
            with open(input_filename, 'r', encoding='utf-8') as infile:
                log_data = infile.read()
            
            print(f"--- Input log from {input_filename} ---")
            # print(log_data)
            
            haskell_expr_list = parse_log_to_haskell_tuples_list(log_data)
            
            if haskell_expr_list:
                print(f"\n--- Parsed List of {len(haskell_expr_list)} Haskell Tuple String(s) ---")
                print("[\n" + ",\n\n".join(haskell_expr_list) + "\n]")
            else:
                print("\n--- Log parser returned no data blocks (empty list). ---")

        except FileNotFoundError:
            print(f"Error: Test input file '{input_filename}' not found.")
        except Exception as e:
            print(f"Error processing test input file: {e}")
            raise
    else:
        print("To test futhark_log_parser.py: python futhark_log_parser.py <path_to_log_file>")

