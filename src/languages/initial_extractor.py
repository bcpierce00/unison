import os
import re

ocaml_files = '..'
output_file = 'maybe_messages.pot'
string_pattern = re.compile(r'"([^"]*?)"')

with open(output_file, 'w') as outfile:
    for filename in os.listdir(ocaml_files):
        if filename.endswith('.ml'):
            try:
                with open(os.path.join(ocaml_files, filename), 'r') as filee:
                    content = filee.read()
            except UnicodeDecodeError:
                print(f"Failed to read a file {filename}, not utf-8 encoded?")
                with open(os.path.join(ocaml_files, filename), 'r', encoding='utf-8', errors='ignore') as filee:
                    content = filee.read()
            matches = string_pattern.findall(content)
            for match in matches:
                outfile.write("msgid " + match + '\n')
                outfile.write("msgstr " + match + '\n')
