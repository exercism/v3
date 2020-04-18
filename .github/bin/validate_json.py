#!/usr/bin/env python3

import glob
import json
import sys

def get_files():
  return glob.glob('**/**/*.json', recursive=True)

def validate_files(files):
  return [report for report in (validate_json_file(file) for file in files) if report != None]

def validate_json_file(file):
  with open(file,'r') as f:
    json_content = f.read()

    try:
      json.loads(json_content)
      return None
    except ValueError as error:
      return f"{file}, invalid json: {error}"

def main():
  files = get_files()
  reports = validate_files(files)
  if reports:
    print('\n'.join(reports))
    sys.exit(1)

if __name__ == "__main__":
  main()
