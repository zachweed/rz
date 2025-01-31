import re
import sys

from os import walk

class FuzzySearch(object):
  def __init__(self, string, path):
    self.string = string
    self.files = []
    self.path = path
    self.search_for_string()

  def search_for_string(self):
    files = []
    for (path, directories, file_names) in walk(self.path):
      for file in file_names:
        file = open(f"{self.path}/{file}", "r")
        line_counter = 0
        for line in file:
          if re.search(f"{self.string}" , line):
            print(f"File {file.name} at line {line_counter} could contain this function")
            line_counter += 1
    return files
