import re
import sys

from os import walk

class FuzzySearch(object):
  def __init__(self, string, path):
    self.search_for_string(string)
    self.files = []
    self.path = path

  def search_for_string(self, string):
    files = []
    for (self.path, directories, file_names) in walk(path):
      for file in file_names:
        file = open(file, "r")
        for line in file:
          if re.search(f"{string}" , line):
            files.extend(file_names)
    return files
