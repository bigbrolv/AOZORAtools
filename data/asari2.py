import asari
from asari.api import Sonar
import os
#import MeCab
import numpy as np

def funcAsari2(txt):
  sonar = Sonar()
  res = sonar.ping(text=txt)
  return res


