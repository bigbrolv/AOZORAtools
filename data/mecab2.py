import os
import MeCab
import numpy as np
import pickle
#wakati = MeCab.Tagger("-Owakati")
#wakati.parse("pythonが大好きです").split()
tagger = MeCab.Tagger()
#print(tagger.parse("pythonががりりりりりり大好きです"))
#print(mecab.parse(sentence))

def funcMecab(inputfile):
  import os
  import MeCab
  import numpy as np
  data=np.loadtxt(inputfile,delimiter=",",encoding='Shift_JIS',dtype=str)
  wenben = ""
  for i in range(0,len(data)):
    wenben = wenben + data[i]
  wenben_mecab = tagger.parse(wenben)
  return wenben_mecab
  #data=np.loadtxt("/Users/apple/bio-learn/yuliaoku/chikyuzu.txt",delimiter=",",encoding='Shift_JIS',dtype=str)



import asari
from asari.api import Sonar
import os
#import MeCab
import numpy as np

def funcAsari2(txt):
  sonar = Sonar()
  res = sonar.ping(text=txt)
  return res




    

