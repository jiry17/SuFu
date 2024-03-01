import sys
from sexp import sexp as sexpParser
import json
import pprint as pp

def stripComments(bmFile):
    noComments = '('
    for line in bmFile:
        line = line.split(';', 1)[0]
        noComments += line
    return noComments + ')'

def sexpFromString(value):
    return sexpParser.parseString(value, parseAll = True).asList()[0]

def sexpFromFile(benchmarkFileName):
    try:
        benchmarkFile = open(benchmarkFileName)
    except:
        print('File not found: %s' % benchmarkFileName)
        return None

    bm = stripComments(benchmarkFile)
    bmExpr = sexpFromString(bm)
    benchmarkFile.close()
    return bmExpr

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("python3 main.py $file_name $output_file")
        exit(0)
    file_name = sys.argv[1]
    output_file = sys.argv[2]
    result = sexpFromFile(file_name)

    with open(output_file, "w") as oup:
        json.dump(result, fp=oup, indent=4)