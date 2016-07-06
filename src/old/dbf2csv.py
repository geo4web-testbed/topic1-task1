#!/usr/bin/python

"""Convert DBF files to CSV."""
import argparse
import csv
import sys
from dbfread import DBF


def dbf2csv(dbfFile, csvFile):
    """Convert the given DBF file to CSV."""
    print(dbfFile)
    print(csvFile)
    table = DBF(dbfFile)
    with open(csvFile, mode='+tw', newline='') as f:
        writer = csv.writer(f)
        writer.writerow(table.field_names)
        for record in table:
            writer.writerow(list(record.values()))

parser = argparse.ArgumentParser(description='Convert DBF files to CSV.')
parser.add_argument(
    'dbfFile',
    help='The name of a DBF file',
    metavar='dbfFile',
    type=str
)
parser.add_argument(
    'csvFile',
    help='The name of a CSV file',
    metavar='csvFile',
    type=str
)
args = parser.parse_args()
dbf2csv(args.dbfFile, args.csvFile)
