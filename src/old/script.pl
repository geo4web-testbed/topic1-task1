%! dbf2csv(+DbfFile, -CsvFile) is det.

dbf2csv(DbfFile, CsvFile) :-
  file_change_extension(DbfFile, csv, CsvFile),
  run_process(python3, [file('dbf2csv.py'),file(DbfFile),file(CsvFile)]).
