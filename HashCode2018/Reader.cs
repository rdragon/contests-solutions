using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace HashCode2018
{
  class Reader
  {
    public string File { get; }
    private readonly string[] _lines;
    private int _row;
    private int _column;

    public Reader(string[] lines, string file = null)
    {
      Helper.ForbidNull(lines);
      _lines = lines;
      File = file;
    }
    public static Reader FromFile(string file)
    {
      Helper.RequireExistsFile(file);
      return new Reader(System.IO.File.ReadAllLines(file), file);
    }
    public int ReadInt()
    {
      var token = ReadToken();
      if (!int.TryParse(token, out var number))
        throw new ParseException($"Cannot convert '{token}' to int.");
      return number;
    }
    public List<int> ReadInts(int count)
    {
      Helper.RequireNonNegative(count);
      var list = new List<int>();
      for (int i = 0; i < count; i++)
        list.Add(ReadInt());
      return list;
    }
    public List<string> ReadLines(int count, int? expectedColumnCount)
    {
      Helper.RequireNonNegative(count);
      if (0 < _column)
        throw new ParseException($"Function {nameof(ReadLines)} expects a fresh line, but current position is line {_row + 1}, column " +
          $"{_column + 1}.");
      var lines = new List<string>(count);
      for (int i = 0; i < count; i++)
      {
        ForbidEndOfFile();
        var line = _lines[_row];
        if (expectedColumnCount.HasValue && line.Length != expectedColumnCount.Value)
          throw new ParseException($"Expecting {expectedColumnCount} columns for line {_row + 1}, but found {line.Length} columns.");
        lines.Add(line);
        _row++;
      }
      return lines;
    }
    public string ReadToken()
    {
      ForbidEndOfFile();
      var line = _lines[_row];
      var column = line.IndexOf(' ', _column);
      column = column == -1 ? line.Length : column;
      if (column == _column)
        throw new ParseException($"Unexpected whitespace at line {_row + 1}, column {_column + 1}.");
      var token = line.Substring(_column, column - _column);
      _column = column + 1;
      if (line.Length <= _column)
        MoveToNextLine();
      return token;
    }
    private void MoveToNextLine()
    {
      _row++;
      _column = 0;
    }
    private void ForbidEndOfFile()
    {
      if (_lines.Length <= _row)
        throw new ParseException("Unexpected end of file.");
    }
  }
}
