# Delimiter-separated values and parsing

`Dsv` parses and formats comma-, tab- and other single-character-delimited text
strictly to RFC 4180. Quotes are recognised only at the start of a field and
escape themselves by doubling; quoted fields may contain delimiters, quotes and
line breaks. CRLF, LF and CR end records, a leading byte-order mark is ignored,
and one trailing line break adds no record. Malformed input (an unterminated
quote, a quote inside an unquoted field, text after a closing quote) returns a
typed error naming the line and column; a partial table is never returned.

| Function | Contract |
|---|---|
| `Dsv.parseRows` | Every record as exact field text, within `MaxCharacters`, `MaxRows` and `MaxColumns` |
| `Dsv.parse` | Header plus rows; header names must be non-empty and unique and every row must match the header width |
| `Dsv.convert` | Maps each `DsvRow` to a typed value, collecting up to 100 located errors instead of dropping rows |
| `Dsv.format`, `formatRows` | LF line ends, quoting only fields that contain the delimiter, a quote or a line break; parsing the output returns an equal table |
| `Dsv.readAsync` | Reads a `TextReader` asynchronously within the character budget, then parses with cancellation |

`DsvRow.text`, `number`, `integer`, `boolean`, `time` and `optional` read named
fields; errors carry the source line and column name. Nothing reads the
machine's culture or timezone: callers pass a `CultureInfo` and a `TimeZoneInfo`.

`Parse.number` accepts a sign, decimal separator and exponent in the given
culture and rejects whitespace, group separators and non-finite words.
`Parse.time` matches an exact pattern. A pattern with an offset keeps it;
otherwise the wall-clock time is placed in the given zone, and
`LocalTimePolicy` decides daylight-saving cases explicitly: a fold resolves to
the `Earlier` or `Later` occurrence or is rejected, and a gap is rejected or
`ShiftForward` by the gap.

Tests cover quoting, embedded breaks, every line ending, BOM, trailing breaks,
each malformed form with its location, budgets, 200 random round trips in both
CSV and TSV, typed conversion with collected errors, asynchronous reading and
cancellation, strict numbers, and New York's 2026 gap and fold. A gate bounds
parsing 10,000 rows of ten fields (930,946 characters) at 120 ms and 12 MB;
the local median was 8 ms.
