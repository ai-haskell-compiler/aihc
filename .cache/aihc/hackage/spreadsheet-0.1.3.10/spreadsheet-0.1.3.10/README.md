## Example: `csvreplace`

If you build the package with the Cabal flag `-fbuildExamples`
then the program `csvreplace` will be built.
It allows you to replace placeholders in a template file
according to the columns of a CSV file.
E.g. given a file `template.txt` with content

~~~~
Name: FIRSTNAME SURNAME
Born: BIRTH
~~~~

and `names.csv` with content

~~~~
"FIRSTNAME","SURNAME",BIRTH
"Georg","Cantor",1845
"Haskell","Curry",1900
"Ada","Lovelace",1815
~~~~

the call

~~~~
csvreplace template.txt <names.csv
~~~~

produces the output

~~~~
Name: Georg Cantor
Born: 1845
Name: Haskell Curry
Born: 1900
Name: Ada Lovelace
Born: 1815
~~~~

You may also generate one file per CSV row in the following manner:

~~~~
csvreplace --multifile=FIRSTNAME-SURNAME.txt template.txt <names.csv
~~~~


### Character Encoding

For simple replacement of parts of the text
we would not need to decode the input texts
and thus we would not need to know the used encoding scheme.
Essentially, we would only require that both CSV and template file
employ the same character encoding.

However, it is not as simple as that.
We need to decode the structure of the CSV file.
In multi-file mode we also need to generate proper file names.
Both requirements force us to decode both CSV and template file.
For the de- and encoding we use the default locale encoding.

If you want essentially a byte-by-byte replacement
and you assert that all files are in the same encoding
where the commas and quotation marks are compatible with ASCII
then you can set the encoding locally
to a complete 8-bit encoding like `latin1` as in:

~~~~
LANG=de_DE csvreplace --multifile=FIRSTNAME-SURNAME.txt template.txt <names.csv
~~~~


## Example: `csvextract`

This is somehow the inverse of `csvreplace`.
Given a text file that was generated
by substituting placeholders in a regular way.
You can then obtain back a CSV file.

E.g. take the example files from `csvreplace` and call

~~~~
csvreplace template.txt <names.csv | csvextract --columns FIRSTNAME,SURNAME,BIRTH template.txt
~~~~

You should get back `names.csv`.

This is, how it works:
The text in `template.txt` is first divided into text and placeholders
according to the comma separated list of names for the `--columns` option.
Then the program matches the template fragments with the input text
and assigns the text between template fragments to the placeholders.
Placeholder replacements are chosen as short as possible
in a greedy way, i.e. per placeholder, not globally.

If you want to skip larger portions of the input text,
you may use a placeholder like `SKIP` in `template.txt`
and call `csvextract` with the option `--ignore SKIP`.
