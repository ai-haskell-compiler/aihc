Changes for Database/BBDB
=========================

from 0.7 -> 0.8
---------------
Changed back from Foundation to Prelude to prevent breakage.

from 0.6 -> 0.7
---------------
Changed over to Foundation rather than Prelude.

from 0.5 -> 0.6
---------------
The new BBDB version (BBDB3) uses file format 9 now.  Three fields
were added: a hash field, a creation date field, and a modification
time field.  Theses used to be present in the notes field.


from 0.4 -> 0.5
---------------
The BBDB file format changed.  The current version of the format
is 7.  Once field, called *Affixes* was added, and now the extension
in a US phone field is required.

