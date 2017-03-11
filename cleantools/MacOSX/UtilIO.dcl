definition module UtilIO

from StdFile import ::Files
from UtilDate import ::DATE

GetFullApplicationPath :: !*Files -> (!{#Char},!*Files)
GetLongPathName :: !{#Char} -> {#Char}
FModified :: !String !Files -> (!DATE, !Files);
FExists	:: .a
