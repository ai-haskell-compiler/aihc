# 0.2.5
 - (Neil) Make sure to capture variables bound in PFieldPun 
 - (Neil) Make freeVars on Foo{a} include 'a'
# 0.2.4
 - Neil fixed the spotting of brackets inside a lambda.
# 0.2.3
 - `isAtom` is now more complete, thanks to Neil M.
# 0.2.2
 - Improved handling of OverloadedLabel syntax, thanks to Neil M.
 - Changed the treatment of multiple function composition in `needParen`.
    The new treatment is less keen to remove parentheses.
    Note that this is a change of behavior.
 - GHC 8.4 compatibility, courtesy of Ryan G. Scott
