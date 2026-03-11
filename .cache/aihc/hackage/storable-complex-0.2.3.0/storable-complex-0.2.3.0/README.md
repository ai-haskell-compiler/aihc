# what is storable-complex?

historically, `Complex a` didn't have a storable instance in base,
and thus storable-complex was born!

# when might i use this package?

you're using an older ghc/version of base AND calling out to some C or fortran
codes around complex numbers. You might find yourself needing this then!

# when might i regret using this?
if you're writing your own orphan instances instead of using base-orphans,
you might have a bad day, use base-orphans (and storable-complex) instead!
