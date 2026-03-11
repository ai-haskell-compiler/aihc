# Changelog mailpool

# Release 2.3.1 2024.08.14
+ set poolStripeMax to numCapabilities
  this causes issues if set to a low number. (5)

# Release 2.3.0 2024.08.12 
+ set default to starttls
  + apparantly mailgun requires it by default now
  + this change is what makes it a minor release
+ upgrade to new data-pool


# Release 2.2.3 2020.12.03 
+ No I need to fix haskell net

# Release 2.2.2 2020.12.03 
+ Maybe it works if we put the same base constraint on this package as haskell net?

# Release 2.2.1 2020.12.03 

+ Try making hackage build by upgrading haskell net

# Release 2.2.0 2020.12.02 

+ Add changelog
+ Add fromJSON instance to smtp cred so this easily works with yesod.
+ Improve docs.
