import DBus.Notify

main = do
         client <- connectSession
         let startNote = appNote { summary="Starting"
                                 , body=(Just $ Text "Calculating fib(33).") }
         notification <- notify client startNote
         let endNote = appNote { summary="Finished"
                               , body=(Just . Text . show $ fib33) }
         fib33 `seq` replace client notification endNote
     where
         appNote = blankNote { appName="Fibonacci Demonstration" }
         fib 0 = 0
         fib 1 = 1
         fib n = fib (n-1) + fib (n-2)
         fib33 = fib 33
