*Main> print hello
Put "What is your name?" (
  Get ($0 -> 
    Put "What is your age?" (
      Get ($1 -> 
        Put "Hello $0!" (
          Put "You are $1 years old" (
            Return ()
          )
        )
      )
    )
  )
)