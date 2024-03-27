Setup
  $ source "$TESTDIR/setup.sh"

  $ glean create --finish --db abc/0
  Creating DB.* (re)
  $ glean create --finish --db abc/1
  Creating DB.* (re)
  $ glean create --finish --db xyz/0
  Creating DB.* (re)

  $ glean list ab
  

  $ glean list abc
  abc/0 (complete)
    Created:.* (re)
    Completed:.* (re)
      
  abc/1 (complete)
    Created:.* (re)
    Completed:.* (re)
      




  $ glean list abc/
  abc/0 (complete)
    Created:.* (re)
    Completed:.* (re)
      
  abc/1 (complete)
    Created:.* (re)
    Completed:.* (re)
      



  $ glean list *b*/*
  abc/0 (complete)
    Created:.* (re)
    Completed:.* (re)
      
  abc/1 (complete)
    Created:.* (re)
    Completed:.* (re)
      




  $ glean list x*/0
  xyz/0 (complete)
    Created:.* (re)
    Completed:.* (re)
      
