//> using option -Wvalue-discard -Wnonunit-statement

object App:
  // Warning should be emitted
  1 + 2 // warning

  // Warning should be silenced
  (1 + 2): Unit


