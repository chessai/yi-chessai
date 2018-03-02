# bytestring-encodings

This library aims to provide an efficient way to determine if a given ByteString
is encoded according to a certain encoding scheme, e.g. ASCII or UTF8.

Currently available are efficient implementations of 'isAscii' and 'isUtf8'.

'isAscii' is consistently 8x faster than the naive implementation of checking
that each byte is less than 128 (see benchmarks). If compiling with LLVM, this
can be made even faster with vectorising (see [GHC's Operations on SIMD Vectors](https://hackage.haskell.org/package/ghc-prim-0.5.1.1/docs/GHC-Prim.html#g:29)).

I don't know of any other 'isUtf8' function which acts on ByteStrings, to perform
this exact same check, so there are no benchmarks comparing to another function in
the UTF-8 range, however there are still benchmarks for the function itself.

PRs welcome if anyone would like to add support for other encodings. 
