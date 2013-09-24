default: she

she: Main.lhs Parsley.lhs Patsy.lhs TypesToKinds.lhs HaLay.lhs Imports.lhs Aspect.lhs Superclass.lhs
	ghc --make Main -o she

install: she
	sudo cp she /usr/local/bin/
# DO NOT DELETE: Beginning of Haskell dependencies
Parsley.o : Parsley.lhs
HaLay.o : HaLay.lhs
HaLay.o : Parsley.hi
Imports.o : Imports.lhs
Imports.o : Parsley.hi
Imports.o : HaLay.hi
Aspect.o : Aspect.lhs
Aspect.o : Parsley.hi
Aspect.o : HaLay.hi
DeBruijn.o : DeBruijn.lhs
DeBruijn.o : HaLay.hi
DeBruijn.o : Parsley.hi
Patsy.o : Patsy.lhs
Patsy.o : HaLay.hi
Patsy.o : Parsley.hi
TypesToKinds.o : TypesToKinds.lhs
TypesToKinds.o : Parsley.hi
TypesToKinds.o : HaLay.hi
IdiomBrackets.o : IdiomBrackets.lhs
IdiomBrackets.o : Parsley.hi
IdiomBrackets.o : HaLay.hi
Main.o : Main.lhs
Main.o : IdiomBrackets.hi
Main.o : TypesToKinds.hi
Main.o : Patsy.hi
Main.o : DeBruijn.hi
Main.o : Aspect.hi
Main.o : Imports.hi
Main.o : HaLay.hi
# DO NOT DELETE: End of Haskell dependencies
