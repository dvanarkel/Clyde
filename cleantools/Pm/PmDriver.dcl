definition module PmDriver

from PmProject		import :: Project, :: Pathname
from IdeState		import :: General
from PmCleanSystem	import :: CompileOrCheckSyntax

import PmCallBack

:: SetMadeProjectFun :==
	(	Bool
	->	Bool
	->	Project
	->	GeneralSt -> GeneralSt)

:: CleanupCont :==
	Pathname
	Bool
	Bool
	*GeneralSt -> *GeneralSt

CompileProjectModule ::					// Compile or Syntax-check a single module
	!CompileOrCheckSyntax
	!Pathname
	!Project
	!SetMadeProjectFun
	!*GeneralSt -> *GeneralSt

GenAsmProjectModule ::					// Generate assembly for a single module
	!.Pathname
	!Project
	!SetMadeProjectFun
	!*GeneralSt -> *GeneralSt

BringProjectUptoDate ::					// Bring complete project up-to-date
	!Bool								// force recompile...
	CleanupCont
	!*GeneralSt -> *GeneralSt
