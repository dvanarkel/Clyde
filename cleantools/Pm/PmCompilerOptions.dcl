definition module PmCompilerOptions

import StdClass

::	ListTypes	= NoTypes | InferredTypes | StrictExportTypes | AllTypes

instance == ListTypes
instance toString ListTypes
instance fromString ListTypes

//	The Compiler Options: default settings for the compiler.
::	CompilerOptions	=
	{	neverMemoryProfile	:: !Bool
	,	neverTimeProfile	:: !Bool
	,	sa					:: !Bool		// strictness analysis
	,	listTypes			:: !ListTypes	// how to present inferred types
	,	attr				:: !Bool		// show attributes with inferred types
	,	gw					:: !Bool		// give warnings
	,	bv					:: !Bool		// be verbose
	,	gc					:: !Bool		// generate commented abc-code
	,	reuseUniqueNodes	:: !Bool 
	,	fusion				:: !Bool
	}

DefaultCompilerOptions		:: CompilerOptions
