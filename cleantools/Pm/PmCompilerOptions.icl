implementation module PmCompilerOptions

import StdClass, StdBool
import StdFile

Unexpected message defaultValue
	:== UnexpectedEvalBefore defaultValue (UnexpectedReport message)

UnexpectedEvalBefore :: !.a !.b -> .a
UnexpectedEvalBefore defaultValue _
	=	defaultValue

Don`tCareValue :== 0

UnexpectedReport :: !{#Char} -> Int
UnexpectedReport message
	=	UnexpectedEvalBefore Don`tCareValue (fwrites message stderr)

//--	ListTypes: Options for the listing of derived types

::	ListTypes	= NoTypes | InferredTypes | StrictExportTypes | AllTypes

instance == ListTypes
where
	(==) :: !ListTypes !ListTypes -> Bool
	(==) NoTypes NoTypes
		=	True
	(==) InferredTypes InferredTypes
		=	True
	(==) StrictExportTypes StrictExportTypes
		=	True
	(==) AllTypes AllTypes
		=	True
	(==) _ _
		=	False

instance toString ListTypes
where
	toString :: !ListTypes -> {#Char}
	toString NoTypes
		=	"NoTypes"
	toString InferredTypes
		=	"InferredTypes"
	toString StrictExportTypes
		=	"StrictExportTypes";
	toString AllTypes
		=	"AllTypes"

instance fromString ListTypes
where
	fromString :: !{#Char} -> ListTypes
	fromString "NoTypes"
		=	NoTypes
	fromString "InferredTypes"
		=	 InferredTypes
	fromString "StrictExportTypes"
		=	StrictExportTypes
	fromString "AllTypes"
		=	AllTypes
	fromString _
		=	Unexpected "fromString (Types): unknown Type" NoTypes

//--	CompilerOptions: default settings for the compiler.

::	CompilerOptions	=
	{	neverMemoryProfile	:: !Bool		// memory profiling
	,	neverTimeProfile	:: !Bool		// time profiling
	,	sa					:: !Bool		// strictness analysis
	,	listTypes			:: !ListTypes	// derived type reporting
	,	attr				:: !Bool		// show attributes
	,	gw					:: !Bool		// give warnings
	,	bv					:: !Bool		// be verbose
	,	gc					:: !Bool		// generate comments
	,	reuseUniqueNodes	:: !Bool 		// reuse unique nodes
	,	fusion				:: !Bool
	}

DefaultCompilerOptions :: CompilerOptions
DefaultCompilerOptions =
	{ neverMemoryProfile	= False
	, neverTimeProfile		= False
	, sa					= True
	, listTypes				= StrictExportTypes
	, attr					= True
	, gw					= True
	, bv					= True
	, gc					= False
	, reuseUniqueNodes		= True
	, fusion				= False
	}

