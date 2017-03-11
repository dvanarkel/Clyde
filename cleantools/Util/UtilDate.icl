implementation module UtilDate

import StdArray, StdBool, StdChar, StdClass, StdInt, StdString

NoDate	:== {exists=False,yy=0,mm=0,dd=0,h=0,m=0,s=0};

::	DATE = {	exists	:: !Bool,
				yy		:: !Int,
				mm		:: !Int,
				dd		:: !Int,
				h		:: !Int,
				m		:: !Int,
				s		:: !Int };

instance fromString DATE
where
	fromString string
		=	ConvertToDate string
	where
		ConvertToDate :: !String -> DATE
		ConvertToDate str
			| colpos >= strlen	= NoDate
								= {	exists	= first == 'y' || first == 'Y',
									yy		= SubStringToInt yypos mmpos str,
									mm		= SubStringToInt mmpos ddpos str,
									dd		= SubStringToInt ddpos hpos str,
									h		= SubStringToInt hpos mpos str,
									m		= SubStringToInt mpos spos str,
									s		= SubStringToInt spos strlen str }
		where
			yypos	= SkipSpaces (FindSpace colpos strlen str) strlen str
			mmpos	= SkipSpaces (FindSpace yypos strlen str) strlen str
			ddpos	= SkipSpaces (FindSpace mmpos strlen str) strlen str
			hpos	= SkipSpaces (FindSpace ddpos strlen str) strlen str
			mpos	= SkipSpaces (FindSpace hpos strlen str) strlen str
			spos	= SkipSpaces (FindSpace mpos strlen str) strlen str
			first	= str.[colpos]
			colpos	= 0 // FindColon 0 strlen str
			strlen	= size str
		
		SubStringToInt	:: !Int !Int !String -> Int
		SubStringToInt fro to string =  StringToInt2 0 fro to string
		
		StringToInt2 :: !Int !Int !Int !String -> Int
		StringToInt2 acc fro to str
			| fro >= to		= acc
			| isDigit cur	= StringToInt2 acc` (inc fro) to str
							= acc
		where
			acc`	= 10 * acc + (toInt cur) - (toInt '0')
			cur		= str.[fro]
		
		FindSpace :: !Int !Int !String -> Int
		FindSpace i len str	| i>= len || str .[ i] == ' '	= i
															= FindSpace (inc i) len str
		
		SkipSpaces	:: !Int !Int !String -> Int
		SkipSpaces i len str | i >= len || str .[ i] <> ' '	= i
			                     							= SkipSpaces (inc i) len str

instance toString DATE
where
	toString date
		=	DateToString date
	where
		DateToString :: !DATE -> String
		DateToString {exists,yy,mm,dd,h,m,s}
			=	BoolToAnswer exists +++ " " +++
				toString yy +++ " " +++ toString mm +++ " " +++ toString dd +++ " " +++
				toString h +++ " " +++ toString m +++ " " +++ toString s
		
		BoolToAnswer :: !Bool  -> String
		BoolToAnswer False =  "No"
		BoolToAnswer true  =  "Yes"

/*	Checks whether the first date is older than the second one.
*/
Older_Date	:: !DATE !DATE -> Bool;
Older_Date	{exists=b1,yy=yy1,mm=mm1,dd=dd1,h=h1,m=m1,s=s1}
			{exists=b2,yy=yy2,mm=mm2,dd=dd2,h=h2,m=m2,s=s2}
	| b1		&& not b2	= True;
	| not b1	|| not b2	= False;
							= Older_IntList [yy1,mm1,dd1,h1,m1,s1] [yy2,mm2,dd2,h2,m2,s2];
where
	Older_IntList	:: ![Int] ![Int] -> Bool;
	Older_IntList []		[]					=  False;
	Older_IntList [h1:t1]	[h2:t2]	| h1 < h2	=  True;
									| h1 > h2	=  False;
												=  Older_IntList t1 t2;
