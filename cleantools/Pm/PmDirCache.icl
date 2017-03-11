implementation module PmDirCache

// Search paths for a file

import StdArray,StdBool,StdEnum,StdList,StdFile,StdOrdList,StdStrictLists,StdMaybe
from StdLibMisc import :: Date{..}, :: Time{..}
import Directory

import UtilStrictLists, PmPath, UtilIO
import Platform

:: DirCacheElem :== (String,String,DateTime) // module name, module path, module modified

:: SimpleDirCache :== {!DirCacheElem}

:: SubdirElem = { subdir_name :: !{#Char}, subdir_cache :: !.SimpleDirCache}

:: SubdirCache :== {#.SubdirElem}

:: DirCache = { root :: !.SimpleDirCache, subdirs :: !.SubdirCache, paths :: ![#Pathname!] }

:: Warn = Warn String String [(String,String,DateTime)]

emptyDateTime :== ({year=0,month=0,day=0,dayNr=0},{hours=0,minutes=0,seconds=0})

DC_Setup :: !(List Pathname) !*Files -> (!(![String],![Warn],!.DirCache),!*Files)
DC_Setup paths files
	# paths	= remove_dup_paths paths
	# paths = [|winfix path \\ path<|-paths]				// local hack to fix dir names
	# (err,files_in_dir,files)	= setup paths files
	# (wrn,cache) = make_dir_cache files_in_dir
	# dir_cache = {root = cache, subdirs={}, paths=paths}
	= ((err,wrn,dir_cache),files)
where
	remove_dup_paths :: (List Pathname) -> *[#Pathname!]
	remove_dup_paths paths
		= u_reverse (remove_dup_paths_reversed paths [|]) [|]
	where
		remove_dup_paths_reversed :: !(List {#Char}) !*[#{#Char}!] -> *[#{#Char}!]
		remove_dup_paths_reversed (path:!paths) unique_paths
			| occurs_in path unique_paths
				= remove_dup_paths_reversed paths unique_paths
				= remove_dup_paths_reversed paths [|path:unique_paths]
		where
			occurs_in path [|path_e:paths]
				= path==path_e || occurs_in path paths
			occurs_in path [|]
				= False
		remove_dup_paths_reversed Nil unique_paths
			= unique_paths

		u_reverse :: *[#{#Char}!] *[#{#Char}!] -> *[#{#Char}!]
		u_reverse [|] r = r
		u_reverse [|e:l] r = u_reverse l [|e:r]

	setup [|] files
		= ([],[],files)
	setup [|path:paths] files
		# ((ok,path`),files)		= pd_StringToPath path files
		| not ok
			# (errs,cache,files)	= setup paths files
			= ([path:errs],cache,files)
		# ((err,dir),files)			= getDirectoryContents path` files
		| err <> NoDirError
			# (errs,cache,files)	= setup paths files
			= ([path:errs],cache,files)
		# dir = [(fileName,path,lastModified)
				\\ {fileName,fileInfo={pi_fileInfo={lastModified,isDirectory}}} <- dir
				| not isDirectory]
		# (errs,cache,files)		= setup paths files
		# cache						= dir ++ cache
		= (errs,cache,files)

	//winfix :: renames root directories
	winfix s = PlatformDependant
		(/*Win*/ if (size s > 0 && s.[size s - 1] == ':') (s +++. "\\") (s))
		(/*Mac*/ if (findPos ':' s 0 == ~1) (s +++. ":") (s))

	findPos c s i
		| i >= size s = ~1
		| s.[i] == c = i
		= findPos c s (inc i)

make_dir_cache :: [DirCacheElem] -> (![Warn],!*SimpleDirCache)
make_dir_cache cache
	# cache					= sortBy (\(l,_,_) (r,_,_) -> l<r) cache
	// must remove duplicates otherwise binsearch gives errors...
	# (wrn,cache)			= removedups cache
	= (wrn,{dir \\ dir<-cache})
where
	removedups :: ![DirCacheElem] -> ([Warn],[DirCacheElem])
	removedups [x=:(".DS_Store",_,_):xs]
		= removedups xs
	removedups [x=:(l,p,_):xs]
		# (wrn,xs)					= dropWarn (\(r,_,_) -> l==r) xs
		| isEmpty wrn
			# (wrn,xs)				= removedups xs
			= (wrn,[x:xs])
		# (wrn`,xs)					= removedups xs
		= ([Warn l p wrn:wrn`],[x:xs])
	removedups _      = ([],[])

	dropWarn :: (a -> Bool) ![a] -> ([a],[a])
	dropWarn f cons=:[a:x]	| f a	# (wrn,x) = dropWarn f x
									= ([a:wrn],x)
									= ([],cons)
	dropWarn f []					= ([],[])


DC_Update :: !(!String,!String,!DateTime) !*DirCache -> *DirCache
DC_Update (n`,p`,m`) dir_cache=:{root=cache}
	# cache = update_dir_cache (n`,p`,m`) cache
	= {dir_cache & root=cache}

DC_HUpdate :: !(!String,!String,!DateTime) !String !*DirCache !*Files -> (!*DirCache,!*Files)
DC_HUpdate (n`,p`,m`) file_ext dir_cache=:{root=cache,subdirs,paths} files
	# last_dot_i = find_last_dot_i n`
	| last_dot_i<0
		# cache = update_dir_cache (n`,p`,m`) cache
		= ({dir_cache & root=cache},files)
		# (subdir_name,file_name) = subdir_and_file_name_of_hmodule_name last_dot_i n` file_ext
		  (found,subdir_i,subdirs) = bin_search_subdir subdir_name subdirs
		| found
			# (subdir,subdirs) = subdirs![subdir_i]
			  cache = update_dir_cache (n`,p`,m`) subdir.subdir_cache
			  subdirs & [subdir_i]={subdir & subdir_cache=cache}
			= ({dir_cache & subdirs=subdirs},files)
			# (errs,files_in_dir,files) = setup_h paths subdir_name files
			  (wrn,cache) = make_dir_cache files_in_dir
			  cache = update_dir_cache (n`,p`,m`) cache
			  new_subdirs = add_subdir subdir_i subdir_name cache subdirs
			= ({dir_cache & subdirs=new_subdirs},files)

update_dir_cache :: !(!String,!String,!DateTime) !*SimpleDirCache -> *SimpleDirCache
update_dir_cache (n`,p`,m`) cache
	# (maxi,cache) = usize cache
	= binsearch 0 maxi maxi cache
where
	binsearch :: !Int !Int !Int !*SimpleDirCache -> *SimpleDirCache
	binsearch left right max cache
		| left >= right
			# newcache			= createArray (inc max) (n`,p`,m`)
			# (newcache,cache)	= copy (dec right) newcache cache 0 0
			# (newcache,cache)	= copy (max - right) newcache cache (inc right) right
			= newcache
		# mid					= (left+right)>>1
		# ((n,p,m),cache)		= cache![mid]
		| n` == n
			= {cache & [mid]=(n`,p`,m`)}
		| n` < n
			= binsearch left mid max cache
		// n` > n
			= binsearch (inc mid) right max cache
	
	copy :: !Int !*SimpleDirCache !*SimpleDirCache !Int !Int -> (!*SimpleDirCache, !*SimpleDirCache)
	copy num new old newbegin oldbegin
		| num <= 0	= (new,old)
		# (e,old)	= old![oldbegin]
		# new		= {new & [newbegin]=e}
		= copy (dec num) new old (inc newbegin) (inc oldbegin)

find_last_dot_i :: !{#Char} -> Int
find_last_dot_i s
	= find_last_dot_i s (size s-1)
where
	find_last_dot_i s i
		| i>=0
			| s.[i]=='.'
				= i
				= find_last_dot_i s (i-1)
			= i

concat_paths path subdir
	# size_path=size path
	| size_path>0 && path.[size_path-1]==DirSeparator
		= path+++subdir
		= path+++DirSeparatorString+++subdir

DC_Search :: !Modulename !*DirCache -> *(!Bool,!Pathname,!DateTime,!*DirCache)
DC_Search mod dir_cache=:{root=cache}
	# (found,path_name,date_time,cache) = bin_search_file mod cache
	= (found,path_name,date_time,{dir_cache & root=cache})

DC_HSearch :: !Modulename !String !*DirCache !*Files -> *(!Bool,!Pathname,!DateTime,!*DirCache,!*Files)
DC_HSearch mod file_ext dir_cache=:{root=cache,subdirs,paths} files
	# last_dot_i = find_last_dot_i mod
	| last_dot_i<0
		# (found,path_name,date_time,cache) = bin_search_file (mod+++file_ext) cache
		= (found,path_name,date_time,{dir_cache & root=cache},files)
		# (subdir_name,file_name) = subdir_and_file_name_of_hmodule_name last_dot_i mod file_ext
		  (found,subdir_i,subdirs) = bin_search_subdir subdir_name subdirs
		| found
			# (subdir,subdirs) = subdirs![subdir_i]
			  (found,path_name,date_time,cache) = bin_search_file file_name subdir.subdir_cache
			  subdirs & [subdir_i]={subdir & subdir_cache=cache}
			= (found,path_name,date_time,{dir_cache & subdirs=subdirs},files)
			# (errs,files_in_dir,files) = setup_h paths subdir_name files
			  (wrn,cache) = make_dir_cache files_in_dir
			  (found,path_name,date_time,cache) = bin_search_file file_name cache
			  new_subdirs = add_subdir subdir_i subdir_name cache subdirs
			= (found,path_name,date_time,{dir_cache & subdirs=new_subdirs},files)

setup_h :: ![#{#Char}!] !{#Char} !*Files -> *(![{#Char}],![DirCacheElem],!*Files)
setup_h [|] subdir files
	= ([],[],files)
setup_h [|path:paths] subdir files
	# subdir_path = concat_paths path subdir
	# ((ok,path`),files)		= pd_StringToPath subdir_path files
	| not ok
		# (errs,cache,files)	= setup_h paths subdir files
		= ([subdir_path:errs],cache,files)
	# ((err,dir),files)			= getDirectoryContents path` files
	| err <> NoDirError
		# (errs,cache,files)	= setup_h paths subdir files
		= ([subdir_path:errs],cache,files)
	# dir = [(fileName,path,lastModified)
			\\ {fileName,fileInfo={pi_fileInfo={lastModified,isDirectory}}} <- dir
			| not isDirectory]
	# (errs,cache,files)		= setup_h paths subdir files
	# cache						= dir ++ cache
	= (errs,cache,files)

add_subdir :: !Int !{#Char} !{!DirCacheElem} !*{#SubdirElem} -> *{#SubdirElem}
add_subdir subdir_i subdir_name cache subdirs
	# (n_subdirs,subdirs) = usize subdirs
	  new_subdirs={{subdir_name="",subdir_cache={}} \\ i<-[0..n_subdirs]}
	  (new_subdirs,subdirs) = move_begin_elements 0 subdir_i new_subdirs subdirs
	  new_subdirs & [subdir_i]={subdir_name=subdir_name, subdir_cache=cache}
	= move_end_elements_to_next subdir_i new_subdirs subdirs
where
	move_begin_elements :: !Int !Int !*SubdirCache !*SubdirCache -> (!*SubdirCache,!*SubdirCache)
	move_begin_elements i max_i d_a s_a
		| i<max_i
			# (e,s_a) = replace s_a i {subdir_name="",subdir_cache={}}
			# d_a & [i]=e
			= move_begin_elements (i+1) max_i d_a s_a
			= (d_a,s_a)

	move_end_elements_to_next :: !Int !*SubdirCache !*SubdirCache -> *SubdirCache
	move_end_elements_to_next i d_a s_a
		| i<size s_a
			# (e,s_a) = replace s_a i {subdir_name="",subdir_cache={}}
			# d_a & [i+1]=e
			= move_end_elements_to_next (i+1) d_a s_a
			= d_a

bin_search_file :: !Modulename !*SimpleDirCache -> *(!Bool,!Pathname,!DateTime,!*SimpleDirCache)
bin_search_file mod cache
	# (cache_size,cache) = usize cache
	= bin_search_file 0 cache_size mod cache
where
	bin_search_file :: !Int !Int !Modulename !*SimpleDirCache -> *(!Bool,!Pathname,!DateTime,!*SimpleDirCache)
	bin_search_file left right mod cache
		| left >= right
			= (False,"",emptyDateTime,cache)
		# mid				= (left+right)>>1
		# ((n,p,m),cache)	= cache![mid]
		| mod == n
			= (True,p,m,cache)
		| mod < n
			= bin_search_file left mid mod cache
		// mod > n
			= bin_search_file (inc mid) right mod cache

bin_search_subdir subdir_name subdirs
	# (n_subdirs,subdirs) = usize subdirs
	= bin_search_subdir 0 n_subdirs subdir_name subdirs
where
	bin_search_subdir :: !Int !Int !{#Char} !*SubdirCache -> *(!Bool,!Int,!*SubdirCache)
	bin_search_subdir left right subdir cache
		| left >= right
			= (False,right,cache)
		# mid = (left+right)>>1
		# (subdir_name,cache) = cache![mid].subdir_name
		| subdir == subdir_name
			= (True,mid,cache)
		| subdir < subdir_name
			= bin_search_subdir left mid subdir cache
		// subdir > subdir_name
			= bin_search_subdir (inc mid) right subdir cache

SearchDisk :: !Modulename !(List Pathname) !*Files -> (!(!Bool,!Pathname),!*Files)
SearchDisk modname dirs files
	= SearchDisk2 modname dirs files

FindHModule :: !Modulename !{#Char} !(List Pathname) !*Files -> (!(!Bool,!Pathname),!*Files)
FindHModule module_name file_ext dirs files
	# last_dot_i = find_last_dot_i module_name
	| last_dot_i<0
		= SearchDisk2 (module_name+++file_ext) dirs files
		# (subdir_name,file_name) = subdir_and_file_name_of_hmodule_name last_dot_i module_name file_ext
		# dirs = Map (\dir->concat_paths dir subdir_name) dirs
		= SearchDisk2 file_name dirs files

SearchDisk2 :: !Modulename !(List Pathname) !*Files -> (!(!Bool,!Pathname),!*Files)
SearchDisk2 modname Nil files
	=  ((False, EmptyPathname),files)
SearchDisk2 modname (dir:!rest) files
	# dir_modname1				= MakeFullPathname dir modname
	# (exists,files)			= FExists dir_modname1 files
	| exists
		= ((True, dir_modname1),files)
		= SearchDisk2 modname rest files
