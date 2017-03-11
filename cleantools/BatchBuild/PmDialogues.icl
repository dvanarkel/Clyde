implementation module PmDialogues

import StdPathname, UtilStrictLists

doPathsDialog :: !String !Pathname !Pathname !(List Pathname) ((List Pathname) .state -> .state) .state -> .state
doPathsDialog _ _ _ _ f ps = ps
