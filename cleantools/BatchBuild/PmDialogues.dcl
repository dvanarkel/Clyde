definition module PmDialogues

import StdPathname, UtilStrictLists

doPathsDialog :: !String !Pathname !Pathname !(List Pathname) ((List Pathname) .state -> .state) .state -> .state
