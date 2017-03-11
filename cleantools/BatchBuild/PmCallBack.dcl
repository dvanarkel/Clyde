definition module PmCallBack

from StdFile import class FileSystem,class FileEnv
from IdeState import ::GeneralSt,instance FileSystem GeneralSt,instance FileEnv GeneralSt

start :: !.a !(.Bool -> .(.a -> .(*GeneralSt -> *(.a,*GeneralSt)))) !*GeneralSt -> *GeneralSt
cont :: !*(!.a,!*GeneralSt) -> *(.a,!*GeneralSt);
stop :: !*(.a,!*GeneralSt) -> *(.a,!*GeneralSt);

IF_BATCHBUILD_OR_IDE batchbuild ide :== batchbuild