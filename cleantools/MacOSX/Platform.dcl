definition module Platform

PlatformDependant win mac :== win

IF_MACOSX macosx not_macosx :== macosx
IF_WINDOWS windows not_windows :== not_windows

DirSeparator:=='/'
DirSeparatorString:=="/"

TempDir :== "/tmp"

application_path :: !{#Char} -> {#Char}
