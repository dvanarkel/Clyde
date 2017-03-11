# Clyde
A Clean IDE for Mac OS X


This is a work in progress towards an integrated development environment for [Clean](http://clean.cs.ru.nl/Clean)  similar to the [CleanIDE for Windows](http://clean.cs.ru.nl/Clean_IDE). 

## status
This v0.0 is not yet directly useable due to:

* hardcoding for local system paths
* build not yet functioning
* code signing (is this actually needed?)(note that it also calls through to local backup script, not included)

## Mac OS X version supported?
* No Mac OS supported version is currently explicitly set, therefore the built application will default to requiring the OS version on which  it was built. 
* No explicit testing has been done on OS versions prior to OS X 10.11 (El Capitan), or later for that matter :-)

## Reuse
We currently include 

- prebuilt static libraries for dyncall v0.9 <www.dyncall.org>
- relevant sources from 'clean tools' development repository from the Clean team
- Titto icons from [Alejandro Lopez](http://musett.com/)

## Contributing
Contributions welcome (code, comments, issue reports)