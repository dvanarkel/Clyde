# Clyde
A Clean IDE for Mac OS X


This is a work in progress towards an integrated development environment for [Clean](http://clean.cs.ru.nl/Clean)  similar to the [CleanIDE for Windows](http://clean.cs.ru.nl/Clean_IDE). 

## status
This v0.0 is not yet directly useable due to:

* build not yet functioning
* code signing (is this actually needed?)(note that it also calls through to local backup script, not included)
## Using ##

- Clyde is automatically bound to files of type \*.prj, \*.icl and \*.dcl
- Clyde needs to know the path to your Clean installation. Use `defaults write com.mac.dvanarkel.Clyde CLEAN_HOME <path>` from your Terminal to inform it of the correct path. 

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

## Building ##

Building Clyde requires

- installation of a recent Nightly build of Clean for Mac OS X (http://clean.cs.ru.nl/Download_Clean).
- installation of the Xcode command line tools, if you don't have these already you can install them by running `xcode-select --install` from a Terminal session and following the prompts
- open the Terminal in the Clyde.git directory
- Run `.\cpm Clyde.prj`  (note the .\ in order to pick up the local copy of `cpm`)
- Run `lsregister -f Clyde.app` in order to have the OS pick up any changes to the Clyde.app properties
- Append `StdEnvClyde.env` to the IDEEnvs file in your Clean installation

## Working With Git ##

Append the following to your `.git/config` in order to clean out Clean project files automatically when committing to the repository:
```
[filter "vacuum"]
	clean = sed '/^OtherModules$/,$d'
	smudge = cat
```
Where the filter is triggered as the `.gitattributes` has been modified to include:
```
# hook to vacuum Clean projects on commit
*.prj	filter=vacuum
```

## Roadmap ##

### v0.1 ###
- [x] upload to github
- [x] fix hardcoding to Clean installation
- [ ] get `Build` functioning (as well as Build & Run and Run)
- [ ] aborts when trying to open file it can't find
- [ ] cleanup Project menu
- [ ] figure out if code signing required

### v0.2 ###
- [ ] update project window after build
- [ ] error, message and type windows for build output
- [ ] after first save it appears later edit's do not immediately mark file as dirty
- [ ] save all before Build
- [ ] shift-doubleclick doesn't have desired effect in project window (should open definition module)


### Unplanned ###
- [ ] still some issues with syntax colouring vs unicode
- [ ] project global Clean sensitive search
- [ ] allow opening project files 'as text'
- [ ] enable project menu items at all times.. probably requires restricting ourselves to a single project window?
- [ ] update module mangling so that hierarchical modules are part of directory name instead of module name
	eg				{Project}.Cocoa		dyncall
	rather than		{Project}			Cocoa.dyncall
- [ ] cascade windows when opening
- [ ] add line numbers
- [ ] integrate cloogle
- [ ] api documentation
- [ ] xref
- [ ] cross compilation
- [ ] consider moving to collection of *.env files under /etc instead of monolithic 
IDEEnvs
- [ ] logo in README once we have sorted licensing
- [ ] edit IDE options (CLEAN_HOME)
- [ ] edit project options
- [] weird line wrapping on new files
### Others ###

- [ ] Update .gitignore
- [ ] Sort out icon licensing

