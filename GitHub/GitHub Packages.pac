| package |
package := Package name: 'GitHub Packages'.
package paxVersion: 1;
	basicComment: 'GitHub Packages
 - Download and install Dolphin Smalltalk Packages and their prerequisites directly from GitHub
 - Simplifies the loading of complex frameworks

To download and install a package from GitHub evaluate:

GitHubPackageManager install: ''<github username>/<repository name>/<package path>''

Example:

GitHubPackageManager install: ''rko281/MethodSpies/Method Spies''

Package name can be omitted where it matches the repository name:

GitHubPackageManager install: ''rko281/MethodSpies''

Any required prerequisite packages will also be downloaded (this includes prerequisites from other repositories where these can be located)

John Aspinall 2020'.


package classNames
	add: #GHIShellDispatch;
	add: #GHShell32Folder3;
	add: #GHShell32FolderItem;
	add: #GHShell32FolderItems;
	add: #GitHubBranch;
	add: #GitHubPackageManager;
	add: #GitHubRepository;
	yourself.

package methodNames
	add: #PackageManager -> #installUntrapped:;
	yourself.

package globalNames
	add: #GHShell32Constants;
	add: #GHShell32Lib;
	yourself.

package binaryGlobalNames: (Set new
	add: #GHShell32Lib;
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\Core\Object Arts\Dolphin\ActiveX\Automation\ActiveX Automation'
	'..\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\Core\Object Arts\Dolphin\ActiveX\COM\OLE COM'
	'..\Core\Object Arts\Dolphin\ActiveX\Structured Storage\OLE Structured Storage').

package!

"Class Definitions"!

Object subclass: #GitHubBranch
	instanceVariableNames: 'repository name tempFilename packageFolder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GitHubPackageManager
	instanceVariableNames: 'includeMap repositories shellObject isHeadless shouldExtractEntireRootRepository extractFileExtensions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #GitHubRepository
	instanceVariableNames: 'manager name ownerName namedBranches'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IDispatch subclass: #GHIShellDispatch
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'GHShell32Constants'
	classInstanceVariableNames: ''!
IDispatch subclass: #GHShell32Folder3
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
IDispatch subclass: #GHShell32FolderItem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'GHShell32Constants'
	classInstanceVariableNames: ''!
IDispatch subclass: #GHShell32FolderItems
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'GHShell32Constants'
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!PackageManager methodsFor!

installUntrapped: aString 
	"Identical to install except missing prerequisite signals are passed through (with additional info), not masked"

	| newPackages startPackage |
	startPackage := self loadPackage: aString.
	(self includesPackageNamed: startPackage name) 
		ifTrue: [^self errorNameIsNotUnique: startPackage name].
	
	[| filename |
	filename := startPackage packageFileName.
	newPackages := self 
				loadPrereqsForPackage: startPackage
				relativeTo: startPackage path
				extension: (File splitExtensionFrom: filename)
				loaded: (PluggableSet searchPolicy: SearchPolicy caseInsensitive)
				trail: OrderedCollection new] 
			on: PackageNotFoundSignal
			do: 
				[:ex || sourceFrame |
				"Tag the exception with the base path from which the missing prereq could not be found"
				sourceFrame := ex raisingFrame sender sender.
				ex tag: (sourceFrame arguments at: (sourceFrame tempNames indexOf: 'basePathString')).
				ex pass].
	self assert: [newPackages last == startPackage].
	self beNotProcessingEvents.
	
	[newPackages do: 
			[:each | 
			self addPackage: each.
			[
				self basicInstall: each.
				self trigger: #packageInstalled: with: each.
			] ifCurtailed: [self basicUninstall: each].
			self observePackage: each]] 
			ensure: 
				[self beProcessingEvents.
				self loadedChanged].
	^newPackages! !
!PackageManager categoriesFor: #installUntrapped:!operations!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #GHShell32Constants put: (PoolConstantsDictionary named: #GHShell32Constants)!
GHShell32Constants at: 'OFS_DIRTYCACHE' put: 16r3!
GHShell32Constants at: 'OFS_INACTIVE' put: -16r1!
GHShell32Constants at: 'OFS_OFFLINE' put: 16r1!
GHShell32Constants at: 'OFS_ONLINE' put: 16r0!
GHShell32Constants at: 'OFS_SERVERBACK' put: 16r2!
GHShell32Constants at: 'SFVVO_DESKTOPHTML' put: 16r200!
GHShell32Constants at: 'SFVVO_DOUBLECLICKINWEBVIEW' put: 16r80!
GHShell32Constants at: 'SFVVO_SHOWALLOBJECTS' put: 16r1!
GHShell32Constants at: 'SFVVO_SHOWCOMPCOLOR' put: 16r8!
GHShell32Constants at: 'SFVVO_SHOWEXTENSIONS' put: 16r2!
GHShell32Constants at: 'SFVVO_SHOWSYSFILES' put: 16r20!
GHShell32Constants at: 'SFVVO_WIN95CLASSIC' put: 16r40!
GHShell32Constants at: 'ssfALTSTARTUP' put: 16r1D!
GHShell32Constants at: 'ssfAPPDATA' put: 16r1A!
GHShell32Constants at: 'ssfBITBUCKET' put: 16rA!
GHShell32Constants at: 'ssfCOMMONALTSTARTUP' put: 16r1E!
GHShell32Constants at: 'ssfCOMMONAPPDATA' put: 16r23!
GHShell32Constants at: 'ssfCOMMONDESKTOPDIR' put: 16r19!
GHShell32Constants at: 'ssfCOMMONFAVORITES' put: 16r1F!
GHShell32Constants at: 'ssfCOMMONPROGRAMS' put: 16r17!
GHShell32Constants at: 'ssfCOMMONSTARTMENU' put: 16r16!
GHShell32Constants at: 'ssfCOMMONSTARTUP' put: 16r18!
GHShell32Constants at: 'ssfCONTROLS' put: 16r3!
GHShell32Constants at: 'ssfCOOKIES' put: 16r21!
GHShell32Constants at: 'ssfDESKTOP' put: 16r0!
GHShell32Constants at: 'ssfDESKTOPDIRECTORY' put: 16r10!
GHShell32Constants at: 'ssfDRIVES' put: 16r11!
GHShell32Constants at: 'ssfFAVORITES' put: 16r6!
GHShell32Constants at: 'ssfFONTS' put: 16r14!
GHShell32Constants at: 'ssfHISTORY' put: 16r22!
GHShell32Constants at: 'ssfINTERNETCACHE' put: 16r20!
GHShell32Constants at: 'ssfLOCALAPPDATA' put: 16r1C!
GHShell32Constants at: 'ssfMYPICTURES' put: 16r27!
GHShell32Constants at: 'ssfNETHOOD' put: 16r13!
GHShell32Constants at: 'ssfNETWORK' put: 16r12!
GHShell32Constants at: 'ssfPERSONAL' put: 16r5!
GHShell32Constants at: 'ssfPRINTERS' put: 16r4!
GHShell32Constants at: 'ssfPRINTHOOD' put: 16r1B!
GHShell32Constants at: 'ssfPROFILE' put: 16r28!
GHShell32Constants at: 'ssfPROGRAMFILES' put: 16r26!
GHShell32Constants at: 'ssfPROGRAMFILESx86' put: 16r30!
GHShell32Constants at: 'ssfPROGRAMS' put: 16r2!
GHShell32Constants at: 'ssfRECENT' put: 16r8!
GHShell32Constants at: 'ssfSENDTO' put: 16r9!
GHShell32Constants at: 'ssfSTARTMENU' put: 16rB!
GHShell32Constants at: 'ssfSTARTUP' put: 16r7!
GHShell32Constants at: 'ssfSYSTEM' put: 16r25!
GHShell32Constants at: 'ssfSYSTEMx86' put: 16r29!
GHShell32Constants at: 'ssfTEMPLATES' put: 16r15!
GHShell32Constants at: 'ssfWINDOWS' put: 16r24!
GHShell32Constants shrink!

"Classes"!

GitHubBranch guid: (GUID fromString: '{3e13edd0-8713-42bb-9a51-3bfb3b37d80b}')!
GitHubBranch comment: ''!
!GitHubBranch categoriesForClass!Kernel-Objects! !
!GitHubBranch methodsFor!

beDefaultBranch

	name := nil!

displayOn: aStream

	aStream nextPutAll: self name!

download

	"Download the entire repository as an archive (zip) file, extracting all files to the appropriate location"

	| destination destinationFolder zipRepositoryItems |

	zipRepositoryItems := self packageFolder items.

	destination := File fullPathOf: self repository name relativeTo: SessionManager current imageBase.
	(File exists: destination) ifFalse: [File createDirectoryPath: destination].
	destinationFolder := self manager shellObject nameSpace: destination.

	"Extract the files"
	destinationFolder copyHere: zipRepositoryItems vOptions: self manager copyHereVOptions.

	destinationFolder free.
	zipRepositoryItems free!

download: aString

	"Download the entire repository as an archive (zip) file, extracting all files at the folder level of the package whose path is aString"

	| stream packagePath fullPackagePath installFolder zipFolder zipItems |

	packagePath := File splitPathFrom: aString.
	fullPackagePath := File fullPathOf: packagePath relativeTo: SessionManager current imageBase.
	(File exists: fullPackagePath) ifFalse: [File createDirectoryPath: fullPackagePath].
	installFolder := self manager shellObject nameSpace: fullPackagePath.

	zipFolder := self packageFolder.
	stream := packagePath readStream.
	stream skipThrough: File pathDelimiter. "skip over repository name"
	stream atEnd ifFalse: [zipFolder := (zipFolder parseName: stream upToEnd) getFolder].

	zipItems := zipFolder items.
	zipItems filter: 16r40 "SHCONTF_NONFOLDERS" bstrFileSpec: self manager extractFilePattern.

	"Extract the files"
	installFolder copyHere: zipItems vOptions: self manager copyHereVOptions.

	zipItems free.
	installFolder free!

downloadArchive

	"Private - Download the entire repository as an archive (zip) file, creating the receiver's packageFolder"

	| tailStream shell zipFile zipFolder zipItems |

	tempFilename := [URLMonLibrary default urlDownloadToCacheFile: self zipballUrl] on: HRESULTError do: 
		[ :exc |
		"File not found. Try default branch if we aren't already"
		self isDefaultBranch ifTrue: [exc pass].
		Notification signal: ('GitHubPackageManager: <1d> <2d> branch not found. Trying default branch...' expandMacrosWith: repository with: self).
		self beDefaultBranch.
		exc retry].

	"Update the repository owner and name based on the zipball filename to ensure we are using the same case"
	tailStream := (File splitFilenameFrom: tempFilename) readStream.
	self repository 
		ownerName: (tailStream upTo: $-);
		name: (tailStream upTo: $-).

	shell := self manager shellObject.
	zipFile := shell nameSpace: tempFilename.
	zipFolder := shell nameSpace: (File composePath: tempFilename subPath: (zipFile items item: 0) name).

	"Check for include item"
	zipItems := zipFolder items.
	zipItems filter: 16r40 "SHCONTF_NONFOLDERS" bstrFileSpec: 'include'.
	zipItems count = 1 ifTrue:  [self extractAndReadIncludeFrom: zipItems].

	packageFolder := self packageFolderIn: zipFolder.

	zipItems free.
	zipFile free!

extractAndReadIncludeFrom: aFolderItems

	"Extract the include file which is the sole elements of aFolderItems.
	Read and update the manager's includeMap"

	| tempPath tempFolder tempFilename |

	tempPath := File composePath: File tempPath subPath: GUID newUnique asString.
	File createDirectory: tempPath.
	tempFolder := self manager shellObject nameSpace: tempPath.
	tempFilename := File composePath: tempPath subPath: 'include'.

	tempFolder copyHere: aFolderItems vOptions: 4 "Very short operation - do not display progress".
	self manager readIncludeFrom: tempPath.
	
	File delete: tempFilename.
	File deleteDirectory: tempPath.
	tempFolder free.
	aFolderItems free!

free

	packageFolder ifNotNil: [ :pf | pf free].
	tempFilename ifNotNil: [ :filename | (File exists: filename) ifTrue: [[File delete: filename] on: Win32Error do: [ :exc | exc return]]].

	packageFolder := nil.
	tempFilename := nil!

isDefaultBranch

	^name isNil!

isPackageFilename: aString

	^##({Package sourcePackageExtension. 
		Package packageExtension}) includes: (File splitExtensionFrom: aString) asLowercase!

isSmalltalkFilename: aString

	^##({'cls'. 
		Package sourcePackageExtension. 
		Package packageExtension. 
		Package sourceGlobalExtension. 
		Package binaryGlobalExtension}) includes: (File splitExtensionFrom: aString) asLowercase!

manager

	^self repository manager!

name
	^name ifNil: ['master']!

name: anObject
	name := anObject!

packageFolder
	^packageFolder ifNil: 
		[self downloadArchive. 
		self packageFolder]!

packageFolderIn: anArchiveFolder

	"There are two supported ways for package items (code) to be stored in the repository:
	1) code items are stored beginning in root of repository
	2a) code items are stored beginning in a directory with the same name as the repository (there are no other package files or package-containing directories in root of repository)
	2b) code is stored in a directory named 'src' in the root of the repository
	Option 1 is simple for small repositories
	Option 2 is useful to avoid the main GitHub repository page being a huge list of package and associated files, obscuring the readme at the bottom.
	Note option 2a is deprecated in favour of option 2b and will be removed once repositories are migrated to the 'src' structure

	In the case of option 1 we locate packages in <working directory>/<repository name>/<code item path>
	In the case of option 2 we locate packages in <working directory>/<code item path>, i.e. we do not duplicate the repository name

	Return the appropriate code-holding folder in anArchiveFolder"

	| items folders |

	"Additional special case - the Dolphin home repository is named 'Dolphin' but stores all packages in its 'Core' directory. We treat this like option 2, i.e. do not prefix package path with repository name"
	self repository isDolphinCore ifTrue: [^(anArchiveFolder parseName: 'Core') getFolder].

	"2b"
	(anArchiveFolder parseName: 'src') ifNotNil: [ :item | ^item getFolder].

	items := anArchiveFolder items.
	items := (0 to: (items count - 1)) collect: [ :index | items item: index].
	folders := items select: [ :each | each isFolder].

	^(folders size = 1 and: [folders first name = self repository name and: [items allSatisfy: [ :each | each isFolder or: [(self isPackageFilename: each name) not]]]])
		ifTrue: [folders first getFolder "2a"]
		ifFalse: [anArchiveFolder "1"]!

printOn: aStream

	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $).!

repository
	^repository!

repository: anObject
	repository := anObject!

zipballUrl

	^self repository url, '/zipball/', self name! !
!GitHubBranch categoriesFor: #beDefaultBranch!initialize/release!public! !
!GitHubBranch categoriesFor: #displayOn:!printing!public! !
!GitHubBranch categoriesFor: #download!operations!public! !
!GitHubBranch categoriesFor: #download:!operations!public! !
!GitHubBranch categoriesFor: #downloadArchive!operations!private! !
!GitHubBranch categoriesFor: #extractAndReadIncludeFrom:!operations!public! !
!GitHubBranch categoriesFor: #free!initialize/release!private! !
!GitHubBranch categoriesFor: #isDefaultBranch!public!testing! !
!GitHubBranch categoriesFor: #isPackageFilename:!private!testing! !
!GitHubBranch categoriesFor: #isSmalltalkFilename:!private!testing! !
!GitHubBranch categoriesFor: #manager!accessing!public! !
!GitHubBranch categoriesFor: #name!accessing!public! !
!GitHubBranch categoriesFor: #name:!accessing!private! !
!GitHubBranch categoriesFor: #packageFolder!accessing!public! !
!GitHubBranch categoriesFor: #packageFolderIn:!private!testing! !
!GitHubBranch categoriesFor: #printOn:!printing!public! !
!GitHubBranch categoriesFor: #repository!accessing!public! !
!GitHubBranch categoriesFor: #repository:!accessing!private! !
!GitHubBranch categoriesFor: #zipballUrl!accessing!public! !

GitHubPackageManager guid: (GUID fromString: '{b779eae5-ae52-40f5-95fe-ad42922c021e}')!
GitHubPackageManager comment: ''!
!GitHubPackageManager categoriesForClass!Kernel-Objects! !
!GitHubPackageManager methodsFor!

beHeadless
	self isHeadless: true!

copyHereVOptions

	"Private - Return the vOptions parameter to use for copyHere:vOptions: operations"

	^self isHeadless 
		ifTrue: [4 "Do not display progress"] 
		ifFalse: [0 "Display progress"]!

defaultIncludeMap

	^Dictionary new
		at: ##(self) dolphinCoreRepositoryName put: ##(self) dolphinOwnerName;
		at: ##(self) dolphinContributionsRepositoryName put: ##(self) dolphinOwnerName;
		yourself!

extractFileExtensions

	"extractFileExtensions stores a collection of extensions of files to be used when selectively extracting packages from an archive (zip) folder.
	Defaults to standard package/smalltalk extensions"

	^extractFileExtensions!

extractFileExtensions: anObject

	"extractFileExtensions stores a collection of extensions of files to be used when selectively extracting packages from an archive (zip) folder.
	Defaults to standard package/smalltalk extensions"

	extractFileExtensions := anObject!

extractFilePattern

	"Concatenate the extractFileExtensions into a single String suitable for filtering"

	| stream |

	stream := WriteStream on: (String new: 32).

	self extractFileExtensions do: [ :each | stream nextPutAll: '*.'; nextPutAll: each] separatedBy: [stream nextPut: $;].

	^stream contents!

free

	self beUnfinalizable.
	self repositories do: [ :each | each free].
	shellObject ifNotNil: [ :shell | shell free].

	repositories := #().
	shellObject := nil!

includeMap

	"includeMap stores a Dictionary mapping repository names (package root directories) to the owner of that repository on GitHub.
	This is required to allow packages to have prerequisites in other users' repositories.

	Mappings may be defined by including a file named 'include' in the root of a repository.
	This should include the paths of such 'foreign' repositories, e.g.

	ownerName1/repositoryName1
	ownerName1/repositoryName2
	ownerName2/repositoryName3

	etc."

	^includeMap!

initialize

	includeMap := self defaultIncludeMap.
	repositories := SearchPolicy caseInsensitive newLookupTable.

	"Set defaults"
	self 
		isHeadless: false;
		shouldExtractEntireRootRepository: true;
		extractFileExtensions: self smalltalkExtractFileExtensions.

	self beFinalizable!

install: aString

	"aString is a repository path, i.e. owner/repository name.
	Optionally this may be followed by a package pathname within the repository.
	If the package name is omitted it is assumed the repository root contains a package with the name of the repository.

	The package, with all its prerequisites (recursively) are downloaded (if not already present) and installed"

	^self install: aString branch: nil!

install: aString branch: bStringOrNil

	"aString is a repository path, i.e. owner/repository name.
	Optionally this may be followed by a package pathname within the repository.
	If the package name is omitted it is assumed the repository root contains a package with the name of the repository.

	bStringOrNil if not nil is the branch name. If nil the default (master, usually) branch is used. 

	The package, with all its prerequisites (recursively) are downloaded (if not already present) and installed"

	| repository branch stream packagePath installPath |

	repository := self repositoryWithPath: aString.
	branch := repository branchWithName: bStringOrNil.
	branch packageFolder. "Lazy initialize"

	stream := aString readStream.
	stream skipThrough: $/; skipThrough: $/. "Skip past owner/repository name/"
	stream atEnd 
		ifTrue: [packagePath := File composePath: repository name subPath: repository name]
		ifFalse: [packagePath := File composePath: repository name subPath: (stream upToEnd replaceAll: $/ with: File pathDelimiter)].

	self shouldExtractEntireRootRepository
		ifTrue: [branch download]
		ifFalse: [branch download: packagePath].

	installPath := File fullPathOf: (File composeStem: packagePath extension: Package sourcePackageExtension) relativeTo: SessionManager current imageBase.
	(File exists: installPath) ifFalse: [installPath := self locateRootPackageFor: installPath].

	[Package manager installUntrapped: installPath] on: PackageManager.PackageNotFoundSignal do: 
		[ :exc || missingPath missingPackagePath missingRepositoryPath |
		missingPath := File fullPathOf: exc messageText relativeTo: exc tag.
		missingPackagePath := FileLocator imageRelative relativePathTo: missingPath.
		missingRepositoryPath := self repositoryPathFor: missingPackagePath relativeTo: repository.
		((self repositoryWithPath: missingRepositoryPath) branchWithName: bStringOrNil) download: missingPackagePath.
		exc retry]!

isHeadless

	"Answer whether the receiver is being used headless, i.e. should display no progress information.
	Default is false"

	^isHeadless!

isHeadless: anObject

	"Set whether the receiver is being used headless, i.e. should display no progress information.
	Default is false"

	isHeadless := anObject!

locateRootPackageFor: aString

	"Private - Special handling - repository names cannot contain space characters, however we allow the root package name to include spaces for cosmetic reasons.
	aString is the full path of an expected root package which doesn't exist; look for a matching package with spaces"

	| repositoryPath spacelessPackageName |

	repositoryPath := File splitPathFrom: aString.
	spacelessPackageName := File splitFilenameFrom: aString.

	File for: ('*.', Package sourcePackageExtension) in: repositoryPath do: 
		[ :each | 
		(spacelessPackageName match: (each fileName reject: [ :char | char isSeparator])) ifTrue: [^File composePath: repositoryPath subPath: each fileName]].

	self error: 'package not found: ', spacelessPackageName!

readIncludeFrom: aPath

	| includeFile stream |

	includeFile := File composePath: aPath subPath: 'include'.
	stream := FileStream read: includeFile text: true.

	[stream atEnd] whileFalse: 
		[| repository |
		repository := self repositoryWithPath: stream nextLine.
		self includeMap at: repository name put: repository ownerName].

	stream close!

repositories
	^repositories!

repositoryPathFor: aString relativeTo: aRepository

	"aString is a package path following the convention that top-level directory = repository name e.g. repositoryName\packageName.
	Return the repository path where the package can be located, e.g. userName/repositoryName.
	This is assumed to be in the same GitHub user account as aRepository unless the repository name appears in the includeMap"

	| repositoryName ownerName |

	repositoryName := aString readStream upTo: File pathDelimiter.
	ownerName := self includeMap at: repositoryName ifAbsent: [aRepository ownerName].

	^ownerName, '/', repositoryName!

repositoryWithPath: aString

	^self repositories at: aString ifAbsentPut: 
		[(GitHubRepository withPath: aString)
			manager: self;
			yourself]!

shellObject

	"We use the Shell scripting object to do zip file extracts"

	^shellObject ifNil: [shellObject := GHIShellDispatch new]!

shouldExtractEntireRootRepository

	"Answer whether the entire root repository should be extracted, or just the requested package.
	Default is true since the ususal case is to install the entirety (or at least the majority) of the repository.

	Set to false when (e.g.) requesting a package from the dolphinsmalltalk/Contributions repository (or similar) which contains a large number of unrelated packages"

	^shouldExtractEntireRootRepository!

shouldExtractEntireRootRepository: anObject

	"Set whether the entire root repository should be extracted, or just the requested package.
	Default is true since the ususal case is to install the entirety (or at least the majority) of the repository.

	Set to false when (e.g.) requesting a package from the dolphinsmalltalk/Contributions repository (or similar) which contains a large number of unrelated packages"

	shouldExtractEntireRootRepository := anObject!

smalltalkExtractFileExtensions

	"Default wildcard pattern String of file types to extract from archives - covers all standard Package source types"

	^##(
			{
				'cls'. 
				Package sourcePackageExtension. 
				Package packageExtension. 
				Package sourceGlobalExtension. 
				Package binaryGlobalExtension
			}
		)!

smalltalkExtractFilePatterns

	"Default wildcard pattern String of file types to extract from archives - covers all standard Package source types"

	^##(
			{
				'cls'. 
				Package sourcePackageExtension. 
				Package packageExtension. 
				Package sourceGlobalExtension. 
				Package binaryGlobalExtension
			}
		)! !
!GitHubPackageManager categoriesFor: #beHeadless!initialize/release!public! !
!GitHubPackageManager categoriesFor: #copyHereVOptions!accessing!private! !
!GitHubPackageManager categoriesFor: #defaultIncludeMap!constants!public! !
!GitHubPackageManager categoriesFor: #extractFileExtensions!accessing!public! !
!GitHubPackageManager categoriesFor: #extractFileExtensions:!accessing!public! !
!GitHubPackageManager categoriesFor: #extractFilePattern!accessing!private! !
!GitHubPackageManager categoriesFor: #free!initialize/release!private! !
!GitHubPackageManager categoriesFor: #includeMap!accessing!public! !
!GitHubPackageManager categoriesFor: #initialize!initialize/release!public! !
!GitHubPackageManager categoriesFor: #install:!operations!public! !
!GitHubPackageManager categoriesFor: #install:branch:!operations!public! !
!GitHubPackageManager categoriesFor: #isHeadless!accessing!public!testing! !
!GitHubPackageManager categoriesFor: #isHeadless:!accessing!public! !
!GitHubPackageManager categoriesFor: #locateRootPackageFor:!operations!private! !
!GitHubPackageManager categoriesFor: #readIncludeFrom:!operations!private! !
!GitHubPackageManager categoriesFor: #repositories!accessing!private! !
!GitHubPackageManager categoriesFor: #repositoryPathFor:relativeTo:!accessing!private! !
!GitHubPackageManager categoriesFor: #repositoryWithPath:!accessing!public! !
!GitHubPackageManager categoriesFor: #shellObject!accessing!public! !
!GitHubPackageManager categoriesFor: #shouldExtractEntireRootRepository!accessing!public!testing! !
!GitHubPackageManager categoriesFor: #shouldExtractEntireRootRepository:!accessing!public! !
!GitHubPackageManager categoriesFor: #smalltalkExtractFileExtensions!constants!public! !
!GitHubPackageManager categoriesFor: #smalltalkExtractFilePatterns!constants!public! !

!GitHubPackageManager class methodsFor!

dolphinContributionsRepositoryName

	^'Contributions'!

dolphinCoreRepositoryName

	^'Dolphin'!

dolphinOwnerName

	^'dolphinsmalltalk'!

install: aString

	self new 
		install: aString;
		free!

new

	^super new initialize! !
!GitHubPackageManager class categoriesFor: #dolphinContributionsRepositoryName!constants!public! !
!GitHubPackageManager class categoriesFor: #dolphinCoreRepositoryName!constants!public! !
!GitHubPackageManager class categoriesFor: #dolphinOwnerName!constants!public! !
!GitHubPackageManager class categoriesFor: #install:!operations!public! !
!GitHubPackageManager class categoriesFor: #new!instance creation!public! !

GitHubRepository guid: (GUID fromString: '{ce07ef56-3a3d-4423-b33e-6b5f178ec4fe}')!
GitHubRepository comment: ''!
!GitHubRepository categoriesForClass!Kernel-Objects! !
!GitHubRepository methodsFor!

branchWithName: aStringOrNil

	| branchName |

	branchName := aStringOrNil ifNil: [self defaultBranchName].

	^self namedBranches at: branchName ifAbsentPut: 
		[GitHubBranch new
			repository: self;
			name: branchName;
			yourself]!

defaultBranch

	^self branchWithName: self defaultBranchName!

defaultBranchName

	^'master'!

displayOn: aStream

	aStream nextPutAll: self ownerName; nextPut: $/; nextPutAll: self name!

free

	self namedBranches do: [ :each | each free].
	namedBranches := #()!

hasDolphinOwner

	^GitHubPackageManager dolphinOwnerName match: self ownerName!

initialize

	namedBranches := SearchPolicy caseInsensitive newLookupTable!

isDolphinCore

	^self hasDolphinOwner and: [GitHubPackageManager dolphinCoreRepositoryName match: self name]!

manager
	^manager!

manager: anObject
	manager := anObject!

name
	^name!

name: anObject
	name := anObject!

namedBranches

	^namedBranches!

ownerName
	^ownerName!

ownerName: anObject
	ownerName := anObject!

packageFolderName

	^self isDolphinCore
		ifTrue: ['Core']
		ifFalse: [self name]!

path: aString

	"user/name e.g. dolphinsmalltalk/dolphin.
	May optionally be followed by a package name or path, i.e. user/name/packageName or user/name/folder/packageName"

	| parts |

	parts := aString subStrings: $/.
	parts size < 2 ifTrue: [self error: 'invalid path'].

	self 
		ownerName: parts first;
		name: parts second.

	"Special case for Core Dolphin package/repository"
	(self hasDolphinOwner and: ['core' match: self name]) ifTrue: [self name: GitHubPackageManager dolphinCoreRepositoryName]!

printOn: aStream

	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $).!

url

	^'https://github.com/<1s>/<2s>' expandMacrosWith: self ownerName with: self name! !
!GitHubRepository categoriesFor: #branchWithName:!accessing!public! !
!GitHubRepository categoriesFor: #defaultBranch!accessing!public! !
!GitHubRepository categoriesFor: #defaultBranchName!constants!public! !
!GitHubRepository categoriesFor: #displayOn:!printing!public! !
!GitHubRepository categoriesFor: #free!initialize/release!private! !
!GitHubRepository categoriesFor: #hasDolphinOwner!public!testing! !
!GitHubRepository categoriesFor: #initialize!initialize/release!public! !
!GitHubRepository categoriesFor: #isDolphinCore!public!testing! !
!GitHubRepository categoriesFor: #manager!accessing!public! !
!GitHubRepository categoriesFor: #manager:!accessing!private! !
!GitHubRepository categoriesFor: #name!accessing!private! !
!GitHubRepository categoriesFor: #name:!accessing!private! !
!GitHubRepository categoriesFor: #namedBranches!accessing!public! !
!GitHubRepository categoriesFor: #ownerName!accessing!private! !
!GitHubRepository categoriesFor: #ownerName:!accessing!private! !
!GitHubRepository categoriesFor: #packageFolderName!accessing!public! !
!GitHubRepository categoriesFor: #path:!initialize/release!public! !
!GitHubRepository categoriesFor: #printOn:!printing!public! !
!GitHubRepository categoriesFor: #url!accessing!public! !

!GitHubRepository class methodsFor!

new

	^super new initialize!

withPath: aString

	"user/name e.g. dolphinsmalltalk/dolphin"

	^self new
		path: aString;
		yourself! !
!GitHubRepository class categoriesFor: #new!instance creation!public! !
!GitHubRepository class categoriesFor: #withPath:!instance creation!public! !

GHIShellDispatch guid: (IID fromString: '{D8F015C0-C278-11CE-A49E-444553540000}')!
GHIShellDispatch comment: '`GHShell32IShellDispatch` is a wrapper class for the COM interface ''Shell32.IShellDispatch'' generated from type information in the ''Microsoft Shell Controls And Automation'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains the following helpstring for this interface
	"Definition of interface IShellDispatch"

** This comment was automatically generated from a type library. Delete this line to prevent any manual edits from being overwritten if the wrapper class is regenerated.

IDL definition follows:
```
[
	object, 
	uuid(d8f015c0-c278-11ce-a49e-444553540000), 
	helpstring("Definition of interface IShellDispatch"), 
	hidden, 
	dual
]
interface IShellDispatch : IDispatch
 {
	[id(0x60020000), propget, helpstring("Get Application object")]
	HRESULT __stdcall Application(
		[out, retval]IDispatch** ppid);
	[id(0x60020001), propget, helpstring("Get Parent object")]
	HRESULT __stdcall Parent(
		[out, retval]IDispatch** ppid);
	[id(0x60020002), helpstring("Get special folder from ShellSpecialFolderConstants")]
	HRESULT __stdcall NameSpace(
		[in]VARIANT vDir,
		[out, retval]Folder** ppsdf);
	[id(0x60020003), helpstring("Browse the name space for a Folder")]
	HRESULT __stdcall BrowseForFolder(
		[in]long Hwnd,
		[in]BSTR Title,
		[in]long Options,
		[in, optional]VARIANT RootFolder,
		[out, retval]Folder** ppsdf);
	[id(0x60020004), helpstring("The collection of open folder windows")]
	HRESULT __stdcall Windows(
		[out, retval]IDispatch** ppid);
	[id(0x60020005), helpstring("Open a folder")]
	HRESULT __stdcall Open(
		[in]VARIANT vDir);
	[id(0x60020006), helpstring("Explore a folder")]
	HRESULT __stdcall Explore(
		[in]VARIANT vDir);
	[id(0x60020007), helpstring("Minimize all windows")]
	HRESULT __stdcall MinimizeAll();
	[id(0x60020008), helpstring("Undo Minimize All")]
	HRESULT __stdcall UndoMinimizeALL();
	[id(0x60020009), helpstring("Bring up the file run")]
	HRESULT __stdcall FileRun();
	[id(0x6002000a), helpstring("Cascade Windows")]
	HRESULT __stdcall CascadeWindows();
	[id(0x6002000b), helpstring("Tile windows vertically")]
	HRESULT __stdcall TileVertically();
	[id(0x6002000c), helpstring("Tile windows horizontally")]
	HRESULT __stdcall TileHorizontally();
	[id(0x6002000d), helpstring("Exit Windows")]
	HRESULT __stdcall ShutdownWindows();
	[id(0x6002000e), helpstring("Suspend the pc")]
	HRESULT __stdcall Suspend();
	[id(0x6002000f), helpstring("Eject the pc")]
	HRESULT __stdcall EjectPC();
	[id(0x60020010), helpstring("Bring up the Set time dialog")]
	HRESULT __stdcall SetTime();
	[id(0x60020011), helpstring("Handle Tray properties")]
	HRESULT __stdcall TrayProperties();
	[id(0x60020012), helpstring("Display shell help")]
	HRESULT __stdcall Help();
	[id(0x60020013), helpstring("Find Files")]
	HRESULT __stdcall FindFiles();
	[id(0x60020014), helpstring("Find a computer")]
	HRESULT __stdcall FindComputer();
	[id(0x60020015), helpstring("Refresh the menu")]
	HRESULT __stdcall RefreshMenu();
	[id(0x60020016), helpstring("Run a Control Panel Item")]
	HRESULT __stdcall ControlPanelItem(
		[in]BSTR bstrDir);
};
```
'!
!GHIShellDispatch categoriesForClass!COM-Interfaces!Shell32-Interfaces! !
!GHIShellDispatch methodsFor!

nameSpace: vDir
	"Answer the <GHShell32Folder3> result of invoking the NameSpace() method of the COM object.
	Helpstring: Get special folder from ShellSpecialFolderConstants"

	| answer |
	answer := GHShell32Folder3 newPointer.
	self NameSpace: vDir asVariant ppsdf: answer.
	^answer asObject!

NameSpace: vDir ppsdf: ppsdf
	"Private - Invoke the NameSpace() method of the COM object.
	Helpstring: Get special folder from ShellSpecialFolderConstants

		HRESULT __stdcall NameSpace(
			[in]VARIANT vDir,
			[out, retval]Folder** ppsdf);"

	<virtual stdcall: hresult 10 variant GHShell32Folder3**>
	^self invalidCall: _failureCode! !
!GHIShellDispatch categoriesFor: #nameSpace:!**auto generated**!methods!public! !
!GHIShellDispatch categoriesFor: #NameSpace:ppsdf:!**auto generated**!COM Interfaces-IShellDispatch!private! !

!GHIShellDispatch class methodsFor!

clsid
	"Private - Answer the CLSID of the coclass (Shell) for which the receiver is the default interface."

	^CLSID fromString: '{13709620-c279-11ce-a49e-444553540000}'
!

defineFunctions
	"Declare the virtual function table for the COM interface 'Shell32.IShellDispatch'
		GHShell32IShellDispatch defineTemplate"

	self
		defineFunction: #NameSpace:ppsdf:
			argumentTypes: 'variant GHShell32Folder3**'!

initializeTypeLib
	"Private - Establish a connection to the receiver's type library.
		GHShell32IShellDispatch initializeTypeLib"

	typeLib := GHShell32Lib! !
!GHIShellDispatch class categoriesFor: #clsid!**auto generated**!constants!private! !
!GHIShellDispatch class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!GHIShellDispatch class categoriesFor: #initializeTypeLib!**auto generated**!initializing!private! !

GHShell32Folder3 guid: (IID fromString: '{A7AE5F64-C4D7-4D7F-9307-4D24EE54B841}')!
GHShell32Folder3 comment: '`GHFolder3` is a wrapper class for the COM interface ''Shell32.Folder3'' generated from type information in the ''Microsoft Shell Controls And Automation'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains the following helpstring for this interface
	"Definition of interface Folder version 3"

** This comment was automatically generated from a type library. Delete this line to prevent any manual edits from being overwritten if the wrapper class is regenerated.

IDL definition follows:
```
[
	object, 
	uuid(a7ae5f64-c4d7-4d7f-9307-4d24ee54b841), 
	helpstring("Definition of interface Folder version 3"), 
	dual
]
interface Folder3 : Folder2
 {
	[id(0x00000002), propget, helpstring("Ask if the WebView barricade should be shown or not")]
	HRESULT __stdcall ShowWebViewBarricade(
		[out, retval]VARIANT_BOOL* pbShowWebViewBarricade);
	[id(0x00000002), propput, helpstring("Ask if the WebView barricade should be shown or not")]
	HRESULT __stdcall ShowWebViewBarricade(
		[in]VARIANT_BOOL pbShowWebViewBarricade);
};
```
'!
!GHShell32Folder3 categoriesForClass!COM-Interfaces!Shell32-Interfaces! !
!GHShell32Folder3 methodsFor!

copyHere: vItem vOptions: vOptions
	"Invoke the CopyHere() method of the COM object.
	Helpstring: Copy Items to this folder."

	^self CopyHere: vItem asVariant vOptions: vOptions asVariant!

CopyHere: vItem vOptions: vOptions
	"Private - Invoke the CopyHere() method of the COM object.
	Helpstring: Copy Items to this folder.

		HRESULT __stdcall CopyHere(
			[in]VARIANT vItem,
			[in, optional]VARIANT vOptions);"

	<virtual stdcall: hresult 16 variant variant>
	^self invalidCall: _failureCode!

get_Title: pbs
	"Private - Get the value of the 'Title' property of the receiver.

		HRESULT __stdcall Title(
			[out, retval]BSTR* pbs);"

	<virtual stdcall: hresult 8 bstr*>
	^self invalidCall: _failureCode!

items
	"Answer the <GHShell32FolderItems> result of invoking the Items() method of the COM object.
	Helpstring: The collection of Items in folder"

	| answer |
	answer := GHShell32FolderItems newPointer.
	self Items: answer.
	^answer asObject!

Items: ppid
	"Private - Invoke the Items() method of the COM object.
	Helpstring: The collection of Items in folder

		HRESULT __stdcall Items(
			[out, retval]FolderItems** ppid);"

	<virtual stdcall: hresult 12 GHShell32FolderItems**>
	^self invalidCall: _failureCode!

parseName: bName
	"Answer the <Shell32FolderItem> result of invoking the ParseName() method of the COM object.
	Helpstring: Parse the name to get an item."

	| answer |
	answer := GHShell32FolderItem newPointer.
	self ParseName: bName ppid: answer.
	^answer asObject!

ParseName: bName ppid: ppid
	"Private - Invoke the ParseName() method of the COM object.
	Helpstring: Parse the name to get an item.

		HRESULT __stdcall ParseName(
			[in]BSTR bName,
			[out, retval]FolderItem** ppid);"

	<virtual stdcall: hresult 13 bstr GHShell32FolderItem**>
	^self invalidCall: _failureCode!

title
	"Answer the <bstr> value of the 'Title' property of the receiver.
	Helpstring: Get the display name for the window"

	| answer |
	answer := BSTR new.
	self get_Title: answer.
	^answer asObject!

value
	"Answer the <bstr> value of the 'Title' property of the receiver.
	This is the default value (DISPID_VALUE) property of the receiver."

	^self title! !
!GHShell32Folder3 categoriesFor: #copyHere:vOptions:!**auto generated**!methods!public! !
!GHShell32Folder3 categoriesFor: #CopyHere:vOptions:!**auto generated**!COM Interfaces-Folder!private! !
!GHShell32Folder3 categoriesFor: #get_Title:!**auto generated**!COM Interfaces-Folder!private! !
!GHShell32Folder3 categoriesFor: #items!**auto generated**!methods!public! !
!GHShell32Folder3 categoriesFor: #Items:!**auto generated**!COM Interfaces-Folder!private! !
!GHShell32Folder3 categoriesFor: #parseName:!methods!public! !
!GHShell32Folder3 categoriesFor: #ParseName:ppid:!**auto generated**!COM Interfaces-Folder!private! !
!GHShell32Folder3 categoriesFor: #title!**auto generated**!properties!public! !
!GHShell32Folder3 categoriesFor: #value!**auto generated**!properties!public! !

!GHShell32Folder3 class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'Shell32.Folder'
		GHShell32Folder3 defineTemplate"

	self
		defineFunction: #get_Title:
			argumentTypes: 'bstr*';
		defineFunction: #Items:
			argumentTypes: 'GHShell32FolderItems**';
		defineFunction: #ParseName:ppid:
			argumentTypes: 'bstr GHShell32FolderItem**';
		defineFunction: #CopyHere:vOptions:
			argumentTypes: 'variant variant'!

initializeTypeLib
	"Private - Establish a connection to the receiver's type library.
		GHShell32Folder3 initializeTypeLib"

	typeLib := GHShell32Lib! !
!GHShell32Folder3 class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!GHShell32Folder3 class categoriesFor: #initializeTypeLib!**auto generated**!initializing!private! !

GHShell32FolderItem guid: (IID fromString: '{FAC32C80-CBE4-11CE-8350-444553540000}')!
GHShell32FolderItem comment: '`GHShell32FolderItem` is a wrapper class for the COM interface ''Shell32.FolderItem'' generated from type information in the ''Microsoft Shell Controls And Automation'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains the following helpstring for this interface
	"Definition of interface FolderItem"

** This comment was automatically generated from a type library. Delete this line to prevent any manual edits from being overwritten if the wrapper class is regenerated.

IDL definition follows:
```
[
	object, 
	uuid(fac32c80-cbe4-11ce-8350-444553540000), 
	helpstring("Definition of interface FolderItem"), 
	dual
]
interface FolderItem : IDispatch
 {
	[id(0x60020000), propget, helpstring("Get Application object")]
	HRESULT __stdcall Application(
		[out, retval]IDispatch** ppid);
	[id(0x60020001), propget, helpstring("Get Parent object")]
	HRESULT __stdcall Parent(
		[out, retval]IDispatch** ppid);
	[id(0000000000), propget, helpstring("Get display name for item")]
	HRESULT __stdcall Name(
		[out, retval]BSTR* pbs);
	[id(0000000000), propput, helpstring("Get display name for item")]
	HRESULT __stdcall Name(
		[in]BSTR pbs);
	[id(0x60020004), propget, helpstring("Get the pathname to the item")]
	HRESULT __stdcall Path(
		[out, retval]BSTR* pbs);
	[id(0x60020005), propget, helpstring("If item is link return link object")]
	HRESULT __stdcall GetLink(
		[out, retval]IDispatch** ppid);
	[id(0x60020006), propget, helpstring("If item is a folder return folder object")]
	HRESULT __stdcall GetFolder(
		[out, retval]IDispatch** ppid);
	[id(0x60020007), propget, helpstring("Is the item a link?")]
	HRESULT __stdcall IsLink(
		[out, retval]VARIANT_BOOL* pb);
	[id(0x60020008), propget, helpstring("Is the item a Folder?")]
	HRESULT __stdcall IsFolder(
		[out, retval]VARIANT_BOOL* pb);
	[id(0x60020009), propget, helpstring("Is the item a file system object?")]
	HRESULT __stdcall IsFileSystem(
		[out, retval]VARIANT_BOOL* pb);
	[id(0x6002000a), propget, helpstring("Is the item browsable?")]
	HRESULT __stdcall IsBrowsable(
		[out, retval]VARIANT_BOOL* pb);
	[id(0x6002000b), propget, helpstring("Modification Date?")]
	HRESULT __stdcall ModifyDate(
		[out, retval]DATE* pdt);
	[id(0x6002000b), propput, helpstring("Modification Date?")]
	HRESULT __stdcall ModifyDate(
		[in]DATE pdt);
	[id(0x6002000d), propget, helpstring("Size")]
	HRESULT __stdcall Size(
		[out, retval]long* pul);
	[id(0x6002000e), propget, helpstring("Type")]
	HRESULT __stdcall Type(
		[out, retval]BSTR* pbs);
	[id(0x6002000f), helpstring("Get the list of verbs for the object")]
	HRESULT __stdcall Verbs(
		[out, retval]FolderItemVerbs** ppfic);
	[id(0x60020010), helpstring("Execute a command on the item")]
	HRESULT __stdcall InvokeVerb(
		[in, optional]VARIANT vVerb);
};
```
'!
!GHShell32FolderItem categoriesForClass!COM-Interfaces!Shell32-Interfaces! !
!GHShell32FolderItem methodsFor!

get_GetFolder: ppid
	"Private - Get the value of the 'GetFolder' property of the receiver.

		HRESULT __stdcall GetFolder(
			[out, retval]IDispatch** ppid);"

	<virtual stdcall: hresult 14 IDispatch**>
	^self invalidCall: _failureCode!

get_IsFolder: pb
	"Private - Get the value of the 'IsFolder' property of the receiver.

		HRESULT __stdcall IsFolder(
			[out, retval]VARIANT_BOOL* pb);"

	<virtual stdcall: hresult 16 varbool*>
	^self invalidCall: _failureCode!

get_Name: pbs
	"Private - Get the value of the 'Name' property of the receiver.

		HRESULT __stdcall Name(
			[out, retval]BSTR* pbs);"

	<virtual stdcall: hresult 10 bstr*>
	^self invalidCall: _failureCode!

get_Path: pbs
	"Private - Get the value of the 'Path' property of the receiver.

		HRESULT __stdcall Path(
			[out, retval]BSTR* pbs);"

	<virtual stdcall: hresult 12 bstr*>
	^self invalidCall: _failureCode!

getFolder
	"Answer the <GHShell32Folder3> value of the 'GetFolder' property of the receiver.
	Helpstring: If item is a folder return folder object"

	| answer |
	answer := GHShell32Folder3 newPointer.
	self get_GetFolder: answer.
	^answer asObject!

isFolder
	"Answer the <varbool> value of the 'IsFolder' property of the receiver.
	Helpstring: Is the item a Folder?"

	| answer |
	answer := VARIANT_BOOL new.
	self get_IsFolder: answer.
	^answer asObject!

name
	"Answer the <bstr> value of the 'Name' property of the receiver.
	Helpstring: Get display name for item"

	| answer |
	answer := BSTR new.
	self get_Name: answer.
	^answer asObject!

name: pbs
	"Set the 'Name' property of the receiver to the <bstr> value of the argument.
	Helpstring: Get display name for item"

	self put_Name: pbs!

path
	"Answer the <bstr> value of the 'Path' property of the receiver.
	Helpstring: Get the pathname to the item"

	| answer |
	answer := BSTR new.
	self get_Path: answer.
	^answer asObject!

put_Name: pbs
	"Private - Set the value of the 'Name' property of the object wrapped by the 
	 receiver to the <bstr> argument, pbs.

		HRESULT __stdcall Name(
			[in]BSTR pbs);"

	<virtual stdcall: hresult 11 bstr>
	^self invalidCall: _failureCode!

value
	"Answer the <bstr> value of the 'Name' property of the receiver.
	This is the default value (DISPID_VALUE) property of the receiver."

	^self name!

value: pbs
	"Set the 'Name' property of the receiver to the <bstr> value of the argument.
	This is the default value (DISPID_VALUE) property of the receiver."

	self name: pbs! !
!GHShell32FolderItem categoriesFor: #get_GetFolder:!**auto generated**!COM Interfaces-FolderItem!private! !
!GHShell32FolderItem categoriesFor: #get_IsFolder:!**auto generated**!COM Interfaces-FolderItem!private! !
!GHShell32FolderItem categoriesFor: #get_Name:!**auto generated**!COM Interfaces-FolderItem!private! !
!GHShell32FolderItem categoriesFor: #get_Path:!**auto generated**!COM Interfaces-FolderItem!private! !
!GHShell32FolderItem categoriesFor: #getFolder!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #isFolder!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #name!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #name:!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #path!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #put_Name:!**auto generated**!COM Interfaces-FolderItem!private! !
!GHShell32FolderItem categoriesFor: #value!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #value:!**auto generated**!properties!public! !

!GHShell32FolderItem class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'Shell32.FolderItem'
		GHShell32FolderItem defineTemplate"

	self
		defineFunction: #get_Name:
			argumentTypes: 'bstr*';
		defineFunction: #put_Name:
			argumentTypes: 'bstr';
		defineFunction: #get_Path:
			argumentTypes: 'bstr*';
		defineFunction: #get_GetFolder:
			argumentTypes: 'IDispatch**';
		defineFunction: #get_IsFolder:
			argumentTypes: 'varbool*'!

initializeTypeLib
	"Private - Establish a connection to the receiver's type library.
		GHShell32FolderItem initializeTypeLib"

	typeLib := GHShell32Lib! !
!GHShell32FolderItem class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!GHShell32FolderItem class categoriesFor: #initializeTypeLib!**auto generated**!initializing!private! !

GHShell32FolderItems guid: (IID fromString: '{744129E0-CBE5-11CE-8350-444553540000}')!
GHShell32FolderItems comment: '`GHShell32FolderItems` is a wrapper class for the COM interface ''Shell32.FolderItems'' generated from type information in the ''Microsoft Shell Controls And Automation'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains the following helpstring for this interface
	"Definition of interface FolderItems"

** This comment was automatically generated from a type library. Delete this line to prevent any manual edits from being overwritten if the wrapper class is regenerated.

IDL definition follows:
```
[
	object, 
	uuid(744129e0-cbe5-11ce-8350-444553540000), 
	helpstring("Definition of interface FolderItems"), 
	dual
]
interface FolderItems : IDispatch
 {
	[id(0x60020000), propget, helpstring("Get count of items in the folder")]
	HRESULT __stdcall Count(
		[out, retval]long* plCount);
	[id(0x60020001), propget, helpstring("Get Application object")]
	HRESULT __stdcall Application(
		[out, retval]IDispatch** ppid);
	[id(0x60020002), propget, helpstring("Get Parent object")]
	HRESULT __stdcall Parent(
		[out, retval]IDispatch** ppid);
	[id(0x60020003), helpstring("Return the figure for the given index")]
	HRESULT __stdcall Item(
		[in, optional]VARIANT index,
		[out, retval]FolderItem** ppid);
	[id(0xfffffffc), helpstring("Enumerates the figures")]
	HRESULT __stdcall _NewEnum(
		[out, retval]IUnknown** ppunk);
};
```
'!
!GHShell32FolderItems categoriesForClass!COM-Interfaces!Shell32-Interfaces! !
!GHShell32FolderItems methodsFor!

_newEnum
	"Answer the <IUnknown> result of invoking the _NewEnum() method of the COM object.
	Helpstring: Enumerates the figures"

	| answer |
	answer := IUnknown newPointer.
	self _NewEnum: answer.
	^answer asObject!

_NewEnum: ppunk
	"Private - Invoke the _NewEnum() method of the COM object.
	Helpstring: Enumerates the figures

		HRESULT __stdcall _NewEnum(
			[out, retval]IUnknown** ppunk);"

	<virtual stdcall: hresult 12 IUnknown**>
	^self invalidCall: _failureCode!

count
	"Answer the <sdword> value of the 'Count' property of the receiver.
	Helpstring: Get count of items in the folder"

	| answer |
	answer := SDWORD new.
	self get_Count: answer.
	^answer asObject!

filter: grfFlags bstrFileSpec: bstrFileSpec
	"Invoke the Filter() method of the COM object.
	Helpstring: Set a wildcard filter to apply to the items returned"

	^self Filter: grfFlags bstrFileSpec: bstrFileSpec!

Filter: grfFlags bstrFileSpec: bstrFileSpec
	"Private - Invoke the Filter() method of the COM object.
	Helpstring: Set a wildcard filter to apply to the items returned

		HRESULT __stdcall Filter(
			[in]long grfFlags,
			[in]BSTR bstrFileSpec);"

	<virtual stdcall: hresult 14 sdword bstr>
	^self invalidCall: _failureCode!

get_Count: plCount
	"Private - Get the value of the 'Count' property of the receiver.

		HRESULT __stdcall Count(
			[out, retval]long* plCount);"

	<virtual stdcall: hresult 8 sdword*>
	^self invalidCall: _failureCode!

isVBCollection
	"Answer whether the receiver is a VB style collection."

	^true!

item
	"Answer the <GHShell32FolderItem> result of invoking the Item() method of the COM object.
	Default values are passed for all optional parameters."

	^self item: VARIANT unspecified!

item: index
	"Answer the <GHShell32FolderItem> result of invoking the Item() method of the COM object.
	Helpstring: Return the figure for the given index"

	| answer |
	answer := GHShell32FolderItem newPointer.
	self Item: index asVariant ppid: answer.
	^answer asObject!

Item: index ppid: ppid
	"Private - Invoke the Item() method of the COM object.
	Helpstring: Return the figure for the given index

		HRESULT __stdcall Item(
			[in, optional]VARIANT index,
			[out, retval]FolderItem** ppid);"

	<virtual stdcall: hresult 11 variant GHShell32FolderItem**>
	^self invalidCall: _failureCode! !
!GHShell32FolderItems categoriesFor: #_newEnum!**auto generated**!methods!public! !
!GHShell32FolderItems categoriesFor: #_NewEnum:!**auto generated**!COM Interfaces-FolderItems!private! !
!GHShell32FolderItems categoriesFor: #count!**auto generated**!properties!public! !
!GHShell32FolderItems categoriesFor: #filter:bstrFileSpec:!**auto generated**!methods!public! !
!GHShell32FolderItems categoriesFor: #Filter:bstrFileSpec:!**auto generated**!COM Interfaces-FolderItems3!private! !
!GHShell32FolderItems categoriesFor: #get_Count:!**auto generated**!COM Interfaces-FolderItems!private! !
!GHShell32FolderItems categoriesFor: #isVBCollection!**auto generated**!public!testing! !
!GHShell32FolderItems categoriesFor: #item!**auto generated**!methods!public! !
!GHShell32FolderItems categoriesFor: #item:!**auto generated**!methods!public! !
!GHShell32FolderItems categoriesFor: #Item:ppid:!**auto generated**!COM Interfaces-FolderItems!private! !

!GHShell32FolderItems class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'Shell32.FolderItems'
		GHShell32FolderItems defineTemplate"

	self
		defineFunction: #get_Count:
			argumentTypes: 'sdword*';
		defineFunction: #Item:ppid:
			argumentTypes: 'variant GHShell32FolderItem**';
		defineFunction: #_NewEnum:
			argumentTypes: 'IUnknown**'
!

initializeTypeLib
	"Private - Establish a connection to the receiver's type library.
		GHShell32FolderItems initializeTypeLib"

	typeLib := GHShell32Lib! !
!GHShell32FolderItems class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!GHShell32FolderItems class categoriesFor: #initializeTypeLib!**auto generated**!initializing!private! !

"Binary Globals"!

GHShell32Lib := Object fromBinaryStoreBytes: 
(ByteArray fromBase64String: 'IVNUQiA0IEYJFQACAAAAQVhUeXBlTGlicmFyeUFuYWx5emVyBgIJAElUeXBlTGliMgAAAAAAAAAA
QQgAAAYBCABUTElCQVRUUnIAAAAgAAAAsOmnUO9w0RG3WgCgyQVk/gAAAAABAAAAAQAAAAgAAAAS
AQAAAgAAAEdIEgEAAA8AAABHaXRIdWIgUGFja2FnZXOyAAAACgAAAFNoZWxsMzJMaWIAAAAA6gAA
APAAAABiAAAABgAAAFIAAAALAAAAUFJPUFZBUklBTlSyAAAABwAAAFZBUklBTlRSAAAABAAAAEdV
SUSyAAAABAAAAEdVSURSAAAADwAAAFJlbW90YWJsZUhhbmRsZbIAAAAOAAAARXh0ZXJuYWxIYW5k
bGUAAAAA')!

