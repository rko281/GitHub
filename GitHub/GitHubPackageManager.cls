"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GitHubPackageManager
	instanceVariableNames: 'includeMap repositories shellObject isHeadless shouldExtractEntireRootRepository extractFileExtensions'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

