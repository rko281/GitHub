"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GitHubBranch
	instanceVariableNames: 'repository name tempFilename packageFolder'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

