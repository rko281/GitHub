"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GitHubPackageInstaller
	instanceVariableNames: 'api'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'default'!
GitHubPackageInstaller guid: (GUID fromString: '{76f2dfa1-8f0e-45bf-9e44-2f053a695e82}')!
GitHubPackageInstaller comment: ''!
!GitHubPackageInstaller categoriesForClass!Kernel-Objects! !
!GitHubPackageInstaller methodsFor!

api
	^api!

api: anObject
	api := anObject!

branch: bStringOrNil inRepositoryWithPath: aString

	"Locate the repository with path aString, i.e. '<owner name>/<repository name>'.
	If bStringOrNil is nil return the default branch, otherwise the branch named bStringOrNil"

	| repository |

	repository := self api repositoryWithPath: aString.
	repository isValid ifFalse: [self error: 'invalid repository: ', aString].

	^bStringOrNil isNil 
		ifTrue: [repository defaultBranch]
		ifFalse: [repository branchWithName: bStringOrNil ifNone: [self error: 'invalid branch: ', aString, ' - ', bStringOrNil]]
!

downloadRepository: aString

	"aString is a repository path, i.e. owner/repository name.

	The repository's default branch with its contents will be downloaded to a folder named <repository name> in the image directory"

	self downloadRepository: aString branch: nil!

downloadRepository: aString branch: bStringOrNil

	"aString is a repository path, i.e. owner/repository name.
	bStringOrNil if not nil is the branch name. If nil the default (master, usually) branch is used. 

	The repository with its contents will be downloaded to a folder named <repository name> in the image directory"

	(self branch: bStringOrNil inRepositoryWithPath: aString) download!

include: aString

	"Add aString, which represents a repository owner, to the set of known owners.
	This can be used to enable prerequisite repositories in other owner's accounts to be located automatically"

	self api ownerNamed: aString!

initialize

	self api: GitHubAPI newAuthenticated!

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

	| package |

	package := self packageWithPath: aString branch: bStringOrNil.
	package isNil ifTrue: [self error: 'could not locate package ', package name].
	package isInstalled ifTrue: [self error: 'package already loaded: ', package name].
	package isDownloaded ifTrue: [self error: 'package already exists: ', package name].

	(ProgressDialog operation: 
		[ :progress | | allPrerequisites unloadedPrerequisites missingPrerequisites toDownloadByManager toDownload |
		progress value: 0; text: 'Locating Prerequisite Packages'.

		allPrerequisites := package allPrerequisites.
		unloadedPrerequisites := allPrerequisites reject: [ :each | each isInstalled].
		missingPrerequisites := unloadedPrerequisites reject: [ :each | each isAvailable].
		missingPrerequisites isEmpty ifFalse: [progress topShell exit. self error: 'missing prerequisites: ', missingPrerequisites asSortedCollection printString].

		toDownloadByManager  := Dictionary new.
		toDownloadByManager at: package manager put: (OrderedCollection with: package).
		unloadedPrerequisites do: [ :each | each isAvailableLocally ifFalse: [(toDownloadByManager at: each manager ifAbsentPut: [OrderedCollection new]) add: each]].

		"Where we are downloading >= 50% of packages in a repository, download the whole repository as an archive for speed and efficiency of API requests"
		toDownload := OrderedCollection new.
		toDownloadByManager keysAndValuesDo: 
			[ :manager :packages |
			packages size >= (manager repository allChildPackages size // 2) 
				ifTrue: [toDownload add: manager]
				ifFalse: [toDownload addAll: packages]].

		toDownload keysAndValuesDo: 
			[ :index :pkgOrMgr |
			progress 
				value: ((index / toDownload size) * 100) truncated;
				text: 'Downloading ', pkgOrMgr displayString.
			pkgOrMgr download].
		progress text: 'Installing ', package name.
		package install])
				allowCancel: false;
				caption: 'Downloading Packages...';
				showModal.

	package loadedPackage ifNotNil: [ :pkg | pkg browse]!

packageWithPath: aString branch: bStringOrNil

	"Private - aString is a repository path, i.e. owner/repository name.
	Optionally this may be followed by a package pathname within the repository.
	If the package name is omitted it is assumed the repository root contains a package with the name of the repository.

	bStringOrNil if not nil is the branch name. If nil the default (master, usually) branch is used.

	Locate and return a GitHubPackage matching aString in the branch, or nil if none"

	| stream ownerName repositoryName packagePath repositoryPath branch |

	stream := aString readStream.
	ownerName := stream upTo: $/.
	repositoryName := stream upTo: $/.
	stream atEnd 
		ifTrue: [packagePath := repositoryName]
		ifFalse: [packagePath := stream upToEnd].

	repositoryPath := ownerName, '/', repositoryName.
	branch := self branch: bStringOrNil inRepositoryWithPath: repositoryPath.

	^branch packageWithPath: packagePath ifNone: 
		["Since GitHub doesn't permit spaces in repository names we also check for the repository name with spaces (re)inserted at camel-case intervals"
		packagePath = repositoryName
			ifTrue: [branch packageWithPath: packagePath asPhrase ifNone: [nil]]
			ifFalse: [nil]]! !
!GitHubPackageInstaller categoriesFor: #api!accessing!private! !
!GitHubPackageInstaller categoriesFor: #api:!accessing!private! !
!GitHubPackageInstaller categoriesFor: #branch:inRepositoryWithPath:!accessing!private! !
!GitHubPackageInstaller categoriesFor: #downloadRepository:!operations!public! !
!GitHubPackageInstaller categoriesFor: #downloadRepository:branch:!operations!public! !
!GitHubPackageInstaller categoriesFor: #include:!initializing!operations!public! !
!GitHubPackageInstaller categoriesFor: #initialize!initializing!public! !
!GitHubPackageInstaller categoriesFor: #install:!operations!public! !
!GitHubPackageInstaller categoriesFor: #install:branch:!operations!public! !
!GitHubPackageInstaller categoriesFor: #packageWithPath:branch:!accessing!private! !

!GitHubPackageInstaller class methodsFor!

default

	^default ifNil: [default := self new]!

new

	^super new initialize!

reset

	default := nil!

uninitialize

	self reset! !
!GitHubPackageInstaller class categoriesFor: #default!accessing!public! !
!GitHubPackageInstaller class categoriesFor: #new!instance creation!public! !
!GitHubPackageInstaller class categoriesFor: #reset!class initialization!public! !
!GitHubPackageInstaller class categoriesFor: #uninitialize!class initialization!public! !

