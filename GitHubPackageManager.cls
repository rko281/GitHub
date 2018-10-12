"Filed out from Dolphin Smalltalk 7"!

ShadowPackageManager subclass: #GitHubPackageManager
	instanceVariableNames: 'branch'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: 'registry'!
GitHubPackageManager guid: (GUID fromString: '{cbd419d8-7f4a-4730-bdf2-94c3bb8f6415}')!
GitHubPackageManager comment: ''!
!GitHubPackageManager categoriesForClass!Kernel-Objects! !
!GitHubPackageManager methodsFor!

api

	^self branch api!

branch
	^branch!

branch: anObject
	branch := anObject!

packageForItem: aGitHubItem

	^self shadowPackageNamed: aGitHubItem name ifNone: [self addPackage: (self packageClass newForItem: aGitHubItem)]!

prerequisiteOf: aGitHubPackage named: aString

	"Ensure the returned prerequisitie has its content (assuming it's another GitHubPackage)"

	| prereq |

	prereq := super prerequisiteOf: aGitHubPackage named: aString.

	(prereq isInstalled not and: [prereq hasContent not and: [prereq hasGitHubItem]]) ifTrue: [prereq getContent].

	^prereq!

repository

	^self branch repository!

repositoryOwnersByPriority

	"Priority is 
	 - owner of this manager's repository
	 - dolphinsmalltalk (owner of Dolphin and Contributions repositories)
	 - any other valid owners"

	| dolphinOwner allOwners myOwner byPriority |

	dolphinOwner := self api dolphinOwner. "<- do this first to ensure it's in the owners collection"
	allOwners := self api owners asOrderedCollection.
	myOwner := self repository owner.

	byPriority := OrderedCollection new: allOwners size.
	byPriority add: (allOwners remove: myOwner).
	myOwner isDolphinOwner ifFalse: [byPriority add: (allOwners remove: dolphinOwner)].

	byPriority addAll: (allOwners select: [ :each | each isValid]) asSortedCollection.

	^byPriority!

shadowPrerequisiteOf: aGitHubPackage named: aString

	| template |

	template := super shadowPrerequisiteOf: aGitHubPackage named: aString.
	template branch: self branch.

	"Attempt to locate a matching GitHubPackage in another repository"
	self repositoryOwnersByPriority do: 
		[ :owner |
		(owner repositoryNamed: template repositoryName) ifNotNil: 
			[ :rep | 
			(rep bestMatchForPackage: template) ifNotNil: [ :match | ^match]]].

	^template! !
!GitHubPackageManager categoriesFor: #api!accessing!private! !
!GitHubPackageManager categoriesFor: #branch!accessing!public! !
!GitHubPackageManager categoriesFor: #branch:!accessing!private! !
!GitHubPackageManager categoriesFor: #packageForItem:!accessing!public! !
!GitHubPackageManager categoriesFor: #prerequisiteOf:named:!accessing!public! !
!GitHubPackageManager categoriesFor: #repository!accessing!public! !
!GitHubPackageManager categoriesFor: #repositoryOwnersByPriority!accessing!private! !
!GitHubPackageManager categoriesFor: #shadowPrerequisiteOf:named:!accessing!private! !

!GitHubPackageManager class methodsFor!

forBranch: aGitHubBranch

	^self registry detect: [ :each | each branch = aGitHubBranch] ifNone: 
		[self registry add: 
			(self new 
				branch: aGitHubBranch; 
				yourself)]!

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

	| api stream ownerName repositoryName packagePath repositoryPath repository branch package allPrerequisites unloadedPrerequisites missingPrerequisites toDownload |

	api := GitHubAPI new.
	(api credentials isComplete or: [api credentials authenticateWithUser]) ifFalse: [^self].

	api canAuthenticate ifFalse: [self error: 'Authentication failed - evaluate "GitHubCredentials default authenticateWithUser" to renew authentication'].

	stream := aString readStream.
	ownerName := stream upTo: $/.
	repositoryName := stream upTo: $/.
	stream atEnd 
		ifTrue: [packagePath := repositoryName]
		ifFalse: [packagePath := stream upToEnd].

	repositoryPath := ownerName, '/', repositoryName.
	repository := api repositoryWithPath: repositoryPath.
	repository isValid ifFalse: [self error: 'invalid repository: ', repositoryPath].

	bStringOrNil isNil 
		ifTrue: [branch := repository defaultBranch]
		ifFalse: [branch := repository branchWithName: bStringOrNil ifNone: [self error: 'invalid branch: ', repositoryPath, ' - ', bStringOrNil]].

	package := branch packageWithPath: packagePath ifNone: 
		["Since GitHub doesn't permit spaces in repository names we also check for the repository name with spaces (re)inserted at camel-case intervals"
		packagePath = repositoryName
			ifTrue: [branch packageWithPath: packagePath asPhrase ifNone: [nil]]
			ifFalse: [nil]].
	package isNil ifTrue: [self error: 'could not locate package ', packagePath].
	package isInstalled ifTrue: [self error: 'package already loaded: ', package name].
	package isDownloaded ifTrue: [self error: 'package already exists: ', package name].


	(ProgressDialog operation: 
		[ :progress | 
		progress value: 0; text: 'Locating Prerequisite Packages'.

		allPrerequisites := package allPrerequisites.
		unloadedPrerequisites := allPrerequisites reject: [ :each | each isInstalled].
		missingPrerequisites := unloadedPrerequisites reject: [ :each | each isAvailable].
		missingPrerequisites isEmpty ifFalse: [progress topShell exit. self error: 'missing prerequisites: ', missingPrerequisites asSortedCollection printString].

		toDownload := OrderedCollection new.
		unloadedPrerequisites do: [ :each | each isAvailableLocally ifFalse: [toDownload add: each]].
		toDownload add: package. 

		toDownload keysAndValuesDo: 
			[ :index :pkg |
			progress 
				value: ((index / toDownload size) * 100) truncated;
				text: 'Downloading ', pkg name.
			pkg download].
		progress text: 'Installing ', package name.
		package install])
				allowCancel: false;
				caption: 'Downloading Packages...';
				showModal.

	package loadedPackage ifNotNil: [ :pkg | pkg browse]!

packageClass

	^GitHubPackage!

registry

	^registry ifNil: [registry := WeakIdentitySet new]!

uninitialize
	"Private - Uninitialize the receiver as it is about to be removed from the system."

	registry := nil.! !
!GitHubPackageManager class categoriesFor: #forBranch:!accessing!public! !
!GitHubPackageManager class categoriesFor: #install:!public!utilities! !
!GitHubPackageManager class categoriesFor: #install:branch:!public!utilities! !
!GitHubPackageManager class categoriesFor: #packageClass!constants!public! !
!GitHubPackageManager class categoriesFor: #registry!accessing!public! !
!GitHubPackageManager class categoriesFor: #uninitialize!class hierarchy-removing!public! !

