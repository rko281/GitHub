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

displayOn: aStream

	self branch displayOn: aStream!

download

	self branch download!

name

	^self repository name!

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
			"Don't look for prerequisites for a non-fork in a fork"
			(self repository isFork or: [rep isFork not]) ifTrue: 
				[(rep bestMatchForPackage: template) ifNotNil: [ :match | ^match]]]].

	^template! !
!GitHubPackageManager categoriesFor: #api!accessing!private! !
!GitHubPackageManager categoriesFor: #branch!accessing!public! !
!GitHubPackageManager categoriesFor: #branch:!accessing!private! !
!GitHubPackageManager categoriesFor: #displayOn:!accessing!public! !
!GitHubPackageManager categoriesFor: #download!operations!public! !
!GitHubPackageManager categoriesFor: #name!accessing!public! !
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

include: aString

	"Add aString, which represents a repository owner, to the set of known owners.
	This can be used to enable prerequisite repositories in other owner's accounts to be located automatically"

	GitHubPackageInstaller default include: aString!

install: aString

	"aString is a repository path, i.e. owner/repository name.
	Optionally this may be followed by a package pathname within the repository.
	If the package name is omitted it is assumed the repository root contains a package with the name of the repository.

	The package, with all its prerequisites (recursively) are downloaded (if not already present) and installed"

	^GitHubPackageInstaller default install: aString!

packageClass

	^GitHubPackage!

registry

	^registry ifNil: [registry := WeakIdentitySet new]!

uninitialize
	"Private - Uninitialize the receiver as it is about to be removed from the system."

	registry := nil.! !
!GitHubPackageManager class categoriesFor: #forBranch:!accessing!public! !
!GitHubPackageManager class categoriesFor: #include:!public!utilities! !
!GitHubPackageManager class categoriesFor: #install:!public!utilities! !
!GitHubPackageManager class categoriesFor: #packageClass!constants!public! !
!GitHubPackageManager class categoriesFor: #registry!accessing!public! !
!GitHubPackageManager class categoriesFor: #uninitialize!class hierarchy-removing!public! !

