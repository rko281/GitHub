"Filed out from Dolphin Smalltalk 7"!

Object subclass: #GitHubRepository
	instanceVariableNames: 'manager name ownerName namedBranches'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
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

