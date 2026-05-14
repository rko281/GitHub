"Filed out from Dolphin Smalltalk"!

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

	^self namedBranches at: (aStringOrNil ifNil: ['__main__master__default__']) ifAbsentPut: 
		[GitHubBranch new
			repository: self;
			name: aStringOrNil;
			yourself]!

defaultBranch

	^self branchWithName: nil!

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

packagePathFromGithubPath: aString

	"ASSERT: aString begins with the receiver's path, i.e. <ownerName>/<name> "

	| stream |
	
	stream := aString readStream.
	stream skipTo: $/; skipTo: $/. "Skip past owner/repository name[/]"
	^stream atEnd 
		ifTrue: [File composePath: self name subPath: self name]
		ifFalse: [File composePath: self name subPath: (stream upToEnd replaceAll: $/ with: File pathDelimiter)]!

path: aString

	"user/name e.g. dolphinsmalltalk/dolphin.
	An optional branch name may follow, separated by a colon.
	May then optionally be followed by a package name or path, i.e. user/name/packageName or user/name/folder/packageName"

	| stream owner repository |

	stream := aString readStream.
	owner := stream upTo: $/.
	repository := (stream upTo: $/) readStream upTo: $:.

	(owner isEmpty or: [repository isEmpty]) ifTrue: [self error: 'invalid path'].

	self 
		ownerName: owner;
		name: repository.

	"Special case for Core Dolphin package/repository"
	(self hasDolphinOwner and: ['core' match: self name]) ifTrue: [self name: GitHubPackageManager dolphinCoreRepositoryName]!

printOn: aStream

	super printOn: aStream.
	aStream nextPut: $(.
	self displayOn: aStream.
	aStream nextPut: $).!

url

	^'https://github.com/<1s>/<2s>' expandMacrosWith: self ownerName with: self name! !

!GitHubRepository categoriesForMethods!
branchWithName:!accessing!public! !
defaultBranch!accessing!public! !
displayOn:!printing!public! !
free!initialize/release!private! !
hasDolphinOwner!public!testing! !
initialize!initialize/release!public! !
isDolphinCore!public!testing! !
manager!accessing!public! !
manager:!accessing!private! !
name!accessing!private! !
name:!accessing!private! !
namedBranches!accessing!public! !
ownerName!accessing!private! !
ownerName:!accessing!private! !
packageFolderName!accessing!public! !
packagePathFromGithubPath:!accessing!public! !
path:!initialize/release!public! !
printOn:!printing!public! !
url!accessing!public! !
!

!GitHubRepository class methodsFor!

new

	^super new initialize!

withPath: aString

	"user/name e.g. dolphinsmalltalk/dolphin"

	^self new
		path: aString;
		yourself! !

!GitHubRepository class categoriesForMethods!
new!instance creation!public! !
withPath:!instance creation!public! !
!

