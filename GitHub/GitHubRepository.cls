"Filed out from Dolphin Smalltalk 7"!

GitHubInfoObject subclass: #GitHubRepository
	instanceVariableNames: 'ownerName owner branches branchesRequest'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitHubRepository guid: (GUID fromString: '{1ae46cd9-4f19-4b59-bc33-7d23c0edf7e2}')!
GitHubRepository comment: ''!
!GitHubRepository categoriesForClass!Kernel-Objects! !
!GitHubRepository methodsFor!

_owner

	^owner!

<= aGitHubRepository

	^self owner = aGitHubRepository owner
		ifTrue: [self name <= aGitHubRepository name]
		ifFalse: [self owner <= aGitHubRepository owner]!

archiveURL

	^self json at: 'archive_url'!

branches

	^branches ifNil: 
		[(self isValid and: [self branchesRequest get; isSuccessful])
			ifTrue: [branches := self branchesRequest json collect: [:each | self newBranch initializeFromJson: each]]
			ifFalse: [branches := Array with: (self newBranch name: self class invalidRepositoryName; yourself)]]!

branchesRequest
	^branchesRequest ifNil: [branchesRequest := self api newRequestWithURL: self branchesURL expandGitHubURL]!

branchesURL

	^self json at: 'branches_url'!

branchWithName: aString

	^self branchWithName: aString ifNone: [nil]!

branchWithName: aString ifNone: aBlock

	^self branches detect: [ :each | each name = aString] ifNone: aBlock!

defaultBranch

	^self branchWithName: self defaultBranchName!

defaultBranchName

	^self isValid 
		ifTrue: [self json at: 'default_branch']
		ifFalse: [self class invalidRepositoryName]!

displayOn: aStream

	aStream nextPutAll: self ownerName; nextPut: $/; nextPutAll: self name!

flushLocalCaches

	branches := nil!

gitHubPath

	^self ownerName, '/', self name!

hasMultipleBranches

	^self branches size > 1!

initializeFromJson: aDictionary

	super initializeFromJson: aDictionary.

	self 
		name: (aDictionary at: 'name');
		owner "will instantiate from json"!

isFork

	^self json at: 'fork'!

isSmalltalk

	^self language = 'Smalltalk'!

language

	^self json at: 'language'!

newBranch

	^GitHubBranch new
		repository: self;
		yourself!

newRequest

	^super newRequest, {'repos'. self ownerName. self name}!

otherBranches

	^self branches reject: [ :each | each name = self defaultBranchName]!

owner

	^owner ifNil: 
		[owner := self api ownerFromJson: self ownerJson.
		owner 
			addRepository: self;
			yourself]!

owner: aGitHubOwner

	owner := aGitHubOwner!

ownerJson

	^self json at: 'owner'!

ownerName
	^ownerName ifNil: [ownerName := self owner name]!

ownerName: anObject
	ownerName := anObject!

treesURL

	^self json at: 'trees_url'! !
!GitHubRepository categoriesFor: #_owner!accessing!private! !
!GitHubRepository categoriesFor: #<=!comparing!public! !
!GitHubRepository categoriesFor: #archiveURL!accessing!public! !
!GitHubRepository categoriesFor: #branches!accessing!private! !
!GitHubRepository categoriesFor: #branchesRequest!accessing!private! !
!GitHubRepository categoriesFor: #branchesURL!accessing!public! !
!GitHubRepository categoriesFor: #branchWithName:!accessing!public! !
!GitHubRepository categoriesFor: #branchWithName:ifNone:!accessing!public! !
!GitHubRepository categoriesFor: #defaultBranch!accessing!public! !
!GitHubRepository categoriesFor: #defaultBranchName!accessing!public! !
!GitHubRepository categoriesFor: #displayOn:!displaying!public! !
!GitHubRepository categoriesFor: #flushLocalCaches!initialize/release!private! !
!GitHubRepository categoriesFor: #gitHubPath!accessing!public! !
!GitHubRepository categoriesFor: #hasMultipleBranches!public!testing! !
!GitHubRepository categoriesFor: #initializeFromJson:!initialize/release!private! !
!GitHubRepository categoriesFor: #isFork!public!testing! !
!GitHubRepository categoriesFor: #isSmalltalk!public!testing! !
!GitHubRepository categoriesFor: #language!accessing!public! !
!GitHubRepository categoriesFor: #newBranch!accessing!private! !
!GitHubRepository categoriesFor: #newRequest!accessing!private! !
!GitHubRepository categoriesFor: #otherBranches!accessing!public! !
!GitHubRepository categoriesFor: #owner!accessing!public! !
!GitHubRepository categoriesFor: #owner:!accessing!private! !
!GitHubRepository categoriesFor: #ownerJson!accessing!private! !
!GitHubRepository categoriesFor: #ownerName!accessing!public! !
!GitHubRepository categoriesFor: #ownerName:!accessing!public! !
!GitHubRepository categoriesFor: #treesURL!accessing!public! !

!GitHubRepository class methodsFor!

invalidRepositoryName

	^'invalid repository'! !
!GitHubRepository class categoriesFor: #invalidRepositoryName!constants!private! !

