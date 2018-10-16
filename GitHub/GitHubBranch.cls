"Filed out from Dolphin Smalltalk 7"!

GitHubShaObject subclass: #GitHubBranch
	instanceVariableNames: 'name repository treeRequest treeItems treeRoots'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitHubBranch guid: (GUID fromString: '{c5a81398-9267-463b-b3a5-5d8148ea0b1b}')!
GitHubBranch comment: ''!
!GitHubBranch categoriesForClass!Kernel-Objects! !
!GitHubBranch methodsFor!

<= anotherItem

	^self isDefaultBranch or: [anotherItem isDefaultBranch not and: [self name <= anotherItem name]]!

api

	^self repository api!

buildTree

	| directories roots |

	directories := CaseInsensitiveSearchPolicy current newLookupTable.
	roots := OrderedCollection new.
	
	self treeItems do: [ :each | each isDirectory ifTrue: [directories at: each path put: each]].
	self treeItems do: 
		[ :each | 
		each parentPath
			ifNil: [roots add: each]
			ifNotNil: [ :parentPath | (directories at: parentPath) addChild: each]].

	self treeRoots: roots!

children

	^self treeRoots!

displayOn: aStream

	self repository displayOn: aStream.
	aStream nextPut: $/.
	aStream nextPutAll: self name!

initializeFromJson: aDictionary

	self name: (aDictionary at: 'name').

	super initializeFromJson: (aDictionary at: 'commit')!

isDefaultBranch

	^self repository defaultBranch == self!

name
	^name!

name: anObject
	name := anObject!

newTreeItem

	^GitHubTreeItem new
		branch: self;
		yourself!

ownerName

	^self repository ownerName!

repository

	^repository!

repository: aGitHubRepository

	repository := aGitHubRepository!

treeItems

	^treeItems ifNil: 
		[self treeRequest isSuccessful 
			ifTrue: [treeItems := (self treeRequest json at: 'tree') collect: [ :dict | self newTreeItem initializeFromJson: dict]]
			ifFalse: [treeItems := #()]]!

treeRequest

	^treeRequest ifNil: 
		[treeRequest := self api newRequestWithURL: (self repository treesURL expandGitHubURLWith: self name).
		treeRequest addParameter: 'recursive' value: 1.
		treeRequest get; yourself]!

treeRoots
	^treeRoots ifNil: [self buildTree. treeRoots]!

treeRoots: anObject
	treeRoots := anObject! !
!GitHubBranch categoriesFor: #<=!comparing!public! !
!GitHubBranch categoriesFor: #api!accessing!private! !
!GitHubBranch categoriesFor: #buildTree!helpers!public! !
!GitHubBranch categoriesFor: #children!accessing!public! !
!GitHubBranch categoriesFor: #displayOn:!displaying!public! !
!GitHubBranch categoriesFor: #initializeFromJson:!initialize/release!private! !
!GitHubBranch categoriesFor: #isDefaultBranch!public!testing! !
!GitHubBranch categoriesFor: #name!accessing!public! !
!GitHubBranch categoriesFor: #name:!accessing!public! !
!GitHubBranch categoriesFor: #newTreeItem!accessing!private! !
!GitHubBranch categoriesFor: #ownerName!displaying!public! !
!GitHubBranch categoriesFor: #repository!accessing!public! !
!GitHubBranch categoriesFor: #repository:!accessing!private! !
!GitHubBranch categoriesFor: #treeItems!accessing!public! !
!GitHubBranch categoriesFor: #treeRequest!accessing!public! !
!GitHubBranch categoriesFor: #treeRoots!accessing!public! !
!GitHubBranch categoriesFor: #treeRoots:!accessing!public! !

