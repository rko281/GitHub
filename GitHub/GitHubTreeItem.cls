"Filed out from Dolphin Smalltalk 7"!

GitHubShaObject subclass: #GitHubTreeItem
	instanceVariableNames: 'branch path size type parent children'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitHubTreeItem guid: (GUID fromString: '{158358de-da5f-4398-b960-b7f32345ff7a}')!
GitHubTreeItem comment: ''!
!GitHubTreeItem categoriesForClass!Kernel-Objects! !
!GitHubTreeItem methodsFor!

addChild: aGitHubTreeItem

	(self children ifNil: [self children: OrderedCollection new. self children]) add: aGitHubTreeItem.

	^aGitHubTreeItem 
		parent: self;
		yourself
!

api

	^self branch api!

branch

	^branch!

branch: aGitHubBranch

	branch := aGitHubBranch!

children

	^children!

children: aCollection

	children := aCollection!

contents

	^(self api newRequestWithURL: self url) 
		get; 
		responseText!

displayOn: aStream

	aStream nextPutAll: self path!

fileExtension

	^File splitExtensionFrom: self path!

fileName

	^File splitFilenameFrom: self path!

hasChildren

	"children only initialized if added to"

	^self children notNil!

initializeFromJson: aDictionary

	super initializeFromJson: aDictionary.

	self
		path: (aDictionary at: 'path');
		type: (aDictionary at: 'type');
		size: (aDictionary at: 'size' ifAbsent: [nil]);
		yourself!

isDirectory

	^self type = 'tree'!

isFile

	^self type = 'blob' or: [self type = 'file']!

name

	^File splitStemFrom: self path!

parent

	^parent!

parent: aGitHubTreeItem

	parent := aGitHubTreeItem!

parentPath

	| parentPath |

	parentPath := File splitPathFrom: self path.

	^parentPath isEmpty 
		ifTrue: [nil]
		ifFalse: [parentPath allButLast "Don't want trailing /"]
!

path
	^path!

path: anObject
	path := anObject!

repository

	^self branch repository!

saveContentsTo: aFilename

	"Save as raw data to avoid duplicating any BOM"
	(FileStream write: aFilename text: false)
		nextPutAll: self contents asByteArray;
		close!

saveContentsToPath: aPathname

	"Return the full file name"

	| fullFileName |

	fullFileName := File composePath: aPathname subPath: self fileName.

	self saveContentsTo: fullFileName.

	^fullFileName!

size
	^size!

size: anObject
	size := anObject!

type
	^type!

type: anObject
	type := anObject! !
!GitHubTreeItem categoriesFor: #addChild:!adding!public! !
!GitHubTreeItem categoriesFor: #api!accessing!private! !
!GitHubTreeItem categoriesFor: #branch!accessing!public! !
!GitHubTreeItem categoriesFor: #branch:!accessing!public! !
!GitHubTreeItem categoriesFor: #children!accessing!public! !
!GitHubTreeItem categoriesFor: #children:!accessing!private! !
!GitHubTreeItem categoriesFor: #contents!accessing!public! !
!GitHubTreeItem categoriesFor: #displayOn:!printing!public! !
!GitHubTreeItem categoriesFor: #fileExtension!accessing!public! !
!GitHubTreeItem categoriesFor: #fileName!accessing!public! !
!GitHubTreeItem categoriesFor: #hasChildren!public!testing! !
!GitHubTreeItem categoriesFor: #initializeFromJson:!initialize/release!private! !
!GitHubTreeItem categoriesFor: #isDirectory!public!testing! !
!GitHubTreeItem categoriesFor: #isFile!public!testing! !
!GitHubTreeItem categoriesFor: #name!accessing!public! !
!GitHubTreeItem categoriesFor: #parent!accessing!public! !
!GitHubTreeItem categoriesFor: #parent:!accessing!private! !
!GitHubTreeItem categoriesFor: #parentPath!accessing!public! !
!GitHubTreeItem categoriesFor: #path!accessing!private! !
!GitHubTreeItem categoriesFor: #path:!accessing!private! !
!GitHubTreeItem categoriesFor: #repository!accessing!public! !
!GitHubTreeItem categoriesFor: #saveContentsTo:!operations!public! !
!GitHubTreeItem categoriesFor: #saveContentsToPath:!operations!public! !
!GitHubTreeItem categoriesFor: #size!accessing!private! !
!GitHubTreeItem categoriesFor: #size:!accessing!private! !
!GitHubTreeItem categoriesFor: #type!accessing!private! !
!GitHubTreeItem categoriesFor: #type:!accessing!private! !

