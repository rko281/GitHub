"Filed out from Dolphin Smalltalk 7"!

ShadowPackage subclass: #GitHubPackage
	instanceVariableNames: 'gitHubItem gitHubContents branch'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
GitHubPackage guid: (GUID fromString: '{766ac8f2-ebc7-4864-bd40-df1d252ce615}')!
GitHubPackage comment: ''!
!GitHubPackage categoriesForClass!Kernel-Objects! !
!GitHubPackage methodsFor!

allGitHubItems

	^OrderedCollection new
		addAll: self classItems;
		addAll: self sourceGlobalItems;
		addAll: self binaryGlobalItems;
		add: self gitHubItem;
		yourself!

api

	^self gitHubItem api!

binaryGlobalItems

	^self binaryGlobalNames collect: [ :each | self packageItemNamed: each withExtension: Package binaryGlobalExtension]!

branch

	^branch!

branch: anObject
	branch := anObject!

canBeDownloaded

	^self hasGitHubItem!

classes

	self ensureHasContent.

	^super classes!

classesDo: operation

	self ensureHasContent.

	^super classesDo: operation!

classItems

	^self classNames collect: [ :each | self packageItemNamed: each withExtension: 'cls']!

classNames

	self ensureHasContent.

	^super classNames!

comment

	self ensureHasContent.

	^super comment!

download

	self saveTo: self path!

ensureHasContent

	self hasContent ifFalse: [self getContent]!

getContent

	| packageStream |

	self hasGitHubItem ifFalse: [^self].

	self gitHubContents: self gitHubItem contents.
	packageStream := self gitHubContents readStream.
	self gitHubContents skipEncodingMarkerFrom: packageStream.

	self initializeFrom:
		((Package readFrom: packageStream)
			packagePathname: self packagePathname; "Preserve this"
			yourself)!

gitHubContents
	^gitHubContents!

gitHubContents: anObject
	gitHubContents := anObject!

gitHubItem
	^gitHubItem!

gitHubItem: anObject
	gitHubItem := anObject!

globalNames

	self ensureHasContent.

	^super globalNames!

hasContent

	^self gitHubContents notNil!

hasGitHubItem

	^self gitHubItem notNil!

install

	self isDownloaded ifFalse: [self download].

	super install!

installedPackage

	"We only consider an installed package of the same name to be equivalent to the receiver if the path also matches"

	^super installedPackage ifNotNil: 
		[ :package |
		(File splitPathFrom: self packagePathname) = (File splitPathFrom: package packagePathname)
			ifTrue: [package]
			ifFalse: [nil]]!

isAvailable

	^self isAvailableLocally or: [self canBeDownloaded]!

isAvailableLocally

	"Does the receiver's file exist in the appropriate place for this image (i.e. already downloaded)?"

	^super isAvailable!

isDownloaded

	^self isAvailableLocally!

isInstalled

	^self installedPackage notNil!

methodNames

	self ensureHasContent.

	^super methodNames!

packageItemNamed: aString withExtension: extString

	| path |

	path := (File composePath: (File splitPathFrom: self gitHubItem path) stem: aString extension: extString) withGitHubPathDelimiters.
	^self branch treeItems detect: [ :item | item path = path] ifNone: [nil]!

prerequisiteNames

	self ensureHasContent.

	^super prerequisiteNames!

prerequisites

	self ensureHasContent.

	^super prerequisites!

repositoryName

	"Extract from the path" 

	| pathname repositoryName |

	pathname := self packagePathname.
	repositoryName := pathname readStream upTo: File pathDelimiter.

	"Special case handling for the Dolphin Core package"
	^repositoryName = 'Core'
		ifTrue: ['Dolphin']
		ifFalse: [repositoryName]!

saveTo: aPathname

	File createDirectoryPath: aPathname.

	self allGitHubItems do: [ :each | each saveContentsToPath: aPathname]!

scripts

	self ensureHasContent.

	^super scripts!

sourceGlobalItems

	^self sourceGlobalNames collect: [ :each | self packageItemNamed: each withExtension: Package sourceGlobalExtension]! !
!GitHubPackage categoriesFor: #allGitHubItems!accessing!public! !
!GitHubPackage categoriesFor: #api!accessing!public! !
!GitHubPackage categoriesFor: #binaryGlobalItems!accessing!public! !
!GitHubPackage categoriesFor: #branch!accessing!public! !
!GitHubPackage categoriesFor: #branch:!accessing!private! !
!GitHubPackage categoriesFor: #canBeDownloaded!public!testing! !
!GitHubPackage categoriesFor: #classes!accessing!public! !
!GitHubPackage categoriesFor: #classesDo:!enumerating!public! !
!GitHubPackage categoriesFor: #classItems!accessing!public! !
!GitHubPackage categoriesFor: #classNames!accessing!private! !
!GitHubPackage categoriesFor: #comment!accessing!private! !
!GitHubPackage categoriesFor: #download!operations!public! !
!GitHubPackage categoriesFor: #ensureHasContent!operations!private! !
!GitHubPackage categoriesFor: #getContent!operations!private! !
!GitHubPackage categoriesFor: #gitHubContents!accessing!private! !
!GitHubPackage categoriesFor: #gitHubContents:!accessing!private! !
!GitHubPackage categoriesFor: #gitHubItem!accessing!private! !
!GitHubPackage categoriesFor: #gitHubItem:!accessing!private! !
!GitHubPackage categoriesFor: #globalNames!accessing!private! !
!GitHubPackage categoriesFor: #hasContent!public!testing! !
!GitHubPackage categoriesFor: #hasGitHubItem!public!testing! !
!GitHubPackage categoriesFor: #install!operations!public! !
!GitHubPackage categoriesFor: #installedPackage!accessing!public! !
!GitHubPackage categoriesFor: #isAvailable!public!testing! !
!GitHubPackage categoriesFor: #isAvailableLocally!public!testing! !
!GitHubPackage categoriesFor: #isDownloaded!public!testing! !
!GitHubPackage categoriesFor: #isInstalled!public!testing! !
!GitHubPackage categoriesFor: #methodNames!accessing!private! !
!GitHubPackage categoriesFor: #packageItemNamed:withExtension:!accessing!private! !
!GitHubPackage categoriesFor: #prerequisiteNames!accessing!private! !
!GitHubPackage categoriesFor: #prerequisites!enumerating!public! !
!GitHubPackage categoriesFor: #repositoryName!accessing!public! !
!GitHubPackage categoriesFor: #saveTo:!operations!private! !
!GitHubPackage categoriesFor: #scripts!accessing!private! !
!GitHubPackage categoriesFor: #sourceGlobalItems!accessing!public! !

!GitHubPackage class methodsFor!

newForItem: aGitHubItem

	^self new 
		gitHubItem: aGitHubItem;
		name: aGitHubItem name;
		branch: aGitHubItem branch;
		packagePathname: aGitHubItem packagePathname;
		yourself! !
!GitHubPackage class categoriesFor: #newForItem:!instance creation!public! !

