"Filed out from Dolphin Smalltalk 7"!

Object subclass: #ShadowPackage
	instanceVariableNames: 'name manager packagePathname comment classNames methodNames globalNames binaryGlobalNames globalAliases prerequisiteNames scripts'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShadowPackage guid: (GUID fromString: '{dcdbc8fd-4b28-4b27-8f94-d5fd05801f8f}')!
ShadowPackage comment: ''!
!ShadowPackage categoriesForClass!Kernel-Objects! !
!ShadowPackage methodsFor!

_scripts

	^scripts!

<= anotherPackage

	^self name <= anotherPackage name!

= anotherPackage

	^self packagePathname = anotherPackage packagePathname!

addAllPrerequisitesTo: aSet
	"Private - Add to <aSet> all those Packages which must be loaded in
	before the receiver may be loaded in."

	self prerequisites
		do: [:each | (aSet addNewElement: each) ifTrue: [each addAllPrerequisitesTo: aSet]]!

allPrerequisites
	"Private - Answer a collection of all the Packages objects which must be loaded in before the
	receiver may be loaded in."

	| answer |
	answer := IdentitySet new.
	self addAllPrerequisitesTo: answer.
	^answer.!

binaryGlobalNames
	^binaryGlobalNames!

binaryGlobalNames: anObject
	binaryGlobalNames := anObject!

calculatePrerequisites
	"Private - Answer an <IdentitySet? of the prerequisite <Package>s which have to be
	present before the receiver's objects may be successfully used. These packages may
	well need other packages to be loaded before they can operate."

	^self tracePrerequisites keys!

canBeInstalled

	^self isAvailable!

classes

	^classNames collect: [:each | self classFromName: each]!

classesDo: operation 
	"Evaluate the <monadicValuable>, operation, for each of the classes 
	owned by the receiver. Note that this does not include the classes of
	the receiver's loose methods."

	^classNames do: [:each | operation value: (self classFromName: each)]!

classFromName: aSymbol

	^self manager classNamed: aSymbol!

classNames
	^classNames!

classNames: anObject
	classNames := anObject!

comment
	^comment!

comment: anObject
	comment := anObject!

dependentPackages
	"Answer a <collection> of the packages which are directly dependant on the receiver."

	^self manager dependentPackagesOf: self!

displayOn: aStream
	"Append a representation of the receiver to aStream which is suitable for display to the end user."

	aStream nextPutAll: self name!

fileLocator
	^self fileLocatorClass package: self!

fileLocatorClass
	^self manager fileLocatorClass!

getPrerequisites

	^self prerequisiteNames isNil
		ifTrue: [#()]
		ifFalse: [self prerequisites]!

globalAliases
	^globalAliases!

globalAliases: anObject
	globalAliases := anObject!

globalNames
	^globalNames!

globalNames: anObject
	globalNames := anObject!

hasCyclicPrerequisites

	"Can't be sure"

	^false!

hash

	^self packagePathname hash!

hasUncommittedPrerequisites

	^false!

initializeFrom: aPackage

	self
		packagePathname: aPackage packagePathname;
		comment: aPackage comment;
		classNames: aPackage classNames;
		methodNames: aPackage methodNames;
		globalNames: aPackage globalNames;
		binaryGlobalNames: aPackage binaryGlobalNames;
		globalAliases: aPackage globalAliases; 
		prerequisiteNames: aPackage prerequisiteNames;
		scripts: aPackage _scripts;
		yourself!

install

	Package manager install: self packageFileName!

installedPackage

	^Package manager packageNamed: self name ifNone: [nil]!

isAvailable

	"Does the receiver's file exist in the appropriate place for this image?"

	^File exists: self packageFileName!

isChanged

	^false!

isDefaultPackage

	^false!

isInstalled

	"Is a package with this name already installed in the image?"

	^Package manager includesPackageNamed: self name!

loadedPackage

	^Package manager packageNamed: self name ifNone: [nil]!

manager
	"Answer the object responsible for managing this package."

	^manager!

manager: aShadowPackageManager

	manager := aShadowPackageManager!

methodFromName: methodName ifAbsent: exceptionHandler 
	"Private - Answer the <CompiledMethod> object which the <Association> argument encodes.
	There is some messing around to handle class methods. If there is no such method, then
	answer the value of evaluating the <niladicValuable>, exceptionHandler.

	See Package>>nameForMethod: for the reverse transformation."

	^ShadowCompiledMethod new
		methodClass: (self classFromName: methodName key);
		selector: methodName value asSymbol;
		owningPackage: self;
		yourself!

methodNames
	^methodNames!

methodNames: anObject
	methodNames := anObject!

methods
	"Answer an <IdentitySet> of the <CompiledMethod>s owned by the receiver."

	| newIdentitySet |
	newIdentitySet := IdentitySet new: 16.
	self methodsDo: [:each | newIdentitySet add: each].
	^newIdentitySet!

methodsDo: aMonadicValuable
	"Evaluate the <monadicValuable> argument for each of the loose methods owned by the receiver."

	self methodNames
		do: [:each | (self methodFromName: each ifAbsent: []) ifNotNil: [:method | aMonadicValuable value: method]]!

name
	^name!

name: anObject
	name := anObject!

packageExtension

	^File splitExtensionFrom: self packagePathname!

packageFileName
	"Answer the full path name (ie Path+Stem+Extension) of the file used
	to store the source code representation of the receiver. This is recomposed
	from the relative path held"

	^File fullPathOf: packagePathname relativeTo: SessionManager current imageBase!

packageFileNameOnly

	^File splitFilenameFrom: packagePathname!

packageFolder
	^File splitPathFrom: self packagePathname!

packagePathname
	^packagePathname!

packagePathname: anObject
	packagePathname := anObject!

packageVersion

	^''!

path
	"Answer the full path to the directory holding the receivers files."

	^File splitPathFrom: self packageFileName!

prerequisiteNames
	^prerequisiteNames!

prerequisiteNames: anObject
	prerequisiteNames := anObject!

prerequisites
	"Answer a collection of <Package>s that must be loaded before the
	receiver can be loaded."

	| pacman |

	prerequisiteNames isNil ifTrue: [^#()].

	pacman := self manager.
	^self prerequisiteNames collect: [ :packageName | pacman prerequisiteOf: self named: packageName ]!

printOn: aStream
	"Append a representation of the receiver to aStream."

	aStream
		basicPrint: self;
		nextPut: $(;
		print: self name;
		nextPut: $)
!

scriptAt: scriptName 
	"Answer the script <String> associated with the <Symbol>, scriptName.

	The currently used scripts are: #preinstall, #postinstall, #preuninstall and
	#postuninstall."

	^self scripts at: scriptName ifAbsent: ['']!

scripts
	^scripts ifNil: [IdentityDictionary new]!

scripts: anObject
	scripts := anObject!

sourceGlobalNames
	"Private - Answer a <collection> of <Symbols>'s, being the names of the global variables 
	owned by the receiver that can be filed out as separate source objects."

	^self globalNames 
		- self binaryGlobalNames 
		- self globalAliases!

tracePrerequisites

	| trace |

	trace := IdentityDictionary new.

	self prerequisites do: [ :each | trace at: each put: OrderedCollection new].

	^trace! !
!ShadowPackage categoriesFor: #_scripts!accessing!private! !
!ShadowPackage categoriesFor: #<=!comparing!public! !
!ShadowPackage categoriesFor: #=!comparing!public! !
!ShadowPackage categoriesFor: #addAllPrerequisitesTo:!accessing!private! !
!ShadowPackage categoriesFor: #allPrerequisites!accessing!private! !
!ShadowPackage categoriesFor: #binaryGlobalNames!accessing!private! !
!ShadowPackage categoriesFor: #binaryGlobalNames:!accessing!private! !
!ShadowPackage categoriesFor: #calculatePrerequisites!accessing!private! !
!ShadowPackage categoriesFor: #canBeInstalled!public!testing! !
!ShadowPackage categoriesFor: #classes!accessing!public! !
!ShadowPackage categoriesFor: #classesDo:!enumerating!public! !
!ShadowPackage categoriesFor: #classFromName:!mapping!private! !
!ShadowPackage categoriesFor: #classNames!accessing!private! !
!ShadowPackage categoriesFor: #classNames:!accessing!private! !
!ShadowPackage categoriesFor: #comment!accessing!private! !
!ShadowPackage categoriesFor: #comment:!accessing!private! !
!ShadowPackage categoriesFor: #dependentPackages!accessing!public! !
!ShadowPackage categoriesFor: #displayOn:!displaying!public! !
!ShadowPackage categoriesFor: #fileLocator!accessing!public! !
!ShadowPackage categoriesFor: #fileLocatorClass!accessing!private! !
!ShadowPackage categoriesFor: #getPrerequisites!enumerating!private! !
!ShadowPackage categoriesFor: #globalAliases!accessing!private! !
!ShadowPackage categoriesFor: #globalAliases:!accessing!private! !
!ShadowPackage categoriesFor: #globalNames!accessing!private! !
!ShadowPackage categoriesFor: #globalNames:!accessing!private! !
!ShadowPackage categoriesFor: #hasCyclicPrerequisites!public!testing! !
!ShadowPackage categoriesFor: #hash!comparing!public! !
!ShadowPackage categoriesFor: #hasUncommittedPrerequisites!public!testing! !
!ShadowPackage categoriesFor: #initializeFrom:!initializing!public! !
!ShadowPackage categoriesFor: #install!operations!public! !
!ShadowPackage categoriesFor: #installedPackage!accessing!public! !
!ShadowPackage categoriesFor: #isAvailable!public!testing! !
!ShadowPackage categoriesFor: #isChanged!public!testing! !
!ShadowPackage categoriesFor: #isDefaultPackage!public!testing! !
!ShadowPackage categoriesFor: #isInstalled!public!testing! !
!ShadowPackage categoriesFor: #loadedPackage!accessing!public! !
!ShadowPackage categoriesFor: #manager!accessing!public! !
!ShadowPackage categoriesFor: #manager:!accessing!private! !
!ShadowPackage categoriesFor: #methodFromName:ifAbsent:!mapping!private! !
!ShadowPackage categoriesFor: #methodNames!accessing!private! !
!ShadowPackage categoriesFor: #methodNames:!accessing!private! !
!ShadowPackage categoriesFor: #methods!enumerating!public! !
!ShadowPackage categoriesFor: #methodsDo:!enumerating!public! !
!ShadowPackage categoriesFor: #name!accessing!private! !
!ShadowPackage categoriesFor: #name:!accessing!private! !
!ShadowPackage categoriesFor: #packageExtension!accessing!public! !
!ShadowPackage categoriesFor: #packageFileName!accessing!public! !
!ShadowPackage categoriesFor: #packageFileNameOnly!accessing!public! !
!ShadowPackage categoriesFor: #packageFolder!accessing!public! !
!ShadowPackage categoriesFor: #packagePathname!accessing!private! !
!ShadowPackage categoriesFor: #packagePathname:!accessing!private! !
!ShadowPackage categoriesFor: #packageVersion!accessing!public! !
!ShadowPackage categoriesFor: #path!accessing!public! !
!ShadowPackage categoriesFor: #prerequisiteNames!accessing!private! !
!ShadowPackage categoriesFor: #prerequisiteNames:!accessing!private! !
!ShadowPackage categoriesFor: #prerequisites!enumerating!public! !
!ShadowPackage categoriesFor: #printOn:!printing!public! !
!ShadowPackage categoriesFor: #scriptAt:!accessing!public! !
!ShadowPackage categoriesFor: #scripts!accessing!private! !
!ShadowPackage categoriesFor: #scripts:!accessing!private! !
!ShadowPackage categoriesFor: #sourceGlobalNames!accessing!private! !
!ShadowPackage categoriesFor: #tracePrerequisites!operations!private! !

!ShadowPackage class methodsFor!

fromFile: aString

	^self fromPackage: (Package fromFile: aString)!

fromPackage: aPackage

	^(self name: aPackage name)
		initializeFrom: aPackage;
		yourself!

icon

	^##(Package icon)!

name: aString

	^self new
		name: aString;
		yourself!

readFrom: pacStream
	"Private - Answer a new instance of the receiver created from the .PAX (source 
	text) format representation in the <readableStream>, pacStream."

	^self fromPackage: (Package readFrom: pacStream)! !
!ShadowPackage class categoriesFor: #fromFile:!instance creation!public! !
!ShadowPackage class categoriesFor: #fromPackage:!instance creation!public! !
!ShadowPackage class categoriesFor: #icon!public! !
!ShadowPackage class categoriesFor: #name:!instance creation!public! !
!ShadowPackage class categoriesFor: #readFrom:!instance creation!private! !

