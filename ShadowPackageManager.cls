"Filed out from Dolphin Smalltalk 7"!

Object subclass: #ShadowPackageManager
	instanceVariableNames: 'shadowPackages shadowClasses'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShadowPackageManager guid: (GUID fromString: '{632c026f-9f83-4cf9-a6f8-9cd0713097a5}')!
ShadowPackageManager comment: ''!
!ShadowPackageManager categoriesForClass!Kernel-Objects! !
!ShadowPackageManager methodsFor!

addPackage: aShadowPackage 
	"Private - Add the <ShadowPackage> argument to the collection of packages held by the receiver. 
	Each package must have a unique name. Answer the argument, or raise an error if a package with the same name already exists"

	| name |
	name := aShadowPackage name.
	(self includesPackageNamed: name) ifTrue: [^self errorNameIsNotUnique: name].
	self basicAddPackage: aShadowPackage.
	^aShadowPackage!

basicAddPackage: aShadowPackage
	self shadowPackages at: aShadowPackage name put: aShadowPackage.
	aShadowPackage manager: self!

classNamed: aSymbol

	^Smalltalk at: aSymbol ifAbsent: [self shadowClassNamed: aSymbol]!

dependentPackagesOf: aShadowPackage 
	"Private - Answer a <collection> of the packages which have the <ShadowPackage> argument,
	as a prerequisite."

	^self shadowPackages select: [:each | each prerequisites includes: aShadowPackage]!

errorNameIsNotUnique: aString 
	^PackageManager.DuplicatePackageSignal signal: aString!

fileLocatorClass

	^self class fileLocatorClass!

includesPackageNamed: aString 
	"Answer whether the receiver holds a <Package> whose name matches 
	that specified."

	^(self shadowPackageNamed: aString ifNone: []) notNil!

initialize

	shadowPackages := PluggableLookupTable searchPolicy: SearchPolicy caseInsensitive.
	shadowClasses := IdentityDictionary new!

install: aString 
	"Install a shadow for the package file at the specified <readableString> path"

	| package |

	package := self packageClass fromFile: aString.

	self shadowPackages at: package name put: package.

	^package!

packageClass

	^self class packageClass!

packageNamed: aString ifNone: aBlock

	^self shadowPackageNamed: aString ifNone: aBlock!

prerequisiteOf: aShadowPackage named: aString

	| actualName |

	"aString may be a path name"
	actualName := File splitFilenameFrom: aString.

	"Try for a real package first"
	^Package manager packageNamed: actualName ifNone: [self packageNamed: actualName ifNone: [self shadowPrerequisiteOf: aShadowPackage named: aString]]!

removePackage: aShadowPackage
	self shadowPackages removeKey: aShadowPackage name.
	aShadowPackage manager: nil!

shadowClasses
	^shadowClasses!

shadowClasses: anObject
	shadowClasses := anObject!

shadowClassNamed: aSymbol

	^self shadowClasses at: aSymbol ifAbsentPut: 
		[ShadowClass new 
			name: aSymbol; 
			yourself]!

shadowPackageNamed: aString

	^self shadowPackageNamed: aString ifNone: [self packageClass name: aString]!

shadowPackageNamed: aString ifNone: aBlock

	^self shadowPackages at: aString ifAbsent: aBlock!

shadowPackages
	^shadowPackages!

shadowPackages: anObject
	shadowPackages := anObject!

shadowPrerequisiteOf: aShadowPackage named: aString

	| pathname |

	pathname := File composePath: aShadowPackage packageFolder subPath: aString.
	pathname first = File pathDelimiter ifTrue: [pathname := pathname allButFirst].

	^(self packageClass name: (File splitFilenameFrom: aString))
		packagePathname: (File composeStem: pathname extension: aShadowPackage packageExtension);
		yourself! !
!ShadowPackageManager categoriesFor: #addPackage:!adding!public! !
!ShadowPackageManager categoriesFor: #basicAddPackage:!adding!private! !
!ShadowPackageManager categoriesFor: #classNamed:!accessing!public! !
!ShadowPackageManager categoriesFor: #dependentPackagesOf:!accessing!private! !
!ShadowPackageManager categoriesFor: #errorNameIsNotUnique:!exceptions!private! !
!ShadowPackageManager categoriesFor: #fileLocatorClass!constants!public! !
!ShadowPackageManager categoriesFor: #includesPackageNamed:!public!searching! !
!ShadowPackageManager categoriesFor: #initialize!initializing!private! !
!ShadowPackageManager categoriesFor: #install:!operations!public! !
!ShadowPackageManager categoriesFor: #packageClass!constants!public! !
!ShadowPackageManager categoriesFor: #packageNamed:ifNone:!accessing!public! !
!ShadowPackageManager categoriesFor: #prerequisiteOf:named:!accessing!public! !
!ShadowPackageManager categoriesFor: #removePackage:!public!removing! !
!ShadowPackageManager categoriesFor: #shadowClasses!accessing!private! !
!ShadowPackageManager categoriesFor: #shadowClasses:!accessing!private! !
!ShadowPackageManager categoriesFor: #shadowClassNamed:!accessing!public! !
!ShadowPackageManager categoriesFor: #shadowPackageNamed:!accessing!public! !
!ShadowPackageManager categoriesFor: #shadowPackageNamed:ifNone:!accessing!public! !
!ShadowPackageManager categoriesFor: #shadowPackages!accessing!private! !
!ShadowPackageManager categoriesFor: #shadowPackages:!accessing!private! !
!ShadowPackageManager categoriesFor: #shadowPrerequisiteOf:named:!accessing!private! !

!ShadowPackageManager class methodsFor!

fileLocatorClass

	^ShadowPackageRelativeFileLocator!

new

	^super new initialize!

packageClass

	^ShadowPackage! !
!ShadowPackageManager class categoriesFor: #fileLocatorClass!constants!public! !
!ShadowPackageManager class categoriesFor: #new!instance creation!public! !
!ShadowPackageManager class categoriesFor: #packageClass!constants!public! !

