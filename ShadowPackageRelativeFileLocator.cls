"Filed out from Dolphin Smalltalk 7"!

PackageRelativeFileLocator subclass: #ShadowPackageRelativeFileLocator
	instanceVariableNames: 'manager'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ShadowPackageRelativeFileLocator guid: (GUID fromString: '{fa3528a2-89ba-4f5a-bf87-355f39a7d15e}')!
ShadowPackageRelativeFileLocator comment: ''!
!ShadowPackageRelativeFileLocator categoriesForClass!System-Support! !
!ShadowPackageRelativeFileLocator methodsFor!

manager
	^manager!

manager: anObject
	manager := anObject!

package
	"Answer the <Package> who's folder is used as the base path, or nil if the package is not loaded."

	^self manager packageNamed: packageName ifNone: []! !
!ShadowPackageRelativeFileLocator categoriesFor: #manager!accessing!private! !
!ShadowPackageRelativeFileLocator categoriesFor: #manager:!accessing!private! !
!ShadowPackageRelativeFileLocator categoriesFor: #package!accessing!public! !

!ShadowPackageRelativeFileLocator class methodsFor!

package: aPackage 

	^(super package: aPackage)
		manager: aPackage manager;
		yourself! !
!ShadowPackageRelativeFileLocator class categoriesFor: #package:!instance creation!public! !

