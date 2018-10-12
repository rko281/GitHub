"Filed out from Dolphin Smalltalk 7"!

Error subclass: #JsonError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
JsonError guid: (GUID fromString: '{2a5ca8e6-bbc1-4e7a-baf3-b8a9591506cb}')!
JsonError comment: ''!
!JsonError categoriesForClass!Unclassified! !
!JsonError methodsFor!

_descriptionArguments
	^Array with: messageText with: tag displayString!

_descriptionFormat
	"Answer the Win32 format String to be used to format the description for the receiver."

	^'JSON Error: %1'! !
!JsonError categoriesFor: #_descriptionArguments!*-in class package!displaying!public! !
!JsonError categoriesFor: #_descriptionFormat!*-in class package!displaying!public! !

!JsonError class methodsFor!

icon
^Icon fromFile: 'Udo Schneider\Goodies\Resources\json.ico'! !
!JsonError class categoriesFor: #icon!public! !

