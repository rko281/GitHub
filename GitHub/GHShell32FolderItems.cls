"Filed out from Dolphin Smalltalk 7"!

IDispatch subclass: #GHShell32FolderItems
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'GHShell32Constants'
	classInstanceVariableNames: ''!
GHShell32FolderItems guid: (IID fromString: '{744129E0-CBE5-11CE-8350-444553540000}')!
GHShell32FolderItems comment: '`GHShell32FolderItems` is a wrapper class for the COM interface ''Shell32.FolderItems'' generated from type information in the ''Microsoft Shell Controls And Automation'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains the following helpstring for this interface
	"Definition of interface FolderItems"

** This comment was automatically generated from a type library. Delete this line to prevent any manual edits from being overwritten if the wrapper class is regenerated.

IDL definition follows:
```
[
	object, 
	uuid(744129e0-cbe5-11ce-8350-444553540000), 
	helpstring("Definition of interface FolderItems"), 
	dual
]
interface FolderItems : IDispatch
 {
	[id(0x60020000), propget, helpstring("Get count of items in the folder")]
	HRESULT __stdcall Count(
		[out, retval]long* plCount);
	[id(0x60020001), propget, helpstring("Get Application object")]
	HRESULT __stdcall Application(
		[out, retval]IDispatch** ppid);
	[id(0x60020002), propget, helpstring("Get Parent object")]
	HRESULT __stdcall Parent(
		[out, retval]IDispatch** ppid);
	[id(0x60020003), helpstring("Return the figure for the given index")]
	HRESULT __stdcall Item(
		[in, optional]VARIANT index,
		[out, retval]FolderItem** ppid);
	[id(0xfffffffc), helpstring("Enumerates the figures")]
	HRESULT __stdcall _NewEnum(
		[out, retval]IUnknown** ppunk);
};
```
'!
!GHShell32FolderItems categoriesForClass!COM-Interfaces!Shell32-Interfaces! !
!GHShell32FolderItems methodsFor!

_newEnum
	"Answer the <IUnknown> result of invoking the _NewEnum() method of the COM object.
	Helpstring: Enumerates the figures"

	| answer |
	answer := IUnknown newPointer.
	self _NewEnum: answer.
	^answer asObject!

_NewEnum: ppunk
	"Private - Invoke the _NewEnum() method of the COM object.
	Helpstring: Enumerates the figures

		HRESULT __stdcall _NewEnum(
			[out, retval]IUnknown** ppunk);"

	<virtual stdcall: hresult 12 IUnknown**>
	^self invalidCall: _failureCode!

count
	"Answer the <sdword> value of the 'Count' property of the receiver.
	Helpstring: Get count of items in the folder"

	| answer |
	answer := SDWORD new.
	self get_Count: answer.
	^answer asObject!

filter: grfFlags bstrFileSpec: bstrFileSpec
	"Invoke the Filter() method of the COM object.
	Helpstring: Set a wildcard filter to apply to the items returned"

	^self Filter: grfFlags bstrFileSpec: bstrFileSpec!

Filter: grfFlags bstrFileSpec: bstrFileSpec
	"Private - Invoke the Filter() method of the COM object.
	Helpstring: Set a wildcard filter to apply to the items returned

		HRESULT __stdcall Filter(
			[in]long grfFlags,
			[in]BSTR bstrFileSpec);"

	<virtual stdcall: hresult 14 sdword bstr>
	^self invalidCall: _failureCode!

get_Count: plCount
	"Private - Get the value of the 'Count' property of the receiver.

		HRESULT __stdcall Count(
			[out, retval]long* plCount);"

	<virtual stdcall: hresult 8 sdword*>
	^self invalidCall: _failureCode!

isVBCollection
	"Answer whether the receiver is a VB style collection."

	^true!

item
	"Answer the <GHShell32FolderItem> result of invoking the Item() method of the COM object.
	Default values are passed for all optional parameters."

	^self item: VARIANT unspecified!

item: index
	"Answer the <GHShell32FolderItem> result of invoking the Item() method of the COM object.
	Helpstring: Return the figure for the given index"

	| answer |
	answer := GHShell32FolderItem newPointer.
	self Item: index asVariant ppid: answer.
	^answer asObject!

Item: index ppid: ppid
	"Private - Invoke the Item() method of the COM object.
	Helpstring: Return the figure for the given index

		HRESULT __stdcall Item(
			[in, optional]VARIANT index,
			[out, retval]FolderItem** ppid);"

	<virtual stdcall: hresult 11 variant GHShell32FolderItem**>
	^self invalidCall: _failureCode! !
!GHShell32FolderItems categoriesFor: #_newEnum!**auto generated**!methods!public! !
!GHShell32FolderItems categoriesFor: #_NewEnum:!**auto generated**!COM Interfaces-FolderItems!private! !
!GHShell32FolderItems categoriesFor: #count!**auto generated**!properties!public! !
!GHShell32FolderItems categoriesFor: #filter:bstrFileSpec:!**auto generated**!methods!public! !
!GHShell32FolderItems categoriesFor: #Filter:bstrFileSpec:!**auto generated**!COM Interfaces-FolderItems3!private! !
!GHShell32FolderItems categoriesFor: #get_Count:!**auto generated**!COM Interfaces-FolderItems!private! !
!GHShell32FolderItems categoriesFor: #isVBCollection!**auto generated**!public!testing! !
!GHShell32FolderItems categoriesFor: #item!**auto generated**!methods!public! !
!GHShell32FolderItems categoriesFor: #item:!**auto generated**!methods!public! !
!GHShell32FolderItems categoriesFor: #Item:ppid:!**auto generated**!COM Interfaces-FolderItems!private! !

!GHShell32FolderItems class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'Shell32.FolderItems'
		GHShell32FolderItems defineTemplate"

	self
		defineFunction: #get_Count:
			argumentTypes: 'sdword*';
		defineFunction: #Item:ppid:
			argumentTypes: 'variant GHShell32FolderItem**';
		defineFunction: #_NewEnum:
			argumentTypes: 'IUnknown**'
!

initializeTypeLib
	"Private - Establish a connection to the receiver's type library.
		GHShell32FolderItems initializeTypeLib"

	typeLib := GHShell32Lib! !
!GHShell32FolderItems class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!GHShell32FolderItems class categoriesFor: #initializeTypeLib!**auto generated**!initializing!private! !

