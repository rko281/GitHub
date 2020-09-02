"Filed out from Dolphin Smalltalk 7"!

IDispatch subclass: #GHShell32FolderItem
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'GHShell32Constants'
	classInstanceVariableNames: ''!
GHShell32FolderItem guid: (IID fromString: '{FAC32C80-CBE4-11CE-8350-444553540000}')!
GHShell32FolderItem comment: '`GHShell32FolderItem` is a wrapper class for the COM interface ''Shell32.FolderItem'' generated from type information in the ''Microsoft Shell Controls And Automation'' library. It contains methods to invoke the member functions exposed by that interface.

The type library contains the following helpstring for this interface
	"Definition of interface FolderItem"

** This comment was automatically generated from a type library. Delete this line to prevent any manual edits from being overwritten if the wrapper class is regenerated.

IDL definition follows:
```
[
	object, 
	uuid(fac32c80-cbe4-11ce-8350-444553540000), 
	helpstring("Definition of interface FolderItem"), 
	dual
]
interface FolderItem : IDispatch
 {
	[id(0x60020000), propget, helpstring("Get Application object")]
	HRESULT __stdcall Application(
		[out, retval]IDispatch** ppid);
	[id(0x60020001), propget, helpstring("Get Parent object")]
	HRESULT __stdcall Parent(
		[out, retval]IDispatch** ppid);
	[id(0000000000), propget, helpstring("Get display name for item")]
	HRESULT __stdcall Name(
		[out, retval]BSTR* pbs);
	[id(0000000000), propput, helpstring("Get display name for item")]
	HRESULT __stdcall Name(
		[in]BSTR pbs);
	[id(0x60020004), propget, helpstring("Get the pathname to the item")]
	HRESULT __stdcall Path(
		[out, retval]BSTR* pbs);
	[id(0x60020005), propget, helpstring("If item is link return link object")]
	HRESULT __stdcall GetLink(
		[out, retval]IDispatch** ppid);
	[id(0x60020006), propget, helpstring("If item is a folder return folder object")]
	HRESULT __stdcall GetFolder(
		[out, retval]IDispatch** ppid);
	[id(0x60020007), propget, helpstring("Is the item a link?")]
	HRESULT __stdcall IsLink(
		[out, retval]VARIANT_BOOL* pb);
	[id(0x60020008), propget, helpstring("Is the item a Folder?")]
	HRESULT __stdcall IsFolder(
		[out, retval]VARIANT_BOOL* pb);
	[id(0x60020009), propget, helpstring("Is the item a file system object?")]
	HRESULT __stdcall IsFileSystem(
		[out, retval]VARIANT_BOOL* pb);
	[id(0x6002000a), propget, helpstring("Is the item browsable?")]
	HRESULT __stdcall IsBrowsable(
		[out, retval]VARIANT_BOOL* pb);
	[id(0x6002000b), propget, helpstring("Modification Date?")]
	HRESULT __stdcall ModifyDate(
		[out, retval]DATE* pdt);
	[id(0x6002000b), propput, helpstring("Modification Date?")]
	HRESULT __stdcall ModifyDate(
		[in]DATE pdt);
	[id(0x6002000d), propget, helpstring("Size")]
	HRESULT __stdcall Size(
		[out, retval]long* pul);
	[id(0x6002000e), propget, helpstring("Type")]
	HRESULT __stdcall Type(
		[out, retval]BSTR* pbs);
	[id(0x6002000f), helpstring("Get the list of verbs for the object")]
	HRESULT __stdcall Verbs(
		[out, retval]FolderItemVerbs** ppfic);
	[id(0x60020010), helpstring("Execute a command on the item")]
	HRESULT __stdcall InvokeVerb(
		[in, optional]VARIANT vVerb);
};
```
'!
!GHShell32FolderItem categoriesForClass!COM-Interfaces!Shell32-Interfaces! !
!GHShell32FolderItem methodsFor!

get_GetFolder: ppid
	"Private - Get the value of the 'GetFolder' property of the receiver.

		HRESULT __stdcall GetFolder(
			[out, retval]IDispatch** ppid);"

	<virtual stdcall: hresult 14 IDispatch**>
	^self invalidCall: _failureCode!

get_IsFolder: pb
	"Private - Get the value of the 'IsFolder' property of the receiver.

		HRESULT __stdcall IsFolder(
			[out, retval]VARIANT_BOOL* pb);"

	<virtual stdcall: hresult 16 varbool*>
	^self invalidCall: _failureCode!

get_Name: pbs
	"Private - Get the value of the 'Name' property of the receiver.

		HRESULT __stdcall Name(
			[out, retval]BSTR* pbs);"

	<virtual stdcall: hresult 10 bstr*>
	^self invalidCall: _failureCode!

get_Path: pbs
	"Private - Get the value of the 'Path' property of the receiver.

		HRESULT __stdcall Path(
			[out, retval]BSTR* pbs);"

	<virtual stdcall: hresult 12 bstr*>
	^self invalidCall: _failureCode!

getFolder
	"Answer the <GHShell32Folder3> value of the 'GetFolder' property of the receiver.
	Helpstring: If item is a folder return folder object"

	| answer |
	answer := GHShell32Folder3 newPointer.
	self get_GetFolder: answer.
	^answer asObject!

isFolder
	"Answer the <varbool> value of the 'IsFolder' property of the receiver.
	Helpstring: Is the item a Folder?"

	| answer |
	answer := VARIANT_BOOL new.
	self get_IsFolder: answer.
	^answer asObject!

name
	"Answer the <bstr> value of the 'Name' property of the receiver.
	Helpstring: Get display name for item"

	| answer |
	answer := BSTR new.
	self get_Name: answer.
	^answer asObject!

name: pbs
	"Set the 'Name' property of the receiver to the <bstr> value of the argument.
	Helpstring: Get display name for item"

	self put_Name: pbs!

path
	"Answer the <bstr> value of the 'Path' property of the receiver.
	Helpstring: Get the pathname to the item"

	| answer |
	answer := BSTR new.
	self get_Path: answer.
	^answer asObject!

put_Name: pbs
	"Private - Set the value of the 'Name' property of the object wrapped by the 
	 receiver to the <bstr> argument, pbs.

		HRESULT __stdcall Name(
			[in]BSTR pbs);"

	<virtual stdcall: hresult 11 bstr>
	^self invalidCall: _failureCode!

value
	"Answer the <bstr> value of the 'Name' property of the receiver.
	This is the default value (DISPID_VALUE) property of the receiver."

	^self name!

value: pbs
	"Set the 'Name' property of the receiver to the <bstr> value of the argument.
	This is the default value (DISPID_VALUE) property of the receiver."

	self name: pbs! !
!GHShell32FolderItem categoriesFor: #get_GetFolder:!**auto generated**!COM Interfaces-FolderItem!private! !
!GHShell32FolderItem categoriesFor: #get_IsFolder:!**auto generated**!COM Interfaces-FolderItem!private! !
!GHShell32FolderItem categoriesFor: #get_Name:!**auto generated**!COM Interfaces-FolderItem!private! !
!GHShell32FolderItem categoriesFor: #get_Path:!**auto generated**!COM Interfaces-FolderItem!private! !
!GHShell32FolderItem categoriesFor: #getFolder!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #isFolder!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #name!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #name:!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #path!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #put_Name:!**auto generated**!COM Interfaces-FolderItem!private! !
!GHShell32FolderItem categoriesFor: #value!**auto generated**!properties!public! !
!GHShell32FolderItem categoriesFor: #value:!**auto generated**!properties!public! !

!GHShell32FolderItem class methodsFor!

defineFunctions
	"Declare the virtual function table for the COM interface 'Shell32.FolderItem'
		GHShell32FolderItem defineTemplate"

	self
		defineFunction: #get_Name:
			argumentTypes: 'bstr*';
		defineFunction: #put_Name:
			argumentTypes: 'bstr';
		defineFunction: #get_Path:
			argumentTypes: 'bstr*';
		defineFunction: #get_GetFolder:
			argumentTypes: 'IDispatch**';
		defineFunction: #get_IsFolder:
			argumentTypes: 'varbool*'!

initializeTypeLib
	"Private - Establish a connection to the receiver's type library.
		GHShell32FolderItem initializeTypeLib"

	typeLib := GHShell32Lib! !
!GHShell32FolderItem class categoriesFor: #defineFunctions!**auto generated**!initializing!public! !
!GHShell32FolderItem class categoriesFor: #initializeTypeLib!**auto generated**!initializing!private! !

