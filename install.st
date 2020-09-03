[ :repository :path || localPath directory |
localPath := File fullPathOf: path relativeTo: SessionManager current imageBase.
directory := File splitPathFrom: localPath.
(File exists: directory) ifFalse: [File createDirectoryPath: directory].
URLMonLibrary default urlDownload: ('https://raw.githubusercontent.com/<1s>/master/<2s>' expandMacrosWith: repository with: path)toFile: localPath.
Package manager install: localPath] value: 'rko281/GitHub' value: 'GitHub/GitHub Packages.pac'!
