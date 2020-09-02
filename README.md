# GitHub for Dolphin Smalltalk 7.1
* Download and install Dolphin Smalltalk Packages and their prerequisites directly from GitHub
* Simplifies the loading of complex frameworks
* Developed and tested with Dolphin Smalltalk 7.1

## Getting Started
* To download and install, evaluate the following code snippet:  
`SourceManager default fileItIn: (File readAllText: (URLMonLibrary default urlDownloadToCacheFile: 'https://raw.githubusercontent.com/rko281/GitHub/master/install.st'))`   
* OR Download the repository to your Dolphin working directory and install the package `GitHub Packages` (this will load all required prerequisite packages)
* To download and install a package from GitHub evaluate:

  `GitHubPackageManager install: '<github username>/<repository name>/<package path>'`
    
  Example:
  
  `GitHubPackageManager install: 'rko281/MethodSpies/Method Spies'`
  
  Package name can be omitted where it matches the repository name:
  
  `GitHubPackageManager install: 'rko281/MethodSpies'`

* Any required prerequisite packages will also be downloaded (this includes prerequisites from other repositories where these can be located)
