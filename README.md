[![Issue Stats](http://issuestats.com/github/fsprojects/ProjectScaffold/badge/issue)](http://issuestats.com/github/fsprojects/ProjectScaffold)
[![Issue Stats](http://issuestats.com/github/fsprojects/ProjectScaffold/badge/pr)](http://issuestats.com/github/fsprojects/ProjectScaffold)

# ProjectScaffold

This project can be used to scaffold a prototypical .NET solution including file system layout and tooling. This includes a build process that: 

* updates all AssemblyInfo files
* compiles the application and runs all test projects
* generates [SourceLinks](https://github.com/ctaggart/SourceLink)
* generates API docs based on XML document tags
* generates [documentation based on Markdown files](http://fsprojects.github.io/ProjectScaffold/writing-docs.html)
* generates [NuGet](http://www.nuget.org) packages
* and allows a simple [one step release process](http://fsprojects.github.io/ProjectScaffold/release-process.html).

In order to start the scaffolding process run 

    > build.cmd // on windows    
    $ ./build.sh  // on unix
    
Read the [Getting started tutorial](http://fsprojects.github.io/ProjectScaffold/index.html#Getting-started) to learn more.

Documentation: http://fsprojects.github.io/ProjectScaffold

## Requirements

ProjectScaffold requires a local git installation. You can download git from [Git Downloads](https://git-scm.com/downloads).

## Build Status

Mono | .NET
---- | ----
[![Mono CI Build Status](https://img.shields.io/travis/fsprojects/ProjectScaffold/master.svg)](https://travis-ci.org/fsprojects/ProjectScaffold) | [![.NET Build Status](https://img.shields.io/appveyor/ci/fsgit/ProjectScaffold/master.svg)](https://ci.appveyor.com/project/fsgit/projectscaffold)

## Maintainer(s)

- [@forki](https://github.com/forki)
- [@pblasucci](https://github.com/pblasucci)
- [@sergey-tihon](https://github.com/sergey-tihon)

The default maintainer account for projects under "fsprojects" is [@fsprojectsgit](https://github.com/fsprojectsgit) - F# Community Project Incubation Space (repo management)
