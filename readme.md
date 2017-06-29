hLink
=====

hLink is a collection of tools for managing file systems, designed around my personal preference for using symlinks to reorganize files installed by other programs.

hLink consists of a collection of programs that can be run on one or more directories (very useful for dragging and dropping on systems that support passing filepaths this way) that allow you to manage file systems without any typing.

Alternatively you can use them as commands for combining multiple linking tasks into a single mount command.

.LINKS
------

hLink stores its information in `.links` files near the targets of your links. 
It has a simple format, lines of the format
```<link> -> <target>```

note that single paths can fill a line in the file as well, the target is `.` by default.

The targets are specified relative to the folder in which the file resides.
Eventually the link targets can be specified as such as well, if they begin with `.`, but for now they start with either the system root (or a drive) or they start with a hLink reference.

.hLink
------

hLink references are like environment variables, for specifying prefixes on paths.
References make hLink more portable, and further reduce the friction in moving folders around.

You can store a `.hLink` file anywhere accessible by your search path, and fill it with lines of the form
```<refname>: path```

Then all of your `.links` files will store paths relative to the longest relevant reference in your `.hLink` file.

This has two main uses:
Firstly, you can make references for common environment variables, and use these references to mount the same folders on two computers.
Secondly, you can make references to an installation you will want to move, (or to an index you will want to restructure) and then by changing your `.hLink` file you save yourself the hassle of remaking all of your `.links` files.

In the future I may make a program for generating `.hLink` out of environment variables.

Another missing feature is recompressing existing `.links` files, so if you are going to use `.hLink` use it early!

Commands
========

At the moment hLink has two categories of commands, hLink commands, and misc commands, the former being things I had in mind in making hLink, and the latter being things made possible/easy as a result of hLinks internal library, and other tools I might actually use but that don't relate to link management.

Currently all commands take filepaths as arguments, and run the corresponding program on each filepath in order, as if passed each one at a time.

Mount
-----

This is one of the only tools you will want to use once your `.links` are set up, it goes through a `.links` file and mounts each one.

Currently only directory links are supported, since the relevant Haskell libraries were recently updated and file links don't seem to work.

It does not support relative link locations, may attempt to link relative paths using the working directory of the calling shell or of the mount executable itself.

It will overwrite any system links (including file links) but not actual files or folders.

On Windows it requires administrator privileges to succeed, so I recommend setting it to run as administrator via `properties->compatability`

Mark
----

When passed a filepath, this will add that path to a `.links` file located in that directory.

If the path refers to a file the `.links` will be in the same folder as the file. (not that files work yet!)

Using `mark`, the main technique for using hLink is to `mark` a folder that you want to move to another location, move/rename it as desired, and then `mount` it.

If you have an existing link, you can `mark` the link instead, and the `.links` file will be put in the target of the link. (This trick only works for directory links)

Often you can use hLink generated symlinks to manipulate the paths of folders before you `mark` them, which can be useful, especially for marking large numbers of files as if they were on a different drive already.

Note that `mark` behaves literally when given relative paths, you should use `%CD%` or `$PWD` unless you are hacking.

