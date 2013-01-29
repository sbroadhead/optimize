CS 4TE3 Project Code Repository
========
Important information about using Git
-------------------------------------
In case you are not familiar with using Git, here are some pointers and guidelines.

* Read [this](http://git-scm.com/book) to get familiar with how Git works and in particular how it is different from svn.
* Don't commit all your changes at once. Git supports committing individual chunks or even lines of files
  in a single commit, so group them into related sets of changes, possibly making several commits. This keeps
  the revision history clean and lets us keep track of where each individual line of code originated.
* Commit very often (as in every ten minutes if possible). Any time you make a change that you can adequately describe
  in a single sentence, commit it. The changes aren't pushed to github until you explicitly issue a push command so it
  all happens locally.
* Always use meaningful commit messages that accurately describe all of your changes. 
* Don't be afraid to create a branch if you want to work on changing some code without disturbing what is already there.
* When you push to github, you might want to [squash](http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html) some of your commits into larger commits, if you have several
  commits that relate to a single feature. This keeps the revision history less cluttered.
* Although it's useful to know how to use Git from the command line, a GUI tool can be useful. I recommend:
    - Mac OS X: [SourceTree](http://www.sourcetreeapp.com/)
    - Window and Linux (also runs on OS X): [SmartGit](http://www.syntevo.com/smartgithg/index.html)

Team Members
------------

* Simon Broadhead (4th year Computer Science) <sbroadhead@gmail.com>
* Your name here...