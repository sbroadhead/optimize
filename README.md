CS 4TE3 Project Code Repository
========
Important information about using Git
-------------------------------------
In case you are not familiar with using Git, here are some pointers and guidelines.

* Read or skim the first two chapters of [this](http://git-scm.com/book) to get familiar with how Git works and in
  particular how it is different from svn.
* **Don't use `git rebase` on commits that you have pushed to github. This will break things and it will be a pain to fix.**
* **Never use `push -f` to force a conflicting push to the remote repository. This will overwrite the history and break everyone's checked-out code. Fix your local repository and then push when there are no conflicts.**
* You can fork this repository into your own github account and do development there, using the github Pull Request
  feature to bring changes back into the main repository. Forks of this repository will remain private. Both features
  are available at the top of the repository page.
* Don't commit all your changes at once. Git supports committing individual chunks or even lines of files
  in a single commit, so group them into related sets of changes, possibly making several commits. This keeps
  the revision history clean and lets us keep track of where each individual line of code originated.
* Don't check in binaries unless there is no source code available (i.e., you can check in a research paper as a PDF but
  don't check in the generated PDF of the report).
* Commit very often (as in every ten minutes if possible). Any time you make a change that you can adequately describe
  in a single sentence, commit it. The changes aren't pushed to github until you explicitly issue a push command so it
  all happens locally.
* Make sure you fetch your code from the origin (this github repository) frequently, otherwise you might end up making
  changes to out-of-date code and will have to deal with merge conflicts.
* Always use meaningful commit messages that accurately describe all of your changes. 
* Don't be afraid to create a branch if you want to work on changing some code without disturbing what is already there.
* When you push to github, you might want to [squash](http://gitready.com/advanced/2009/02/10/squashing-commits-with-rebase.html) some of your commits into larger commits, if you have several
  commits that relate to a single feature. This keeps the revision history less cluttered.
* Don't push broken code to the repository, and if you commit broken code to your own repository, squash the broken commits into a single working commit before pushing it.
* Although it's useful to know how to use Git from the command line, a GUI tool can be useful. I recommend:
    - Mac OS X: [SourceTree](http://www.sourcetreeapp.com/)
        - Xcode is not good for this because it abstracts most of Git's useful functionality away
    - Window and Linux (also runs on OS X): [SmartGit](http://www.syntevo.com/smartgithg/index.html)

Team members
------------

* Simon Broadhead (4th year Computer Science) <sbroadhead@gmail.com>
* Your name here...
