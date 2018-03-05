Branches
---------

* The code in the `master` branch should always compile and pass all tests.
* Issue branches. Naming scheme: `<github_username>/<issue_id>-<brief_description>`.
    * Use lowercase letters and no dashes in `issue_id`.
    * Use lowercase letters and dashes in `brief_description`.
    * Make sure the description is really brief but clear enough so that
      it is easy to understand what the issue is about without looking it
      up in YouTrack (assuming that you are familiar with existing issues).

<!--

TODO:

* merge vs rebase
* how often to merge/rebase your issue branch onto master
* to squash or not to squash

-->



Commit messages
----------------

* Follow [this guide][git-commit].
* Prefix the subject line with IDs of all relevant issues.
  Example: `[LTB-1] [LTB-3] Create basic project structure`.


Working on an issue
--------------------

1.  Starting from `master` create a new _issue branch_.
2.  Make your changes.
3.  Sign the most recent commit in your branch.
4.  Open a new pull request.
5.  Request a review from two persons you think would be most suitable as reviewers
    (e.g. because you think they might be interested in your change or because
    they touched this code previously).
6.  If you _really_ want to, request a review from one other person.
7.  Change the state of the issue to “Waiting for review”.
8.  Review at least two other pull requests (if there are less than two open
    pull requests, review all that are open).
9.  If needed (e.g. the changes are complex) schedule a call with the team during
    which you will talk through your pull request.
10. Wait for reviews to arrive. (Keep in mind that it is _your_ responsibility
    to get those changes merged, so do not hesitate to ping others and ask for reviews.)
11. If there are comments that need to be addressed, change the state of the issue
    to “In progress”, make your changes, then go to 4.


Reviewing pull requests
------------------------

* Start from the pull requests where your review was explicitly requested
  (you can use the `review-requested:<username>` search query).
* Other than that try not to prioritise PRs in any way (e.g. by chosing
  ones that seem most interesting). Move from older PRs to newer ones.
* If your review was requested but you think this is a mistake, remove
  the request and request a review from someone else, who you think is
  a more appropriate reviewer for the PR in question.
* Always ask questions. If you are not sure what a piece of code does, ask
  a question. If you are not sure why a piece of code is written the way it is,
  ask a question. If you are not sure what is going on, ask a question.
  It is the job of the PR author to explain everything they did to everyone.
  It is your job to learn from the code you see and/or make sure that
  our repository contains only code of the highest quality.
* Only in the case when you are not sure about the validity of _each_ of your
  comments choose the “comment” option. Otherwise “request changes” or
  “approve”.
* If you are the last of those whose review was requested and all the comments
  are minor and can be addressed reasonably quickly, after posting your
  comments you may make the required changes yourself, push and approve.


[git-commit]:  https://chris.beams.io/posts/git-commit/
