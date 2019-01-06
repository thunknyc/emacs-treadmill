# Some guidance on coding style and workflow for contributors

* Take advantage of GitHub's issue and pull request features.
* Prefer COND over IF for non-trivial tests.
* Prefer more, simpler COND cases over compound logical expressions.
* Avoid extraneous whitespace.
* Avoid PROG1 and PROG2. Think hard before using PROGN. IF-LET, WHEN-LET, and COND will often get you out of this situation. Or try writing a new procedure.
* Prefer more, shorter procedures.
* Bug fixes, unless trivial, should be associated with an issue that identifies the problem being fixed.
* Behavioral changes should be made independently of bug fixes and should be discussed via an an issue.
