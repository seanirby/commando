# commando.el

`commando.el` is a simple way to let you associate elisp scripts with
a project.  Just add a file named `.commando` containing your scripts
into your project's root directory.  Scripts that use emacs commands
like `shell-command` will automatically be run from your project's root
directory.

You run scripts by calling `M-x commando-run-script` or `(commando-run-script "script-name")`.

# Setup

Add `commando.el` to your load path.

# Defining scripts
Add scripts to your .commando file with the `commando-add-script` macro.

The code below shows a typical `.commando` file.

```
;; Start the project
(commando-add-script :start
                     (shell-command "npm start"))

;; Run the test suite
(commando-add-script :start
                     (shell-command "npm run-tests"))

;; Build iOS executable
(commando-add-script :build-ios
                     (progn
                       (shell-command "cordova prepare")
                       (shell-command "cordova build ios --device")))

;; Build iOS executable and deploy to device
(commando-add-script :run-ios
                     (progn
                       (shell-command "cordova prepare")
                       (shell-command "cordova build ios --device")
                       (async-shell-command "cordova run ios --device")))


;; Rebuild tags file
(commando-add-script :rebuild-tags
                     (async-shell-command (format "ctags -ReV -f TAGS %s"
                                                  (mapconcat 'identity
                                                             (list
                                                              "node_modules/phaser/src"
                                                              "node_modules/javascript-state-machine/state-machine.js"
                                                              "src/js")
                                                             " "))))

;; Using git, stage all changes and commit.
(commando-add-script :stage-all-and-commit
                     (progn
                       (magit-stage-modified)
                       (magit-commit (list "-m" (read-string "Enter your commit message: ")))))
```

`commando/current-directory` is a local variable commando provides
when executing your script.

You have access to the following variables in your commando scripts

1. `commando/project-root` - The path to the project's root directory

2. `commando/current-file` - The path to the file currently being viewed in Emacs

3. `commando/current-directory` - The directory of the current file

# TODO

1. Improve performance by caching project scripts

2. Add a way to combine scripts

3. Add before and after hooks to scripts

4. Add support for a global commando file
