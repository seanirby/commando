# commando.el

`commando.el` is a simple way to let you associate elisp scripts with
a project by adding a file named `.commando` containing your scripts
into your project's root directory.  Scripts that use emacs commands
like `shell-command` will automatically be run from your project's root
directory.

You call scripts by calling `M-x commando-run-script` or `(commando-run-script "script-name")`.

# Defining scripts
Add scripts to your .commando file with the `commando-add-script` macro.

The code below shows a typical `.commando` file.

```
(commando-add-script :build-ios
                     (progn
                       (shell-command "cordova prepare")
                       (shell-command "cordova build ios --device")))

(commando-add-script :run-ios
                     (progn
                       (shell-command "cordova prepare")
                       (shell-command "cordova build ios --device")
                       (async-shell-command "cordova run ios --device")))

(commando-add-script :rebuild-tags
                     (async-shell-command (format "ctags -ReV -f TAGS %s"
                                                  (mapconcat 'identity
                                                             (list
                                                              "node_modules/phaser/src"
                                                              "node_modules/javascript-state-machine/state-machine.js"
                                                              "src/js")
                                                             " "))))

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
