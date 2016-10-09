# commando.el

`commando.el` is a simple way to let you associate elisp scripts
with a project by adding a file named `.commando` into your
project's root directory

Then, you can call scripts defined in `.commando` by calling `M-x
commando-run-task` in Emacs

# Defining scripts
Add scripts to your .commando file by calling `commando-add-task`

For example, below is a simple commando script definition for printing out the
contents of the current file you are viewing in Emacs

```
(commando-add-task :ls_current_directory
'(eshell-command (format "ls %s" commando/current-directory)))
```

`commando/current-directory` is a local variable commando provides when executing your script.

You have access to the following variables in your commando scripts
1. `commando/project-root` - The path to the project's top-level directory
2. `commando/current-file` - The path to the file currently being viewed in Emacs
3. `commando/current-directory` - The directory of the current file

# TODO

1. Improve performance by caching project tasks
2. Add a way to combine tasks
3. Add before and after hooks to tasks
4. Setup .commando files to enter emacs lisp interaction mode
