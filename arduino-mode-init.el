(autoload 'arduino-mode "arduino-mode" "Major mode for editing Arduino code." t)
(add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
