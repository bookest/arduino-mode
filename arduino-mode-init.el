(autoload 'arduino-mode "arduino-mode" "Major mode for editing Arduino code." t)
(autoload 'ede-arduino-preferences-file "ede-arduino" "Preferences file of Arduino." t)
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
(add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
