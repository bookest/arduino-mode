;;; Arduino-mode.el --- Major mode for the Arduino language

;; Copyright (C) 2008  Christopher Grim

;; Author: Christopher Grim <christopher.grim@gmail.com>
;; Keywords: languages, arduino
;; Version: 1.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Based on derived-mode-ex.el found here:
;;
;; <http://cc-mode.sourceforge.net/derived-mode-ex.el>.
;;

;;; Code:
(require 'cc-mode)

(eval-when-compile
  (require 'cl)
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cc-menus))

(eval-and-compile
  ;; fall back on c-mode
  (c-add-language 'arduino-mode 'c-mode))

(c-lang-defconst c-primitive-type-kwds
  arduino (append '("boolean" "byte")
                  (c-lang-const c-primitive-type-kwds)))

(c-lang-defconst c-constant-kwds
  arduino (append
           '("HIGH" "LOW"
             "INPUT" "OUTPUT" "INPUT_PULLUP"
             "LED_BUILTIN"
             "true" "false")
           (c-lang-const c-constant-kwds)))

(c-lang-defconst c-simple-stmt-kwds
  arduino (append
           '(;; Digital I/O
             "pinMode"
             "digitalWrite"
             "digitalRead"
             ;; Analog I/O
             "analogReference"
             "analogRead"
             "analogWrite"
             ;; Due only
             "analogReadResolution"
             "analogWriteResolution"
             ;; Advanced I/O
             "tone"
             "noTone"
             "shiftOut"
             "shiftIn"
             "pulseIn"
             ;; Time
             "millis"
             "micros"
             "delay"
             "delayMicroseconds"
             ;; Math
             "min"
             "max"
             "abs"
             "constrain"
             "map"
             "pow"
             "sqrt"
             ;; Trigonometry
             "sin"
             "cos"
             "tan"
             ;; Random Numbers
             "randomSeed"
             "random"
             ;; Bits and Bytes
             "lowByte"
             "highByte"
             "bitRead"
             "bitWrite"
             "bitSet"
             "bitClear"
             "bit"
             ;; External Interrupts
             "attachInterrupt"
             "detachInterrupt"
             ;; Interrupts
             "interrupts"
             "noInterrupts")
           (c-lang-const c-simple-stmt-kwds)))

(c-lang-defconst c-primary-expr-kwds
  arduino (append
           '(;; Communication
             "Serial"
             ;; USB (Leonoardo based boards and Due only)
             "Keyboard"
             "Mouse")
           (c-lang-const c-primary-expr-kwds)))

(defgroup arduino nil "Arduino mode customizations"
  :group 'languages)

(defcustom arduino-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in Arduino mode.
Each list item should be a regexp matching a single identifier." :group 'arduino)

(defcustom arduino-executable "arduino"
  "*The arduino executable"
  :group 'arduino
  :type 'string)

(defconst arduino-font-lock-keywords-1 (c-lang-const c-matchers-1 arduino)
  "Minimal highlighting for Arduino mode.")

(defconst arduino-font-lock-keywords-2 (c-lang-const c-matchers-2 arduino)
  "Fast normal highlighting for Arduino mode.")

(defconst arduino-font-lock-keywords-3 (c-lang-const c-matchers-3 arduino)
  "Accurate normal highlighting for Arduino mode.")

(defvar arduino-font-lock-keywords arduino-font-lock-keywords-3
  "Default expressions to highlight in ARDUINO mode.")

(defvar arduino-mode-syntax-table nil
  "Syntax table used in arduino-mode buffers.")
(or arduino-mode-syntax-table
    (setq arduino-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table arduino))))

(defvar arduino-mode-abbrev-table nil
  "Abbreviation table used in arduino-mode buffers.")

(c-define-abbrev-table 'arduino-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trigger
  ;; reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)))

(defvar arduino-mode-map
  (let ((map (c-make-inherited-keymap)))
    ;; Add bindings which are only useful for Arduino
    map)
  "Keymap used in arduino-mode buffers.")

(easy-menu-define arduino-menu arduino-mode-map "Arduino Mode Commands"
  (cons "Arduino" (c-lang-const c-mode-menu arduino)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))

;;;###autoload
(defun arduino-mode ()
  "Major mode for editing Arduino code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `arduino-mode-hook'.

Key bindings:
\\{arduino-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table arduino-mode-syntax-table)
  (setq major-mode 'arduino-mode
        mode-name "Arduino"
        local-abbrev-table arduino-mode-abbrev-table
        abbrev-mode t
        imenu-generic-expression cc-imenu-c-generic-expression)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars arduino-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'arduino-mode)
  (easy-menu-add arduino-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'arduino-mode-hook)
  (c-update-modeline))

(defun arduino-run-arduino ()
  (interactive)
  (start-file-process "arduino" () arduino-executable (buffer-file-name)))

;;; Arduino cli support
;; See more: https://github.com/arduino/arduino-cli


(defcustom arduino-exe "arduino-cli"
  "Arduino executable."
  :group 'arduino
  :type 'string)

(defcustom arduino-fqbn nil
  "The FQBN for the connected board."
  :group 'arduino
  :type 'string)

(defcustom arduino-port nil
  "The port that the board is connected to."
  :group 'arduino
  :type 'string)

(defcustom arduino-sketch-dir nil
  "The directory that the sketch is in."
  :group 'arduino
  :type 'string)

(defun arduino-update ()
  "Get FQBN of the current device.
This function lags so use it in async.

Returns an alist of ((fqbn . fqbn) (port . port))
You can check it out in terminal by \"arduino-cli board list\"."
  (let ((result (shell-command-to-string (concat arduino-exe " board list"))))
    (string-match
     "Board Name +?\n\\([^ \t]+\\)[ \t]*\\([^ \t]+\\)"
     result)
    (ignore-errors
      `((fqbn . ,(setq arduino-fqbn (match-string 1 result)))
        (port . ,(setq arduino-port (match-string 2 result)))))))


(defun arduino--build-command (command sketch &optional port)
  "Add port and FQBN to command."
  (let (info)
    (unless (or arduino-fqbn arduino-port)
      (message "Collecting board information...")
      (setq info (arduino-update)))
    (format "%s %s %s --fqbn %s %s"
            arduino-exe
            command
            (if port
                (concat "-p "(or arduino-port (alist-get 'port info)))
              "")
            (or arduino-fqbn (alist-get 'fqbn info))
            sketch)))

(defun arduino-compile-sketch ()
  "Compile current sketch."
  (interactive)
  (message "Compiling...")
  (message
   (arduino--quote-string
    (shell-command-to-string
     (arduino--build-command "compile" (or arduino-sketch-dir
                                           default-directory))))))
(defun arduino-upload-sketch ()
  "Upload current sketch."
  (interactive)
  (message "Uploading...")
  (message
   (arduino--quote-string
    (shell-command-to-string
     (arduino--build-command "upload" (or arduino-sketch-dir
                                          default-directory)
                             t)))))

(defun arduino--quote-string (str)
  "Quote % sign in STR."
  (replace-regexp-in-string "%" "%%" str t t))

(defin-key arduino-mode-map (kbd "C-c C-c") #'arduino-compile-sketch)
(defin-key arduino-mode-map (kbd "C-c C-u") #'arduino-upload-sketch)
(defin-key arduino-mode-map (kbd "C-c u") #'arduino-update)

(provide 'arduino-mode)
;;; arduino-mode.el ends here
