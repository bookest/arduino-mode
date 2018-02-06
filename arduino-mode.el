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
  (require 'cc-menus)
  (require 'term))

(eval-and-compile
  ;; fall back on c-mode
  (c-add-language 'arduino-mode 'c-mode))

(c-lang-defconst c-primitive-type-kwds
  arduino (append '(;; Data Types
                    "boolean" "byte"
                    "int" "long" "short" "double" "float"
                    "char" "string"
                    "unsigned char" "unsigned int" "unsigned long"
                    "void" "word"
                    ;; Variable Scope & Qualifiers
                    "const" "scope" "static" "volatile"
                    ;; Structure
                    ;; Sketch
                    "loop" "setup"
                    ;; Control Structure
                    "break" "continue" "do" "while" "else" "for" "goto" "if"
                    "return" "switch" "case"
                    ;; Utilities
                    "PROGMEM"
                    )
                  (c-lang-const c-primitive-type-kwds)))

(c-lang-defconst c-constant-kwds
  arduino (append
           '("HIGH" "LOW"
             "INPUT" "OUTPUT" "INPUT_PULLUP"
             "LED_BUILTIN"
             "true" "false")
           (c-lang-const c-constant-kwds)))

(c-lang-defconst c-simple-stmt-kwds
  arduino
  (append
   '(;; Operator Utilities
     "sizeof"
     ;; Functions
     "pinMode" "digitalWrite" "digitalRead"                ; Digital I/O
     "analogReference" "analogRead" "analogWrite"          ; Analog I/O
     "analogReadResolution" "analogWriteResolution" ; Zero, Due & MKR Family
     "tone" "noTone" "shiftIn" "shiftOut" "pulseIn" "pulseInLong" ; Advanced I/O
     "millis" "micros" "delay" "delayMicroseconds"                ; Time
     "min" "max" "abs" "constrain" "map" "pow" "sq" "sqrt"        ; Math
     "sin" "cos" "tan"                                            ; Trigonometry
     "randomSeed" "random"                              ; Random Numbers
     "bit" "bitRead" "bitWrite" "bitSet" "bitClear" "lowByte" "highByte" ; Bits and Bytes
     "attachInterrupt" "detachInterrupt" ; External Interrupts
     "interrupts" "noInterrupts"         ; Interrupts
     "serial" "stream"                   ; Serial Communication
     ;; Characters
     "isAlpha" "isAlphaNumeric"
     "isAscii" "isControl" "isDigit" "isGraph" "isHexadecimalDigit"
     "isLowerCase" "isUpperCase"
     "isPrintable" "isPunct" "isSpace" "isWhitespace"
     ;; USB Devices like Keyboard functions
     "print" "println"
     ;; Serial
     "begin" "end" "available" "read" "flush"  "peek"
     ;; Keyboard
     "write" "press" "release" "releaseAll"
     ;; Mouse
     "click" "move" "isPressed"
     )
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
(define-key arduino-mode-map "\C-cg"  'arduino-upload)
(unless (string-match "XEmacs" emacs-version)
  (define-key arduino-mode-map "\C-cm"  'arduino-serial-monitor))

(easy-menu-define arduino-menu arduino-mode-map "Arduino Mode Commands"
  (cons "Arduino" (c-lang-const c-mode-menu arduino)))

; How does one add this directly to the Arduino menu in XEmacs?
(if (string-match "XEmacs" emacs-version)
    (easy-menu-add-item arduino-menu
			(list "Micro-controller") ["Upload" arduino-upload t])
  (easy-menu-add-item arduino-menu
		      nil ["----" nil nil])
  (easy-menu-add-item arduino-menu
		      nil ["Upload" arduino-upload t])
  (easy-menu-add-item arduino-menu
		      nil ["Serial monitor" arduino-serial-monitor t]))

(defcustom arduino-makefile-name "Makefile"
  "Name of Makefile used to compile and upload Arduino sketches."
  :type 'string
  :group 'arduino)

(defun arduino-upload ()
  "Upload a sketch to an Arduino board.

You will need a suitable Makefile.  See URL
`http://mjo.tc/atelier/2009/02/arduino-cli.html'."
  (interactive)
  (if (file-exists-p arduino-makefile-name)
      (progn
	(make-local-variable 'compile-command)
	(compile (concat "make -f " arduino-makefile-name " -k upload")))
    (if (y-or-n-p (concat "No Makefile `" arduino-makefile-name
			  "' exists.  Create it? "))
	(let ((arduino-project-name
	       (file-name-nondirectory
		(file-name-sans-extension (buffer-file-name)))))
	  (find-file-other-window arduino-makefile-name)
	  (insert "# Customise the following values as required:

TARGET       = " arduino-project-name "
ARDUINO_LIBS =

MCU          = atmega328p
F_CPU        = 16000000
ARDUINO_PORT = /dev/ttyUSB*
AVRDUDE_ARD_BAUDRATE = 57600
ARDUINO_DIR  = /usr/share/arduino

# If you do not already have Arduino.mk, find it at
# http://mjo.tc/atelier/2009/02/arduino-cli.html
include /usr/share/arduino/Arduino.mk
")
	  (message "Edit the Makefile as required and re-run arduino-upload."))
      (message (concat "No Makefile `" arduino-makefile-name "' exists.  Uploading cancelled.")))))

(unless (string-match "XEmacs" emacs-version)
  (defun arduino-serial-monitor (port speed)
    "Monitor the serial connection to the Arduino."
    (interactive (list (serial-read-name) nil))

    (if (get-buffer-process port)
	(switch-to-buffer port)
      (serial-term port (or speed (serial-read-speed))))))

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
  (use-local-map arduino-mode-map)
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

(provide 'arduino-mode)
;;; arduino-mode.el ends here
