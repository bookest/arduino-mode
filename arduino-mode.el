;;; arduino-mode.el --- Major mode for editing Arduino code.

;; Copyright (C) 2008  Christopher Grim
;; Authors: Christopher Grim <christopher.grim@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: languages, arduino
;; Package-Requires: ((emacs "25") (cl-lib "0.5") (spinner "1.7.3"))
;; Package-Version: 1.2
;; homepage: https://github.com/stardiviner/arduino-mode

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
(require 'spinner)

(eval-when-compile
  (require 'cl-lib)
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cc-menus)
  (require 'term))

(eval-and-compile
  ;; fall back on c-mode
  (c-add-language 'arduino-mode 'c-mode))

(defgroup arduino-mode nil
  "Customize arduino-mode."
  :prefix "arduino-mode-"
  :group 'arduino)

(defcustom arduino-mode-home "~/Arduino"
  "The path of ARDUINO_HOME."
  :type 'directory
  :group 'arduino-mode)

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
  "List of extra types (aside from type keywords) to recognize in Arduino mode.
Each list item should be a regexp matching a single identifier."
  :group 'arduino
  :type 'list)

(defcustom arduino-executable "arduino"
  "The arduino program executable name."
  :group 'arduino
  :type 'string)

(defcustom arduino-spinner-type 'progress-bar
  "The spinner type for arduino processes.

Value is a symbol.  The possible values are the symbols in the
`spinner-types' variable."
  :type 'symbol
  :safe #'symbolp
  :group 'arduino)

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
    (define-key map (kbd "C-c C-c") 'arduino-upload)
    (define-key map (kbd "C-c C-v") 'arduino-verify)
    (define-key map (kbd "C-c C-m") 'arduino-serial-monitor)
    (define-key map (kbd "C-c C-x") 'arduino-open-with-arduino)
    map)
  "Keymap used in arduino-mode buffers.")

(easy-menu-define arduino-menu arduino-mode-map "Arduino Mode Commands"
  (cons "Arduino" (c-lang-const c-mode-menu arduino)))

(easy-menu-add-item arduino-menu
			              (list "Micro-controller") ["Upload" arduino-upload t])
(easy-menu-add-item arduino-menu
		                nil ["----" nil nil])
(easy-menu-add-item arduino-menu
		                nil ["Upload" arduino-upload t])
(easy-menu-add-item arduino-menu
		                nil ["Verify" arduino-verify t])
(easy-menu-add-item arduino-menu
		                nil ["Open with Arduino" arduino-open-with-arduino t])
(easy-menu-add-item arduino-menu
		                nil ["Serial monitor" arduino-serial-monitor t])

(defvar arduino-upload-process-buf nil)

(defun arduino-upload ()
  "Build and upload the sketch to an Arduino board."
  (interactive)
  (setq arduino-upload-process-buf (buffer-name))
  (let* ((proc-name "arduino-upload")
         (proc-buffer "*arduino-upload*")
         (proc (make-process
                :command (list arduino-executable "--upload" (buffer-file-name))
                :name proc-name
                :buffer proc-buffer
                :sentinel (lambda (proc event)
                            (if (string= event "finished\n")
                                (progn
                                  (with-current-buffer arduino-upload-process-buf
                                    (setq mode-line-process nil))
                                  (message "Arduino upload succeed."))
                              (with-current-buffer arduino-upload-process-buf
                                (display-buffer "*arduino-upload*")))
                            (setq-local mode-line-process nil)
                            (with-current-buffer arduino-upload-process-buf
                              (when spinner-current (spinner-stop)))))))
    (spinner-start arduino-spinner-type)
    (setq mode-line-process proc-name)))

(defvar arduino-verify-process-buf nil)

(defun arduino-verify ()
  "Verify the sketch by building it."
  (interactive)
  (setq arduino-verify-process-buf (buffer-name))
  (let* ((proc-name "arduino-verify")
         (proc-buffer "*arduino-verify*")
         (proc (make-process
                :command (list arduino-executable "--verify" (buffer-file-name))
                :name proc-name
                :buffer proc-buffer
                :sentinel (lambda (proc event)
                            (if (string= event "finished\n")
                                (progn
                                  (with-current-buffer arduino-verify-process-buf
                                    (setq mode-line-process nil))
                                  (message "Arduino verify build succeed."))
                              (display-buffer "*arduino-verify*"))
                            (setq-local mode-line-process nil)
                            (with-current-buffer arduino-verify-process-buf
                              (when spinner-current (spinner-stop)))))))
    (spinner-start arduino-spinner-type)
    (setq mode-line-process proc-name)))

(defvar arduino-open-process-buf nil)

(defun arduino-open-with-arduino ()
  "Open the sketch with the Arduino IDE."
  (interactive)
  (setq arduino-open-process-buf (buffer-name))
  (let* ((proc-name "arduino-open")
         (proc-buffer "*arduino-open*")
         (proc (make-process
                :command (list arduino-executable (buffer-file-name))
                :name proc-name
                :buffer proc-buffer
                :sentinel (lambda (proc event)
                            (if (string= event "finished\n")
                                (progn
                                  (with-current-buffer arduino-open-process-buf
                                    (setq mode-line-process nil))
                                  (message "Opened with Arduino succeed.")))
                            (setq-local mode-line-process nil)
                            (with-current-buffer arduino-open-process-buf
                              (when spinner-current (spinner-stop)))
                            ))))
    (spinner-start arduino-spinner-type)
    (setq mode-line-process proc-name)))

;;; NOTE: Because command-line arduino does not support search and list out
;;; boards and libraries. So I will not write a sentinel for installing process.
(defun arduino-install-boards (board)
  "Install `BOARD' support for Arduino."
  (interactive (list (completing-read "Arduino install board: "
                                      '()
                                      nil nil
                                      "arduino:sam")))
  (start-process
   "arduino-install-boards" "*arduino-install-boards*"
   arduino-executable "--install-boards" board)
  )

(defun arduino-install-library (library)
  "Install `LIBRARY' support for Arduino."
  (interactive (list (completing-read "Arduino install library: "
                                      '()
                                      nil nil
                                      "Bridge:1.0.0")))
  (start-process
   "arduino-install-library" "*arduino-install-library*"
   arduino-executable "--install-library" library))

(require 'term)
(defun arduino-serial-monitor (port speed)
  "Monitor the `SPEED' on serial connection on `PORT' to the Arduino."
  (interactive (list (serial-read-name) nil))
  (if (get-buffer-process port)
	    (switch-to-buffer port)
    (serial-term port (or speed (serial-read-speed)))))

;;;###autoload
(defun arduino-sketch-new (sketch)
  "A command to create new `SKETCH' in ARDUINO_HOME (~/Arduino)."
  (interactive (list (read-from-minibuffer "Arduino new sketch file: ")))
  (let ((default-directory (expand-file-name arduino-mode-home)))
    (find-file sketch)))

;;; generate .clang_complete file for `irony-mode' and `company-irony-c-headers'.
(defun arduino-generate-include-path-file ()
  "Generate .clang_complete file for `irony-mode' and `company-irony-c-headers'."
  (interactive)
  (let ((default-directory (expand-file-name arduino-mode-home))
        (filename ".clang_complete"))
    (if (file-exists-p filename)
        (if (y-or-n-p ".clang_complete file already exist, do you want to edit it? ")
            (find-file filename))
      (with-temp-file filename
        (insert "-I/home/stardiviner/Arduino/libraries/")))))


;;;###autoload
(define-derived-mode arduino-mode c-mode "arduino"
  "Major mode for editing Arduino code."
  ;; For `cc-mode' initialize.
  (c-initialize-cc-mode t)
  ;; `c-init-language-vars' is a macro that is expanded at compile time to a
  ;; large `setq' with all the language variables and their customized values
  ;; for our language.
  (c-init-language-vars arduino-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode buffer,
  ;; including setup of the mode menu, font-lock, etc. There's also a lower
  ;; level routine `c-basic-common-init' that only makes the necessary
  ;; initialization to get the syntactic analysis and similar things working.
  (c-common-init 'arduino-mode)
  
  (easy-menu-add arduino-menu)
  (set (make-local-variable 'c-basic-offset) 2)
  (set (make-local-variable 'tab-width) 2)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pde\\'" . arduino-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))


(provide 'arduino-mode)
;;; arduino-mode.el ends here
