;;; ob-arduino.el --- Org-mode Babel support for Arduino.

;;; Commentary:



;;; Code:

(require 'org)
(require 'ob)
(require 'arduino-mode)

(defgroup ob-arduino nil
  "org-mode blocks for Arduino."
  :group 'org)

(defcustom ob-arduino:program "arduino"
  "Default Arduino program name."
  :group 'ob-arduino
  :type 'string)

(defcustom ob-arduino:port "/dev/ttyACM0"
  "Default Arduino port."
  :group 'ob-arduino
  :type 'string)

(defcustom ob-arduino:board "arduino:avr:uno"
  "Default Arduino board."
  :group 'ob-arduino
  :type 'string)


(defvar org-babel-default-header-args:sclang nil)

;;;###autoload
(defun org-babel-execute:arduino (body params)
  "org-babel arduino hook."
  (let* ((port (cdr (assoc :port params)))
         (board (cdr (assoc :board params)))
         (cmd (mapconcat 'identity (list
                                    ob-arduino:program "--upload"
                                    (if port (concat "--port " port))
                                    (if board (concat "--board " board))
                                    ) " "))
         (code (org-babel-expand-body:generic body params))
         (src-file (org-babel-temp-file "ob-arduino-" ".ino")))
    ;; delete all `ob-arduino' temp files, otherwise arduino will compile all
    ;; ob-arduino temp files, and report error.
    (mapc
     (lambda (f)
       (unless (file-directory-p f)
         (delete-file (expand-file-name f org-babel-temporary-directory))))
     (directory-files
      (file-name-directory (org-babel-temp-file "ob-arduino-" ".ino"))
      nil ".ino"))
    ;; specify file for arduino command.
    (with-temp-file src-file
      (insert code))
    (org-babel-eval
     (concat ob-arduino:program
             " " "--upload"
             " " (if port (concat "--port " port))
             " " (if board (concat "--board " board))
             " " src-file)
     "" ; pass empty string "" as `BODY' to `org-babel--shell-command-on-region'
     ;; to fix command `arduino' don't accept string issue.
     )
    ))


;;;###autoload
(eval-after-load 'org
  '(add-to-list 'org-src-lang-modes '("arduino" . arduino)))




(provide 'ob-arduino)

;;; ob-arduino.el ends here
