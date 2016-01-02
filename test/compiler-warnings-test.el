(require 'ert)

(ert-deftest byte-compile-without-warnings ()
  "Byte-compile should not emit warnings"
  (byte-compile-file "arduino-mode.el")
  (switch-to-buffer "*Compile-Log*")
  (let ((lines (buffer-substring (point-min) (point-max))))
    (should (not (string-match "Warning:" lines)))))
