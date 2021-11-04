;;; -*- Mode: Emacs-Lisp; -*-
;;;
;;; Copyright (c) 2012, Max Mikhanosha. All rights reserved.
;;; Copyright (c) 2021, Hugh Daschbach
;;;
;;; This file is licensed to You under the Apache License, Version 2.0
;;; (the "License"); you may not use this file except in compliance
;;; with the License.  You may obtain a copy of the License at
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(require 'cl-lib)
(require 'log4cl)
(require 'sly)

(cl-defmethod log4slime-eval ((backend (eql :log4sly)) form)
  (when (log4slime-check-connection)
    ;; I swear it something in slime-eval screws with point sometimes
    (save-excursion
      (cl-destructuring-bind (fn . args) form
        (let ((sly-current-thread t)
              (fn-sym (intern (concat "log4cl.log4sly:" (symbol-name fn)))))
          (sly-eval `(cl:ignore-errors (,fn-sym ,@args))))))))

(cl-defmethod log4slime-load-lisp-system ((backend (eql :log4sly)))
  (sly-eval `(cl:multiple-value-bind
                (ok err)
                (cl:ignore-errors
                 (cl:setf (cl:get :log4sly :no-emacs-startup-message) t)
                 (asdf:load-system :log4cl.log4sly))
                (cl:if ok :ok (cl:princ-to-string err)))))

(cl-defmethod log4slime-connected-p ((backend (eql :log4sly)))
  (sly-connected-p))

(cl-defmethod log4slime-current-package ((backend (eql :log4sly)))
  (sly-current-package))

(cl-defmethod log4slime-symbol-at-point ((backend (eql :log4sly)))
  (sly-symbol-at-point))

(cl-defmethod log4slime-sexp-at-point ((backend (eql :log4sly)))
  (sly-sexp-at-point))

(cl-defmethod log4slime-analyze-xrefs ((backend (eql :log4sly)) xrefs)
  (sly-analyze-xrefs xrefs))

(cl-defmethod log4slime-push-definition-stack ((backend (eql :log4sly)))
  (sly-push-definition-stack))

(cl-defmethod log4slime-xref.location ((backend (eql :log4sly)) xref)
  (sly-xref.location xref))

(cl-defmethod log4slime-current-connection ((backend (eql :log4sly)))
  (sly-current-connection))

(cl-defmethod log4slime-buffer-file-name ((backend (eql :log4sly)))
  (buffer-file-name))

(cl-defmethod log4slime-show-xrefs ((backend (eql :log4sly))
                                    xrefs types symbol package)
  (sly-edit-definition symbol))

(cl-defmethod log4slime-pop-to-location ((backend (eql :log4sly))
                                         location &optional where)
  (sly--display-source-location location nil where))

;; Log message formatting hook

(defun log4sly-mrepl-highlight-string (string)
  "Highlight STRING as a log4cl message when log4slime-mode enabled.
This is used to format ‘sly’ output.  Output from ’sly’ is
presented as a string to be transformed, rather than a buffer
region."
  (if log4sly-mode
      (with-temp-buffer
        (insert string)
        (let ((log4slime-category-package-properties
               (append
                (list 'font-lock-face 'log4slime-package-face)
                log4slime-category-package-properties))
              (log4slime-category-file-properties
               (append
                (list 'font-lock-face 'log4slime-file-face)
                log4slime-category-file-properties))
              (log4slime-category-function-properties
               (append
                (list 'font-lock-face 'log4slime-function-face)
                log4slime-category-function-properties))
              (log4slime-category-level-properties
               (append
                (list 'font-lock-face 'log4slime-level-face)
                log4slime-category-level-properties)))
          (log4slime-highlight-log-message (point-min) (point-max)))
        (buffer-string))
    string))

(eval-after-load 'sly-mrepl
  '(add-hook
    'sly-mrepl-output-filter-functions
    #'log4sly-mrepl-highlight-string))

;; log4sly mode definition

(define-minor-mode log4sly-mode
  "\\<log4slime-mode-map>\
Support mode integrating log4sly logging system with SLY

\\[log4slime-level-selection]		- Set log level fast via keyboard

Only \"standard\" log levels show up in the menu and keyboard bindings.

There are also 8 extra debug levels, DEBU1..DEBU4 are more specific then DEBUG
but less specific then TRACE, and DEBU5..DEBU9 come after TRACE.

To make them show up in the menu, but you can customize the
variable `log4slime-menu-levels'.
"
  :keymap log4slime-mode-map
  (when log4sly-mode
    (setq log4slime-backend :log4sly)
    (log4slime-check-connection t)))

(defun turn-on-log4sly-mode ()
  "Turn on `log4sly-mode' in the current buffer if appropriate."
  (interactive)
  (if (member major-mode '(lisp-mode sly-mrepl-mode))
      (log4sly-mode 1)
    (when (called-interactively-p 'interactive)
      (message "This buffer does not support log4sly mode"))))

(define-globalized-minor-mode global-log4sly-mode
  log4sly-mode
  turn-on-log4sly-mode
  :group 'log4slime)

(provide 'log4sly)

;;; log4sly.el ends here
