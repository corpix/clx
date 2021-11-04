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
(require 'slime)

(cl-defmethod log4slime-eval ((backend (eql :log4slime)) form)
  (when (log4slime-check-connection)
    ;; I swear it something in slime-eval screws with point sometimes
    (save-excursion
      (cl-destructuring-bind (fn . args) form
        (let ((slime-current-thread t)
              (fn-sym (intern (concat "log4cl.log4slime:" (symbol-name fn)))))
          (slime-eval `(cl:ignore-errors (,fn-sym ,@args))))))))

(cl-defmethod log4slime-load-lisp-system ((backend (eql :log4slime)))
  (slime-eval `(cl:multiple-value-bind
                (ok err)
                (cl:ignore-errors
                 (cl:setf (cl:get :log4slime :no-emacs-startup-message) t)
                 (asdf:load-system :log4cl.log4slime))
                (cl:if ok :ok (cl:princ-to-string err)))))

(cl-defmethod log4slime-connected-p ((backend (eql :log4slime)))
  (slime-connected-p))

(cl-defmethod log4slime-current-package ((backend (eql :log4slime)))
  (slime-current-package))

(cl-defmethod log4slime-symbol-at-point ((backend (eql :log4slime)))
  (slime-symbol-at-point))

(cl-defmethod log4slime-sexp-at-point ((backend (eql :log4slime)))
  (slime-sexp-at-point))

(cl-defmethod log4slime-analyze-xrefs ((backend (eql :log4slime)) xrefs)
  (slime-analyze-xrefs xrefs))

(cl-defmethod log4slime-push-definition-stack ((backend (eql :log4slime)))
  (slime-push-definition-stack))

(cl-defmethod log4slime-xref.location ((backend (eql :log4slime)) xref)
  (slime-xref.location xref))

(cl-defmethod log4slime-current-connection ((backend (eql :log4slime)))
  (slime-current-connection))

(cl-defmethod log4slime-show-xrefs ((backend (eql :log4slime))
                                    xrefs types symbol package)
  (slime-show-xrefs xrefs types symbol package))

(cl-defmethod log4slime-pop-to-location ((backend (eql :log4slime))
                                         location &optional where)
  (slime-pop-to-location location where))

;; Log message formatting hook

(eval-after-load 'slime-repl
  '(defadvice slime-repl-emit (around highlight-logging-category activate compile)
     (with-current-buffer (slime-output-buffer)
       (if log4slime-mode
           (let ((start (marker-position slime-output-end)))
             (setq ad-return-value ad-do-it)
             (log4slime-highlight-log-message start (marker-position slime-output-end)))
         (setq ad-return-value ad-do-it)))))

;; log4slime mode definition

(define-minor-mode log4slime-mode
  "\\<log4slime-mode-map>\
Support mode integrating log4slime logging system with SLIME

\\[log4slime-level-selection]		- Set log level fast via keyboard

Only \"standard\" log levels show up in the menu and keyboard bindings.

There are also 8 extra debug levels, DEBU1..DEBU4 are more specific then DEBUG
but less specific then TRACE, and DEBU5..DEBU9 come after TRACE.

To make them show up in the menu, but you can customize the
variable `log4slime-menu-levels'.
"
  :keymap log4slime-mode-map
  (when log4slime-mode
    (setq log4slime-backend :log4slime)
    (log4slime-check-connection t)))

(defun turn-on-log4slime-mode ()
  "Turn on `log4slime-mode' in the current buffer if appropriate."
  (interactive)
  (if (member major-mode '(lisp-mode slime-repl-mode))
      (log4slime-mode 1)
    (when (called-interactively-p 'interactive)
      (message "This buffer does not support log4slime mode"))))

(define-globalized-minor-mode global-log4slime-mode
  log4slime-mode
  turn-on-log4slime-mode
  :group 'log4slime)

(provide 'log4slime)

;;; log4slime.el ends here
