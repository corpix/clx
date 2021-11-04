;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
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

(defpackage #:log4cl.log4slime
  (:use #:cl)
  (:import-from #:log4cl.elisp
                #:*quicklisp-directory*
                #:*system-directory*)
  (:export #:emacs-helper
           #:get-buffer-log-menu
           #:install
           #:*quicklisp-directory*
           #:*system-directory*))

(in-package #:log4cl.log4slime)
(log:package-options :shortest-nickname nil)

;;
;; Backend dispatch methods
;;

(defmethod log4cl.elisp:find-definitions ((backend (eql :log4slime)) name)
  (swank::find-definitions name))

(defmethod log4cl.elisp:xref>elisp ((backend (eql :log4slime)) xref)
  (swank::xref>elisp xref))

(defmethod log4cl.elisp:parse-package ((backend (eql :log4slime)) package)
  (swank::parse-package package))

(defmethod log4cl.elisp:format-menu-plist ((backend (eql :log4slime))
                                           pkg find-package-categories
                                           find-logger frob)
  (swank::with-buffer-syntax (pkg)
    (log4cl:with-package-naming-configuration (*package*)
      (funcall find-package-categories)
      (multiple-value-bind (logger display-name) (funcall find-logger)
        (log:expr logger display-name)
        (funcall frob logger display-name)))))

;;
;; Elisp interface functions
;;

(defun emacs-helper (info)
  "Wrapper around ‘log4cl.elisp:emacs-helper’.
This provides the dynamic dispatch parameter to provide log4cl.elisp
with access to Swank support functions."
  (log4cl.elisp:emacs-helper :log4slime info))

(defun get-buffer-log-menu (&rest args)
  "Wrapper around ‘log4cl.elisp:get-buffer-log-menu’.
This provides the dynamic dispatch parameter to provide log4cl.elisp
with access to Swank support functions."
  (log4cl.elisp:get-buffer-log-menu :log4slime args))

;;
;; Support for snippets compiled via C-c C-c correctly identifying the source file
;;
(defvar *old-compile-string-for-emacs*
  (fdefinition 'swank::compile-string-for-emacs))

;; Patch the COMPILE-STRING-FOR-EMACS to bind *LOGGER-TRUENAME* to the file
;; name that C-c C-c snippet is from
(setf (fdefinition 'swank::compile-string-for-emacs)
      (lambda (string buffer position filename policy)
        (let ((log4cl:*logger-truename*
                (when filename (ignore-errors (parse-namestring filename)))))
          (funcall *old-compile-string-for-emacs*
                   string buffer position filename policy))))

;; In case SWANK was patched with the "thread stopper" patch that defines
;; protocol for starting/stopping threads around calls to fork(), register
;; a callback for the watcher thread
(let ((rss-foo (find-symbol (symbol-name '#:register-thread-stopper) (find-package :swank))))
  (and rss-foo (funcall rss-foo :log4cl #'log4cl::start/stop-watcher-hook)))

(defun install (&key force
                     (destination-directory *quicklisp-directory*)
                     just-message)
  "Generate log4slime-setup.el if it does not exist.
Sanity check it if it does exist."
  (log4cl.elisp:install "LOG4SLIME" :force force
                                    :destination-directory destination-directory
                                    :just-message just-message))

(unless (get :log4slime :no-emacs-startup-message)
  (install :just-message t))
