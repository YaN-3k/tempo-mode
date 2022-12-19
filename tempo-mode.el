;;; tempo-mode.el --- Enchantment for tempo.         -*- lexical-binding: t; -*-

;; Author: Jan Wiśnieski <wisniewskijas@outlook.com>
;; Keywords: convenience
;; Created: December 19, 2022
;; Time-stamp: <2022-12-19 17:38:31 jan>

;; Everyone is permitted to do whatever they like with this software
;; without limitation.  This software comes without any warranty
;; whatsoever, but with two pieces of advice:
;; - Be kind to yourself.
;; - Make good choices.

;;; Commentary:

;; Provides `tempo-mode' to automate taglist selection based on major modes.
;; To create new tempos declare them with `deftempotable'.

;;; Example:

;; (use-package tempo-mode
;;   :demand
;;   :hook (prog-mode text-mode)
;;   :bind (:map tempo-mode-map
;;            ("C-c C-n" . tempo-forward-mark)
;;            ("C-c C-n" . tempo-forward-mark)
;;            ("C-c ,"   . tempo-maybe-complete))
;;   :config
;;   (deftempotable (c-mode)
;;       (for    "for (" p "; " p "; " p ") {" n> r> n> "}" >)
;;       (if     "if (" p ") {" n> r> n> "}" >)
;;       (else   "else {" n> r> n> "}" >)
;;       (do     "do {" n> r> n> "} while (" p ");" >)
;;       (while  "while (" p ") {" n> r> n> "}" >)))

;;; Code:

(require 'tempo)

(defvar tempo-tmp nil "Temporary variable for user use in tempos.")
(defvar tempo-tags* nil "Used by `deftempo' instead of global `tempo-tags'.")
(defvar tempo-mode-map (make-sparse-keymap) "Keymap for command `tempo-mode'.")

(defun tempo--trim-mode (mode)
  "Trim mode from MODE."
  (string-trim-right (symbol-name mode) (rx "-mode" eos)))

(defun tempo--taglist (mode)
  "Return tempo taglist derived from MODE."
  (intern (concat "tempo-" (tempo--trim-mode mode) "-tags")))

(defun tempo--function-name (name modes)
  "Create unique function NAME derived from MODES."
  (concat (mapconcat
           (lambda (mode) (concat (tempo--trim-mode mode) "-"))
           (sort modes #'string<) "")
          name))

(defun tempo--derived-modes (mode)
  "List all derived modes of MODE + MODE itself."
  (let ((parents (list mode)))
    (while mode
      (setq parents (cons mode parents)
            mode (get mode 'derived-mode-parent)))
    (reverse parents)))

(defun deftempo-1 (name modes body)
  "Define tempo using BODY with unique NAME derived from MODES."
  (let* ((tag (symbol-name name))
         (fun-name (tempo--function-name tag modes))
         (tempo-name (intern (concat "tempo-template-" fun-name))))
    (mapc                               ; add tempo to each mode taglist
     (lambda (mode)
       (let ((taglist (tempo--taglist mode)))
         (unless (boundp taglist) (set taglist nil))
         (tempo-add-tag tag tempo-name taglist)))
     modes)
    (tempo-define-template fun-name body tag nil 'tempo-tags*)))

(defmacro deftempo (name modes &rest body)
  "Define tempo using BODY with unique NAME derived from MODES."
  `(deftempo-1 ',name ',modes ',body))

(defmacro deftempotable (modes &rest tempos)
  "Define TEMPOS for MODES."
  (let ((expansions))
    (dolist (tempo tempos)
      (push `(deftempo ,(car tempo) ,modes ,@(cdr tempo)) expansions))
    `(progn ,@expansions)))

(defun tempo-maybe-complete ()
  "Try to complete tempo tag or if called on region prompt for it."
  (interactive "*")
  (if (use-region-p)
      (if-let (taglist (tempo-build-collection))
          (let* ((tags (mapcar #'car taglist))
                 (tag (completing-read "Tempo: " tags nil t)))
            (tempo-insert-template (cdr (assoc tag taglist)) t))
        (message "No tempos defined for buffer."))
    (unless (tempo-expand-if-complete)
      (let ((tempo-mode nil))
        (execute-kbd-macro (this-command-keys))))))

(define-minor-mode tempo-mode
  "Minor mode for switching tempo taglist depending on buffer major mode."
  :lighter " tempo"
  :keymap tempo-mode-map
  (dolist (taglist (mapcar #'tempo--taglist (tempo--derived-modes major-mode)))
    (when (boundp taglist)
      (tempo-use-tag-list taglist))))

(provide 'tempo-mode)

;;; tempo-mode.el ends here
